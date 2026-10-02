use rowan::TextRange;

use super::lexer::Token;
use syntax::SyntaxKind;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    FinishNode,
    AddToken,
    AddSyntheticToken {
        kind: SyntaxKind,
        text: &'static str,
        consume: bool,
    },
    Placeholder,
}

#[derive(Debug)]
pub struct Marker {
    pos: usize,
    completed: bool,
}

impl Marker {
    pub fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.completed = true;
        match &mut p.events[self.pos] {
            Event::StartNode { kind: solt, .. } => *solt = kind,
            _ => unreachable!(),
        }
        p.events.push(Event::FinishNode);
        CompletedMarker { pos: self.pos }
    }

    // don't make node
    pub fn abandon(mut self, p: &mut Parser) {
        self.completed = true;
        if self.pos == p.events.len() - 1 {
            p.events.pop();
        } else {
            p.events[self.pos] = Event::Placeholder;
        }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        assert!(
            self.completed,
            "Marker must be either completed or abandoned"
        );
    }
}

/// It can be traced back using `precede()`.
#[derive(Debug, Clone, Copy)]
pub struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    fn kind(self, p: &Parser) -> SyntaxKind {
        match p.events[self.pos] {
            Event::StartNode { kind, .. } => kind,
            _ => unreachable!(),
        }
    }

    /// Insert a new parent node before this node in the hierarchy.
    ///
    /// Used for Pratt parsing: First, the value `1` is parsed. Later,
    /// it is determined that the expression actually represents `1 + 2`.
    /// In other words, the `1` is encapsulated within a `BinaryExpr` object.
    pub fn precede(self, p: &mut Parser) -> Marker {
        let new_marker = p.start();
        match &mut p.events[self.pos] {
            Event::StartNode { forward_parent, .. } => {
                *forward_parent = Some(new_marker.pos - self.pos);
            }
            _ => unreachable!(),
        }
        new_marker
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReparseEntry {
    Statement,
    Expression,
    Type,
    Block,
    ParamList,
    StructFieldList,
    ArgList,
    UseTree,
    UseTreeList,
    Path,
    Pattern,
    MatchArm,
    EnumVariant,
    FieldPattern,
    TypeList,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ExprRestrictions {
    allow_struct_expr: bool,
    stop_infix_after_block: bool,
}

impl ExprRestrictions {
    const NONE: Self = Self {
        allow_struct_expr: true,
        stop_infix_after_block: false,
    };

    const NO_STRUCT_EXPR: Self = Self {
        allow_struct_expr: false,
        stop_infix_after_block: false,
    };

    const STATEMENT: Self = Self {
        allow_struct_expr: true,
        stop_infix_after_block: true,
    };
}

/// Saved parser state for speculative parsing (bracket-lambda vs array).
struct SpeculationPoint {
    pos: usize,
    current_non_trivia_pos: usize,
    current_kind: SyntaxKind,
    pending_split_greater: usize,
    events_len: usize,
    errors_len: usize,
}

/// Maximum nesting depth for expressions, types, patterns, and blocks.
///
/// Every recursive-descent level costs a large stack frame chain across the
/// whole pipeline (parser, HIR lowering, move checking, MIR lowering,
/// codegen — tens of KB per level in debug builds), so deep inputs must be
/// rejected at parse time instead of overflowing the stack — a hard crash
/// the diagnostics layer cannot catch. 48 levels is far above anything
/// human-written code needs.
pub(crate) const MAX_NESTING_DEPTH: usize = 48;

pub struct Parser<'s> {
    source: &'s str,
    tokens: Vec<Token>,
    pos: usize, // include trivia
    pub(crate) events: Vec<Event>,
    pub errors: Vec<ParseError>,
    // cache
    current_kind: SyntaxKind,
    current_non_trivia_pos: usize,
    pending_split_greater: usize,
    nesting_depth: usize,
    nesting_exhausted: bool,
}

thread_local! {
    /// Recursion depth of the current thread's parse — parallel parses
    /// (language server, test threads) must not see each other's depth.
    static RIDDLE_CALL_DEPTH: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}
struct RiddleDepthGuard;
impl RiddleDepthGuard {
    fn enter() -> Self {
        RIDDLE_CALL_DEPTH.with(|depth| depth.set(depth.get() + 1));
        RiddleDepthGuard
    }
}
impl Drop for RiddleDepthGuard {
    fn drop(&mut self) {
        RIDDLE_CALL_DEPTH.with(|depth| depth.set(depth.get().saturating_sub(1)));
    }
}

#[must_use]
fn riddle_call_depth() -> usize {
    RIDDLE_CALL_DEPTH.with(std::cell::Cell::get)
}

impl<'s> Parser<'s> {
    /// Enters one nesting level; `false` means the limit is exhausted and an
    /// error was already reported. Callers return early on `false`.
    fn enter_nesting(&mut self) -> bool {
        self.nesting_depth += 1;
        // Postfix-call chains (`f(a)(b)`, `((((1))))`) recurse without ever
        // nesting a syntactic block, so the call depth is the real bound.
        if self.nesting_depth > MAX_NESTING_DEPTH || riddle_call_depth() > MAX_NESTING_DEPTH * 12 {
            self.bail_out_nesting(false);
            self.nesting_depth -= 1;
            return false;
        }
        true
    }

    fn exit_nesting(&mut self) {
        self.nesting_depth = self.nesting_depth.saturating_sub(1);
    }

    /// Reports the nesting diagnostic once per parse and, when `consume` is
    /// set, skips the offending token so the caller's loop cannot retry the
    /// same position. Recovery keeps re-entering the exhausted depth, so an
    /// undeduplicated report emits one identical diagnostic per retry.
    fn bail_out_nesting(&mut self, consume: bool) {
        if !self.nesting_exhausted {
            self.nesting_exhausted = true;
            let span = self.current_span();
            self.errors.push(ParseError {
                message: "expression nesting is too deep".into(),
                span,
            });
        }
        if consume && !self.at(SyntaxKind::Eof) {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::ErrorNode);
        }
    }

    #[must_use]
    pub fn new(source: &'s str, tokens: Vec<Token>) -> Self {
        let mut p = Self {
            source,
            tokens,
            pos: 0,
            events: vec![],
            errors: vec![],
            current_kind: SyntaxKind::Eof,
            current_non_trivia_pos: 0,
            pending_split_greater: 0,
            nesting_depth: 0,
            nesting_exhausted: false,
        };
        p.recompute_current();
        p
    }

    /// Recalculating the current non-trivia token (only called after `pos` changes)
    fn recompute_current(&mut self) {
        if self.pending_split_greater > 0 {
            self.current_kind = SyntaxKind::Greater;
            return;
        }
        let mut i = self.pos;
        while i < self.tokens.len() {
            if !self.tokens[i].kind.is_trivia() {
                self.current_kind = self.tokens[i].kind;
                self.current_non_trivia_pos = i;
                return;
            }
            i += 1;
        }
        self.current_kind = SyntaxKind::Eof;
        self.current_non_trivia_pos = self.tokens.len();
    }

    /// Now non-trivia token.
    const fn current(&self) -> SyntaxKind {
        self.current_kind
    }

    /// Look ahead to the nth non-trivia token.
    #[allow(unused)]
    fn nth(&self, n: usize) -> SyntaxKind {
        if n == 0 {
            return self.current_kind;
        }

        let mut remaining = n;
        let mut i = self.pos;
        while i < self.tokens.len() {
            if !self.tokens[i].kind.is_trivia() {
                if remaining == 0 {
                    return self.tokens[i].kind;
                }
                remaining -= 1;
            }
            i += 1;
        }
        SyntaxKind::Eof
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    /// make a Marker
    fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::StartNode {
            kind: SyntaxKind::Tombstone,
            forward_parent: None,
        });
        Marker {
            pos,
            completed: false,
        }
    }

    fn eat_trivia(&mut self) {
        if self.pending_split_greater > 0 {
            return;
        }
        while self.pos < self.tokens.len() && self.tokens[self.pos].kind.is_trivia() {
            self.events.push(Event::AddToken);
            self.pos += 1;
        }
    }

    fn bump(&mut self) {
        if self.pending_split_greater > 0 {
            self.events.push(Event::AddSyntheticToken {
                kind: SyntaxKind::Greater,
                text: ">",
                consume: false,
            });
            self.pending_split_greater -= 1;
            self.recompute_current();
            return;
        }
        self.eat_trivia();
        if self.pos < self.tokens.len() {
            self.events.push(Event::AddToken);
            self.pos += 1;
        }
        self.recompute_current();
    }

    fn split_shr_as_greater(&mut self) {
        self.eat_trivia();
        if self.at(SyntaxKind::Shr) {
            self.events.push(Event::AddSyntheticToken {
                kind: SyntaxKind::Greater,
                text: ">",
                consume: true,
            });
            self.pos += 1;
            self.pending_split_greater += 1;
        }
        self.recompute_current();
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            return true;
        }

        self.error_no_bump(format!("expected {:?}, found {:?}", kind, self.current()));

        // Delimiters and statement-start keywords are sync points: leave
        // them in place so the surrounding construct (statement, list) can
        // resynchronize there instead of cascading.
        if !matches!(
            self.current(),
            SyntaxKind::RParen
                | SyntaxKind::RBrace
                | SyntaxKind::Semi
                | SyntaxKind::Comma
                | SyntaxKind::Eof
        ) && !self.token_starts_statement()
        {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::ErrorNode);
        }

        false
    }

    fn current_span(&self) -> TextRange {
        if self.current_non_trivia_pos < self.tokens.len() {
            let span = &self.tokens[self.current_non_trivia_pos].span;
            TextRange::new(
                rowan::TextSize::from(
                    u32::try_from(span.start).expect("token offset should fit in u32"),
                ),
                rowan::TextSize::from(
                    u32::try_from(span.end).expect("token offset should fit in u32"),
                ),
            )
        } else {
            TextRange::empty(rowan::TextSize::from(
                u32::try_from(self.source.len()).expect("source length should fit in u32"),
            ))
        }
    }

    fn error(&mut self, msg: String) {
        let span = self.current_span();
        self.errors.push(ParseError { message: msg, span });
        if !self.at(SyntaxKind::Eof) {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::ErrorNode);
        }
    }

    fn error_no_bump(&mut self, msg: String) {
        let span = self.current_span();
        self.errors.push(ParseError { message: msg, span });
    }

    /// Skip to a statement boundary after a malformed statement: consume up
    /// to and including the next `;`, or stop before `}`/EOF/any token that
    /// begins a new statement, so one mistake yields one diagnostic.
    fn sync_to_statement_boundary(&mut self) {
        while !self.at(SyntaxKind::Eof) {
            match self.current() {
                SyntaxKind::Semi => {
                    self.bump();
                    return;
                }
                SyntaxKind::RBrace => return,
                _ if self.token_starts_statement() => return,
                _ => {
                    self.bump();
                }
            }
        }
    }

    /// Sync-point view of "this token begins a new statement": the item and
    /// statement keywords plus the block-like expression introducers.
    const fn token_starts_statement(&self) -> bool {
        matches!(
            self.current(),
            SyntaxKind::Hash
                | SyntaxKind::Let
                | SyntaxKind::Pub
                | SyntaxKind::Fun
                | SyntaxKind::Struct
                | SyntaxKind::Mod
                | SyntaxKind::Use
                | SyntaxKind::Enum
                | SyntaxKind::Trait
                | SyntaxKind::Impl
                | SyntaxKind::Const
                | SyntaxKind::TypeKw
                | SyntaxKind::Extern
                | SyntaxKind::Unsafe
                | SyntaxKind::Break
                | SyntaxKind::Continue
                | SyntaxKind::Return
                | SyntaxKind::If
                | SyntaxKind::While
                | SyntaxKind::Loop
                | SyntaxKind::For
                | SyntaxKind::Match
        )
    }

    #[must_use]
    pub fn parse(mut self) -> (Vec<Event>, Vec<Token>, Vec<ParseError>, &'s str) {
        let m = self.start();

        while !self.at(SyntaxKind::Eof) {
            if self.nesting_exhausted {
                // The nesting diagnostic already explains the failure; swallow
                // the rest of the file so recovery does not report one cascade
                // error per remaining delimiter.
                while !self.at(SyntaxKind::Eof) {
                    self.bump();
                }
                break;
            }
            let pos_before_statement = self.current_non_trivia_pos;
            self.statement();
            self.force_progress(pos_before_statement);
        }
        self.eat_trivia();
        m.complete(&mut self, SyntaxKind::Root);

        (self.events, self.tokens, self.errors, self.source)
    }

    #[allow(clippy::type_complexity)]
    #[must_use]
    pub fn reparse(
        mut self,
        entry: ReparseEntry,
    ) -> Option<(Vec<Event>, Vec<Token>, Vec<ParseError>, &'s str)> {
        use ReparseEntry::{
            ArgList, Block, EnumVariant, Expression, FieldPattern, MatchArm, ParamList, Path,
            Pattern, Statement, StructFieldList, Type, TypeList, UseTree, UseTreeList,
        };
        match entry {
            Statement => {
                self.statement();
            }
            Expression => {
                self.expression()?;
            }
            Type => {
                self.ty();
            }
            Block => {
                self.block();
            }
            ParamList => {
                self.param_list();
            }
            StructFieldList => {
                self.struct_field_list();
            }
            ArgList => {
                self.arg_list();
            }
            UseTree => {
                self.use_tree();
            }
            UseTreeList => {
                self.use_tree_list();
            }
            Path => {
                self.path();
            }
            Pattern => {
                self.pattern();
            }
            MatchArm => {
                self.match_arm();
            }
            EnumVariant => {
                self.enum_variant();
            }
            FieldPattern => {
                self.field_pattern();
            }
            TypeList => {
                self.type_list();
            }
        }
        if self.current() != SyntaxKind::Eof {
            return None;
        }

        Some((self.events, self.tokens, self.errors, self.source))
    }

    fn at_stmt_start(&self) -> bool {
        if self.at(SyntaxKind::Fun) && self.nth(1) == SyntaxKind::LParen {
            return false;
        }
        if self.at(SyntaxKind::Unsafe) {
            return matches!(self.nth(1), SyntaxKind::Fun | SyntaxKind::Extern);
        }
        matches!(
            self.current(),
            SyntaxKind::Hash
                | SyntaxKind::Let
                | SyntaxKind::Pub
                | SyntaxKind::Fun
                | SyntaxKind::Struct
                | SyntaxKind::Mod
                | SyntaxKind::Use
                | SyntaxKind::Break
                | SyntaxKind::Continue
                | SyntaxKind::Return
                | SyntaxKind::Enum
                | SyntaxKind::Trait
                | SyntaxKind::Impl
                | SyntaxKind::Const
                | SyntaxKind::TypeKw
                | SyntaxKind::Extern
        )
    }

    const fn at_expr_start(&self) -> bool {
        matches!(
            self.current(),
            SyntaxKind::Hash
                | SyntaxKind::Number
                | SyntaxKind::Float
                | SyntaxKind::String
                | SyntaxKind::Char
                | SyntaxKind::True
                | SyntaxKind::False
                | SyntaxKind::Ident
                | SyntaxKind::SelfKw
                | SyntaxKind::SuperKw
                | SyntaxKind::CrateKw
                | SyntaxKind::ColonColon
                | SyntaxKind::LParen
                | SyntaxKind::LBrace
                | SyntaxKind::LBracket
                | SyntaxKind::If
                | SyntaxKind::While
                | SyntaxKind::Loop
                | SyntaxKind::For
                | SyntaxKind::Match
                | SyntaxKind::Unsafe
                | SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::Amp
                | SyntaxKind::AmpAmp
                | SyntaxKind::Star
                | SyntaxKind::Bang
                | SyntaxKind::Fun
                | SyntaxKind::Move
        )
    }

    // == stmt ==

    fn statement(&mut self) {
        let _rdg = RiddleDepthGuard::enter();
        self.attrs();
        match self.current() {
            SyntaxKind::Pub => self.pub_item(),
            SyntaxKind::Let => self.var_decl(),
            SyntaxKind::Fun if self.nth(1) == SyntaxKind::LParen => self.expr_stmt(),
            SyntaxKind::Fun => {
                self.func_decl();
            }
            SyntaxKind::Unsafe if self.nth(1) == SyntaxKind::Fun => {
                self.func_decl();
            }
            SyntaxKind::Unsafe if self.nth(1) == SyntaxKind::Extern => self.extern_decl(),
            SyntaxKind::Struct => self.struct_decl(),
            SyntaxKind::Mod => self.mod_decl(),
            SyntaxKind::Use => self.use_decl(),
            SyntaxKind::Enum => self.enum_decl(),
            SyntaxKind::Trait => self.trait_decl(),
            SyntaxKind::Impl => self.impl_decl(),
            SyntaxKind::Const => self.const_decl(),
            SyntaxKind::TypeKw => self.type_alias_decl(true),
            SyntaxKind::Break => self.loop_control_stmt(SyntaxKind::BreakStmt),
            SyntaxKind::Continue => self.loop_control_stmt(SyntaxKind::ContinueStmt),
            SyntaxKind::Return => self.return_stmt(),
            SyntaxKind::Extern => self.extern_decl(),
            SyntaxKind::Eof => (),
            _ => self.expr_stmt(),
        }
    }

    fn pub_item(&mut self) {
        match self.nth(1) {
            SyntaxKind::Fun => {
                self.func_decl();
            }
            SyntaxKind::Unsafe => match self.nth(2) {
                SyntaxKind::Fun => {
                    self.func_decl();
                }
                SyntaxKind::Extern => self.extern_decl(),
                _ => self.error(format!(
                    "expected 'fun' or 'extern' after 'pub unsafe', found {:?}",
                    self.nth(2)
                )),
            },
            SyntaxKind::Struct => self.struct_decl(),
            SyntaxKind::Mod => self.mod_decl(),
            SyntaxKind::Use => self.use_decl(),
            SyntaxKind::Enum => self.enum_decl(),
            SyntaxKind::Trait => self.trait_decl(),
            SyntaxKind::Const => self.const_decl(),
            SyntaxKind::TypeKw => self.type_alias_decl(true),
            SyntaxKind::Extern => self.extern_decl(),
            _ => self.error(format!(
                "expected item after 'pub', found {:?}",
                self.nth(1)
            )),
        }
    }

    fn optional_pub(&mut self) {
        if self.at(SyntaxKind::Pub) {
            self.bump();
        }
    }

    fn optional_unsafe(&mut self) {
        if self.at(SyntaxKind::Unsafe) {
            self.bump();
        }
    }

    fn attrs(&mut self) {
        while self.at(SyntaxKind::Hash) && self.nth(1) == SyntaxKind::LBracket {
            self.attr();
        }
    }

    fn attr(&mut self) {
        let m = self.start();
        self.bump(); // #
        self.expect(SyntaxKind::LBracket);
        self.balanced_tokens_until(SyntaxKind::RBracket);
        self.expect(SyntaxKind::RBracket);
        m.complete(self, SyntaxKind::Attribute);
    }

    fn balanced_tokens_until(&mut self, close: SyntaxKind) {
        while !self.at(close) && !self.at(SyntaxKind::Eof) {
            match self.current() {
                SyntaxKind::LParen => self.balanced_group(SyntaxKind::LParen, SyntaxKind::RParen),
                SyntaxKind::LBracket => {
                    self.balanced_group(SyntaxKind::LBracket, SyntaxKind::RBracket);
                }
                SyntaxKind::LBrace => self.balanced_group(SyntaxKind::LBrace, SyntaxKind::RBrace),
                _ => self.bump(),
            }
        }
    }

    fn balanced_group(&mut self, open: SyntaxKind, close: SyntaxKind) {
        self.expect(open);
        self.balanced_tokens_until(close);
        self.expect(close);
    }

    fn mod_decl(&mut self) {
        let m = self.start();

        self.optional_pub();
        self.expect(SyntaxKind::Mod);
        self.expect(SyntaxKind::Ident);

        if self.at(SyntaxKind::Semi) {
            self.bump();
        } else if self.at(SyntaxKind::LBrace) {
            self.bump();

            while !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
                self.statement();
            }

            self.expect(SyntaxKind::RBrace);
        } else {
            self.error(format!(
                "expected ';' or module body, found {:?}",
                self.current()
            ));
        }

        m.complete(self, SyntaxKind::ModDecl);
    }

    fn use_decl(&mut self) {
        let m = self.start();

        self.optional_pub();
        self.expect(SyntaxKind::Use);
        self.use_tree();
        self.expect(SyntaxKind::Semi);

        m.complete(self, SyntaxKind::UseDecl);
    }

    fn use_tree(&mut self) {
        let m = self.start();

        if self.at(SyntaxKind::LBrace) {
            self.use_tree_list();
        } else if self.at_path_start() {
            self.path();

            if self.at(SyntaxKind::As) {
                self.bump();
                self.expect(SyntaxKind::Ident);
            } else if self.at(SyntaxKind::ColonColon) {
                self.bump();

                if self.at(SyntaxKind::Star) {
                    self.bump();
                } else if self.at(SyntaxKind::LBrace) {
                    self.use_tree_list();
                } else {
                    self.error(format!(
                        "expected '*' or '{{' after '::' in use tree, found {:?}",
                        self.current()
                    ));
                }
            }
        } else {
            self.error(format!("expected use tree, found {:?}", self.current()));
        }

        m.complete(self, SyntaxKind::UseTree);
    }

    fn use_tree_list(&mut self) {
        let m = self.start();

        self.expect(SyntaxKind::LBrace);

        if !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            self.use_tree();
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RBrace) {
                    break;
                }
                self.use_tree();
            }
        }

        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::UseTreeList);
    }

    fn at_path_start(&self) -> bool {
        self.at(SyntaxKind::ColonColon) || Self::is_path_segment_start(self.current())
    }

    fn path(&mut self) -> CompletedMarker {
        let m = self.start();

        if self.at(SyntaxKind::ColonColon) {
            self.bump();
        }

        if Self::is_path_segment_start(self.current()) {
            self.path_segment();
        } else {
            self.error(format!("expected path segment, found {:?}", self.current()));
        }

        while self.at(SyntaxKind::ColonColon) && Self::is_path_segment_start(self.nth(1)) {
            self.bump();
            self.path_segment();
        }

        m.complete(self, SyntaxKind::Path)
    }

    fn path_segment(&mut self) {
        let m = self.start();

        if Self::is_path_segment_start(self.current()) {
            self.bump();
        } else {
            self.error(format!("expected path segment, found {:?}", self.current()));
        }

        if self.at(SyntaxKind::ColonColon)
            && self.nth(1) == SyntaxKind::Less
            && self.type_arg_list_followed_by(1, SyntaxKind::ColonColon)
        {
            self.bump(); // ::
            self.type_arg_list();
        }

        m.complete(self, SyntaxKind::PathSegment);
    }

    const fn is_path_segment_start(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::Ident | SyntaxKind::SelfKw | SyntaxKind::SuperKw | SyntaxKind::CrateKw
        )
    }

    fn var_decl(&mut self) {
        let m = self.start();
        self.bump(); // 'let'
        // `mut` belongs to the binding, as in Rust: `let mut x` is the pattern
        // `mut x`, and `let mut (a, b)` is therefore a syntax error.
        self.pattern();

        // `A | B` is a match-arm form only. Without this the stray `|` falls
        // into the recovery below, which assumes the pattern already errored
        // and would silently drop `| B = init` with no diagnostic at all.
        if self.at(SyntaxKind::Pipe) {
            self.error_no_bump(
                "or-patterns `A | B` are only allowed on match arms; a `let` pattern is a single pattern".into(),
            );
            self.sync_to_statement_boundary();
            m.complete(self, SyntaxKind::VarDecl);
            return;
        }

        // A malformed pattern leaves tokens the type/init grammar cannot
        // consume; sync to the statement boundary so one mistake stays one
        // diagnostic and later statements still parse.
        if !matches!(
            self.current(),
            SyntaxKind::Colon
                | SyntaxKind::Eq
                | SyntaxKind::Else
                | SyntaxKind::Semi
                | SyntaxKind::RBrace
                | SyntaxKind::Eof
        ) {
            self.sync_to_statement_boundary();
            m.complete(self, SyntaxKind::VarDecl);
            return;
        }

        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.ty();
        }

        let has_init = if self.at(SyntaxKind::Eq) {
            self.bump();
            self.expression();
            true
        } else {
            false
        };

        if self.at(SyntaxKind::Else) {
            self.bump();
            // `let x else { .. }` has no value to match the pattern against.
            if !has_init {
                self.error_no_bump("`let`-`else` requires an initializer".to_string());
            }
            if self.at(SyntaxKind::LBrace) {
                self.block();
            } else {
                self.error(format!(
                    "expected block after `else`, found {:?}",
                    self.current()
                ));
            }
        }

        self.expect(SyntaxKind::Semi);
        m.complete(self, SyntaxKind::VarDecl);
    }

    fn func_decl(&mut self) -> bool {
        let m = self.start();

        self.optional_pub();
        self.optional_unsafe();
        self.expect(SyntaxKind::Fun);
        self.expect(SyntaxKind::Ident);
        if self.at(SyntaxKind::Less) {
            self.generic_params(true, false);
        }

        self.param_list();

        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.ty();
        }

        if self.at(SyntaxKind::Where) {
            self.where_clause();
        }

        let has_body = if self.at(SyntaxKind::LBrace) {
            self.block();
            true
        } else {
            self.expect(SyntaxKind::Semi);
            false
        };

        m.complete(self, SyntaxKind::FuncDecl);
        has_body
    }

    fn param_list(&mut self) {
        let m = self.start();

        self.expect(SyntaxKind::LParen);

        if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
            self.param();
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RParen) || self.at(SyntaxKind::Eof) {
                    break;
                }
                self.param();
            }
        }

        self.expect(SyntaxKind::RParen);
        m.complete(self, SyntaxKind::ParamList);
    }

    fn param(&mut self) {
        self.attrs();
        let m = self.start();
        if self.at(SyntaxKind::Amp) || self.at(SyntaxKind::SelfKw) {
            if self.at(SyntaxKind::Amp) {
                self.bump();
                if self.at(SyntaxKind::Mut) {
                    self.bump();
                }
            }
            self.expect(SyntaxKind::SelfKw);
        } else {
            if self.at(SyntaxKind::Mut) {
                self.bump();
            }
            self.expect(SyntaxKind::Ident);
            let has_colon = self.expect(SyntaxKind::Colon);
            // A list delimiter here means the type slot is empty; keep the
            // delimiters intact so `ty` cannot eat them and the rest of the
            // parameter list still parses.
            let at_list_delimiter = matches!(
                self.current(),
                SyntaxKind::Comma
                    | SyntaxKind::RParen
                    | SyntaxKind::Arrow
                    | SyntaxKind::Semi
                    | SyntaxKind::Eof
            );
            if has_colon && at_list_delimiter {
                self.error_no_bump("expected parameter type".to_string());
            } else if !at_list_delimiter {
                self.ty();
            }
        }
        m.complete(self, SyntaxKind::Param);
    }

    /// `fun(params) { body }` anonymous functions were removed in favor of
    /// bracket lambdas. Consume the old shape so recovery and hovers stay
    /// anchored, and point the user at the replacement.
    fn removed_lambda_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        if self.at(SyntaxKind::Move) {
            self.bump();
        }
        // `error` records the diagnostic and bumps the `fun` token itself.
        self.error(
            "anonymous function syntax `fun(x) { ... }` has been removed; use a bracket lambda `[x -> x + 1]` instead".into(),
        );
        if self.at(SyntaxKind::Less) {
            self.generic_params(true, false);
        }
        if self.at(SyntaxKind::LParen) {
            self.lambda_param_list();
        }
        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.ty();
        }
        if self.at(SyntaxKind::Where) {
            self.where_clause();
        }
        if self.at(SyntaxKind::LBrace) {
            self.block();
        }
        m.complete(self, SyntaxKind::ErrorNode)
    }

    /// Snapshot of all parser state that speculative parsing may advance.
    fn speculation_point(&self) -> SpeculationPoint {
        SpeculationPoint {
            pos: self.pos,
            current_non_trivia_pos: self.current_non_trivia_pos,
            current_kind: self.current_kind,
            pending_split_greater: self.pending_split_greater,
            events_len: self.events.len(),
            errors_len: self.errors.len(),
        }
    }

    fn rollback_to(&mut self, point: SpeculationPoint) {
        self.pos = point.pos;
        self.current_non_trivia_pos = point.current_non_trivia_pos;
        self.current_kind = point.current_kind;
        self.pending_split_greater = point.pending_split_greater;
        self.events.truncate(point.events_len);
        self.errors.truncate(point.errors_len);
    }

    /// `[params -> body]` bracket lambda, e.g. `[it -> it * 2]`.
    ///
    /// Speculative: returns `None` without recording any diagnostic when the
    /// bracket group turns out not to be a lambda (caller rolls back and
    /// reinterprets it as an array literal / index). Diagnostics are only
    /// emitted once the `->` has committed us to the lambda reading.
    fn try_bracket_lambda_expr(&mut self) -> Option<CompletedMarker> {
        let m = self.start();
        if self.at(SyntaxKind::Move) {
            self.bump();
        }
        if !self.at(SyntaxKind::LBracket) {
            m.abandon(self);
            return None;
        }
        self.bump();
        if !matches!(
            self.current(),
            SyntaxKind::Arrow | SyntaxKind::RBracket | SyntaxKind::Eof
        ) {
            self.bracket_lambda_params();
        }
        if !self.at(SyntaxKind::Arrow) {
            m.abandon(self);
            return None;
        }
        self.bump();
        self.expression();
        self.expect(SyntaxKind::RBracket);
        Some(m.complete(self, SyntaxKind::BracketLambdaExpr))
    }

    fn bracket_lambda_params(&mut self) {
        let m = self.start();
        self.lambda_param();
        while self.at(SyntaxKind::Comma) {
            self.bump();
            if matches!(self.current(), SyntaxKind::Arrow | SyntaxKind::RBracket) {
                break;
            }
            self.lambda_param();
        }
        m.complete(self, SyntaxKind::ParamList);
    }

    /// `[` in expression-start position: bracket lambda if the group contains
    /// a top-level `->`, array literal otherwise.
    fn bracket_lambda_or_array(&mut self) -> CompletedMarker {
        let point = self.speculation_point();
        if let Some(lambda) = self.try_bracket_lambda_expr() {
            return lambda;
        }
        self.rollback_to(point);
        self.array_expr()
    }

    fn lambda_param_list(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::LParen);
        if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
            self.lambda_param();
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RParen) {
                    break;
                }
                self.lambda_param();
            }
        }
        self.expect(SyntaxKind::RParen);
        m.complete(self, SyntaxKind::ParamList);
    }

    fn lambda_param(&mut self) {
        let m = self.start();
        self.pattern();
        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.ty();
        }
        m.complete(self, SyntaxKind::Param);
    }

    fn struct_decl(&mut self) {
        let m = self.start();
        self.optional_pub();
        self.expect(SyntaxKind::Struct);
        self.expect(SyntaxKind::Ident);
        if self.at(SyntaxKind::Less) {
            // Type declarations may provide default type arguments (for
            // example `Range<T = i32>`), matching Rust's declaration rules.
            self.generic_params(true, true);
        }
        if self.at(SyntaxKind::Where) {
            self.where_clause();
        }
        self.struct_field_list();
        m.complete(self, SyntaxKind::StructDecl);
    }

    fn struct_field_list(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::LBrace);

        if !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            self.struct_field();
            loop {
                if self.at(SyntaxKind::Comma) {
                    self.bump();
                    if self.at(SyntaxKind::RBrace) || self.at(SyntaxKind::Eof) {
                        break;
                    }
                    self.struct_field();
                } else if self.at(SyntaxKind::Ident) {
                    // Missing comma between fields: report once and parse the
                    // next field instead of derailing into expression errors.
                    self.error_no_bump("expected `,` after struct field".to_string());
                    self.struct_field();
                } else {
                    break;
                }
            }
        }

        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::StructFieldList);
    }

    fn struct_field(&mut self) {
        self.attrs();
        let m = self.start();
        self.optional_pub();
        if self.at(SyntaxKind::Mut) {
            self.bump();
        }
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.ty();
        m.complete(self, SyntaxKind::StructField);
    }

    fn return_stmt(&mut self) {
        let m = self.start();
        self.bump();

        if !self.at(SyntaxKind::Semi) && !self.at(SyntaxKind::Eof) {
            self.expression();
        }

        self.expect(SyntaxKind::Semi);
        m.complete(self, SyntaxKind::ReturnStmt);
    }

    fn loop_control_stmt(&mut self, kind: SyntaxKind) {
        let m = self.start();
        self.bump();
        if kind == SyntaxKind::BreakStmt && self.at_expr_start() {
            self.expression();
        }
        self.expect(SyntaxKind::Semi);
        m.complete(self, kind);
    }

    fn block(&mut self) -> CompletedMarker {
        let _rdg = RiddleDepthGuard::enter();
        let guarded = self.enter_nesting();
        let m = self.start();
        self.expect(SyntaxKind::LBrace);
        if !guarded {
            self.expect(SyntaxKind::RBrace);
            let completed = m.complete(self, SyntaxKind::Block);
            self.exit_nesting();
            return completed;
        }
        let result = self.block_inner(m);
        self.exit_nesting();
        result
    }

    fn block_inner(&mut self, m: Marker) -> CompletedMarker {
        while !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            let pos_before_statement = self.current_non_trivia_pos;
            if self.at_stmt_start() {
                self.statement();
                continue;
            }

            if !self.at_expr_start() {
                self.error(format!(
                    "expected statement or expression, found {:?}",
                    self.current()
                ));
                // Consume the unexpected token to avoid infinite loop
                self.bump();
                continue;
            }

            let Some(expr) = self.statement_expression() else {
                self.force_progress(pos_before_statement);
                continue;
            };

            if self.at(SyntaxKind::Semi) {
                let stmt = expr.precede(self);
                self.bump();
                stmt.complete(self, SyntaxKind::ExprStmt);
                continue;
            }

            if self.at(SyntaxKind::RBrace) {
                break;
            }

            if is_expr_with_block(expr.kind(self)) {
                let stmt = expr.precede(self);
                stmt.complete(self, SyntaxKind::ExprStmt);
                continue;
            }

            self.error_no_bump(format!(
                "expected ';' or '}}' after expression, found {:?}",
                self.current()
            ));

            let stmt = expr.precede(self);
            stmt.complete(self, SyntaxKind::ExprStmt);

            if !self.at(SyntaxKind::RBrace)
                && !self.at(SyntaxKind::Eof)
                && !self.token_starts_statement()
            {
                let err = self.start();
                self.bump();
                err.complete(self, SyntaxKind::ErrorNode);
            }
        }

        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::Block)
    }

    fn expr_stmt(&mut self) {
        let Some(expr) = self.statement_expression() else {
            if !self.at(SyntaxKind::Eof) {
                let m = self.start();
                self.bump();
                m.complete(self, SyntaxKind::ErrorNode);
            }
            return;
        };

        let m = expr.precede(self);

        if self.at(SyntaxKind::Semi) {
            self.bump();
            m.complete(self, SyntaxKind::ExprStmt);
            return;
        }

        if is_expr_with_block(expr.kind(self)) {
            m.complete(self, SyntaxKind::ExprStmt);
            return;
        }

        // A missing `;` followed by a statement keyword keeps that keyword
        // in place (no bump) so the next statement parses normally.
        if self.token_starts_statement() || self.at(SyntaxKind::RBrace) {
            self.error_no_bump(format!(
                "expected ';' after expression, found {:?}",
                self.current()
            ));
        } else {
            self.error(format!(
                "expected ';' after expression, found {:?}",
                self.current()
            ));
        }
        m.complete(self, SyntaxKind::ExprStmt);
    }

    // == expr ==

    fn expression(&mut self) -> Option<CompletedMarker> {
        self.expr_bp(0)
    }

    fn statement_expression(&mut self) -> Option<CompletedMarker> {
        self.expr_bp_restricted(0, ExprRestrictions::STATEMENT)
    }

    fn expression_no_struct(&mut self) -> Option<CompletedMarker> {
        self.expr_bp_restricted(0, ExprRestrictions::NO_STRUCT_EXPR)
    }

    fn if_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        self.expect(SyntaxKind::If);
        if self.at(SyntaxKind::Let) {
            self.let_condition();
        } else {
            self.expression_no_struct();
        }

        if self.at(SyntaxKind::LBrace) {
            self.block();
        } else {
            self.error(format!(
                "expected block after if condition, found {:?}",
                self.current()
            ));
        }

        if self.at(SyntaxKind::Else) {
            self.bump();
            if self.at(SyntaxKind::If) {
                self.if_expr();
            } else if self.at(SyntaxKind::LBrace) {
                self.block();
            } else {
                self.error(format!(
                    "expected block or if after else, found {:?}",
                    self.current()
                ));
            }
        }

        m.complete(self, SyntaxKind::IfStmt)
    }

    fn while_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        self.expect(SyntaxKind::While);
        if self.at(SyntaxKind::Let) {
            self.let_condition();
        } else {
            self.expression_no_struct();
        }

        if self.at(SyntaxKind::LBrace) {
            self.block();
        } else {
            self.error(format!(
                "expected block after while condition, found {:?}",
                self.current()
            ));
        }

        m.complete(self, SyntaxKind::WhileStmt)
    }

    fn let_condition(&mut self) {
        let m = self.start();
        self.bump();
        self.pattern();
        self.expect(SyntaxKind::Eq);
        self.expression_no_struct();
        m.complete(self, SyntaxKind::LetCondition);
    }

    fn loop_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        self.expect(SyntaxKind::Loop);

        if self.at(SyntaxKind::LBrace) {
            self.block();
        } else {
            self.error(format!(
                "expected block after 'loop', found {:?}",
                self.current()
            ));
        }

        m.complete(self, SyntaxKind::LoopExpr)
    }

    fn for_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        self.expect(SyntaxKind::For);
        self.pattern();
        self.expect(SyntaxKind::In);
        self.expression_no_struct();

        if self.at(SyntaxKind::LBrace) {
            self.block();
        } else {
            self.error(format!(
                "expected block after for iterable, found {:?}",
                self.current()
            ));
        }

        m.complete(self, SyntaxKind::ForExpr)
    }

    fn match_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        self.expect(SyntaxKind::Match);
        self.expression_no_struct();
        self.expect(SyntaxKind::LBrace);

        if !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            let mut arm_ends_with_block = self.match_arm().unwrap_or_else(|| {
                self.sync_to_arm_boundary();
                false
            });
            loop {
                if self.at(SyntaxKind::Comma) {
                    self.bump();
                    if self.at(SyntaxKind::RBrace) || self.at(SyntaxKind::Eof) {
                        break;
                    }
                    arm_ends_with_block = self.match_arm().unwrap_or_else(|| {
                        self.sync_to_arm_boundary();
                        false
                    });
                    continue;
                }
                // A block-bodied arm may omit its trailing comma: the arm's
                // own closing `}` already terminates it, so keep parsing
                // arms instead of derailing into expression-error cascades.
                if arm_ends_with_block && !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof)
                {
                    arm_ends_with_block = self.match_arm().unwrap_or_else(|| {
                        self.sync_to_arm_boundary();
                        false
                    });
                    continue;
                }
                break;
            }
        }

        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::MatchExpr)
    }

    /// Skip the remains of a malformed arm: the body's own diagnostic was
    /// already reported, so drop tokens up to the next `,`/`}` without
    /// adding more.
    fn sync_to_arm_boundary(&mut self) {
        while !matches!(
            self.current(),
            SyntaxKind::Comma | SyntaxKind::RBrace | SyntaxKind::Eof
        ) {
            self.bump();
        }
    }

    fn unsafe_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        self.expect(SyntaxKind::Unsafe);
        // ponytail: unsafe only supports blocks for now
        self.block();
        m.complete(self, SyntaxKind::UnsafeExpr)
    }

    /// Parses one `pattern [if guard] => expr` arm. Returns `None` when the
    /// arm body failed to parse (the caller should resynchronize instead of
    /// letting the stray tokens cascade), otherwise whether the arm body is
    /// an expression-with-block, whose closing `}` ends the arm and makes
    /// the trailing comma optional.
    fn match_arm(&mut self) -> Option<bool> {
        self.attrs();
        let m = self.start();

        self.arm_pattern();

        if self.at(SyntaxKind::If) {
            self.bump();
            self.expression();
        }

        self.expect(SyntaxKind::FatArrow);
        let body = self.expression();
        let ends_with_block = body.is_some_and(|expr| is_expr_with_block(expr.kind(self)));

        m.complete(self, SyntaxKind::MatchArm);
        body.map(|_| ends_with_block)
    }

    fn expr_bp(&mut self, min_bp: u8) -> Option<CompletedMarker> {
        self.expr_bp_restricted(min_bp, ExprRestrictions::NONE)
    }

    fn expr_bp_restricted(
        &mut self,
        min_bp: u8,
        restrictions: ExprRestrictions,
    ) -> Option<CompletedMarker> {
        let _rdg = RiddleDepthGuard::enter();
        self.attrs();
        let _rdg = RiddleDepthGuard::enter();
        if riddle_call_depth() > MAX_NESTING_DEPTH * 12 {
            self.bail_out_nesting(false);
            return None;
        }
        self.attrs();
        // prefix
        let mut lhs = self.lhs(restrictions)?;
        let mut bare_block = restrictions.stop_infix_after_block
            && min_bp == 0
            && is_expr_with_block(lhs.kind(self));

        loop {
            // A block-shaped expression in statement position is a complete
            // statement, so the next token starts a new one: no postfix or
            // infix operator may attach to it. Checking this before the
            // postfix branch matters for `(`, which would otherwise parse the
            // following parenthesized statement as a call on the block —
            // `if c { .. }` followed by `(*p) = v;` became `if c { .. }(*p)`.
            if bare_block {
                break;
            }
            let op = self.current();
            let pos_before_iteration = self.current_non_trivia_pos;

            if lhs.kind(self) == SyntaxKind::NameRef
                && op == SyntaxKind::Less
                && self.type_arg_list_followed_by(0, SyntaxKind::LParen)
            {
                self.error_no_bump(
                    "generic arguments in expression paths must use `::<...>`".into(),
                );
                let m = lhs.precede(self);
                self.type_arg_list();
                self.arg_list();
                lhs = m.complete(self, SyntaxKind::CallExpr);
                bare_block = false;
                continue;
            }

            if matches!(lhs.kind(self), SyntaxKind::NameRef | SyntaxKind::FieldExpr)
                && op == SyntaxKind::ColonColon
                && self.nth(1) == SyntaxKind::Less
                && self.type_arg_list_followed_by(1, SyntaxKind::LParen)
            {
                let m = lhs.precede(self);
                self.bump(); // ::
                self.type_arg_list();
                self.arg_list();
                lhs = m.complete(self, SyntaxKind::CallExpr);
                bare_block = false;
                continue;
            }

            if lhs.kind(self) == SyntaxKind::NameRef
                && op == SyntaxKind::ColonColon
                && self.nth(1) == SyntaxKind::Less
                && self.type_arg_list_followed_by(1, SyntaxKind::LBrace)
            {
                let m = lhs.precede(self);
                self.bump(); // ::
                self.type_arg_list();
                self.struct_expr_field_list();
                lhs = m.complete(self, SyntaxKind::StructExpr);
                bare_block = false;
                continue;
            }

            if matches!(
                op,
                SyntaxKind::LParen | SyntaxKind::Dot | SyntaxKind::LBracket | SyntaxKind::Question
            ) {
                const POSTFIX_BP: u8 = 23;
                if POSTFIX_BP < min_bp {
                    break;
                }
                lhs = self.postfix_expr(lhs, op);
                bare_block = false;
                continue;
            }

            // struct literal
            if op == SyntaxKind::LBrace
                && restrictions.allow_struct_expr
                && lhs.kind(self) == SyntaxKind::NameRef
            {
                const STRUCT_BP: u8 = 23;
                if STRUCT_BP < min_bp {
                    break;
                }
                let m = lhs.precede(self);
                self.struct_expr_field_list();
                lhs = m.complete(self, SyntaxKind::StructExpr);
                bare_block = false;
                continue;
            }

            // cast
            if op == SyntaxKind::As {
                const CAST_BP: u8 = 21;
                if CAST_BP < min_bp {
                    break;
                }
                let m = lhs.precede(self);
                self.bump(); // 'as'
                self.ty();
                lhs = m.complete(self, SyntaxKind::CastExpr);
                bare_block = false;
                continue;
            }

            // infix
            // binary
            // range
            if matches!(op, SyntaxKind::DotDot | SyntaxKind::DotDotEq) {
                const RANGE_BP: u8 = 1;
                if RANGE_BP < min_bp {
                    break;
                }
                let m = lhs.precede(self);
                self.bump(); // '..' | '..='
                self.expr_bp_restricted(RANGE_BP + 1, restrictions);
                lhs = m.complete(self, SyntaxKind::RangeExpr);
                bare_block = false;
                continue;
            }
            let Some((l_bp, r_bp)) = infix_binding_power(op) else {
                break;
            };

            if l_bp < min_bp {
                break;
            }

            let m = lhs.precede(self);
            self.bump(); // operator
            self.expr_bp_restricted(r_bp, restrictions);
            lhs = m.complete(self, SyntaxKind::BinaryExpr);

            if !self.iteration_made_progress(pos_before_iteration) {
                break;
            }
        }

        Some(lhs)
    }

    /// True when an iteration of the Pratt loop consumed no input; continuing
    /// would loop forever on the same token after a depth-bailout.
    fn iteration_made_progress(&self, pos_before: usize) -> bool {
        self.current_non_trivia_pos != pos_before
    }

    /// Consumes one token when an iteration consumed nothing, so a bailout
    /// that returns without advancing cannot spin the enclosing loop on the
    /// same position. The nesting guards push one diagnostic per attempt, so
    /// a stalled loop would also grow `errors` without bound.
    fn force_progress(&mut self, pos_before: usize) {
        if self.iteration_made_progress(pos_before) {
            return;
        }
        let m = self.start();
        self.bump();
        m.complete(self, SyntaxKind::ErrorNode);
    }

    fn postfix_expr(&mut self, lhs: CompletedMarker, op: SyntaxKind) -> CompletedMarker {
        let _rdg = RiddleDepthGuard::enter();
        if riddle_call_depth() > MAX_NESTING_DEPTH * 12 {
            self.bail_out_nesting(false);
            // Consume the offending delimiter so the caller's loop makes
            // progress instead of retrying the same token forever.
            self.bump();
            let m = lhs.precede(self);
            return m.complete(self, SyntaxKind::ErrorNode);
        }
        let m = lhs.precede(self);
        match op {
            SyntaxKind::LParen => {
                self.arg_list();
                m.complete(self, SyntaxKind::CallExpr)
            }
            SyntaxKind::Dot => {
                self.bump();
                if self.at(SyntaxKind::Ident) || self.at(SyntaxKind::Number) {
                    self.bump();
                } else {
                    self.expect(SyntaxKind::Ident);
                }
                m.complete(self, SyntaxKind::FieldExpr)
            }
            SyntaxKind::LBracket => {
                // Trailing bracket lambda (`c.map [it -> it * 2]`,
                // `f(args) [acc, v -> acc + v]`); fall back to indexing.
                let point = self.speculation_point();
                let arg_list = self.start();
                if self.try_bracket_lambda_expr().is_some() {
                    arg_list.complete(self, SyntaxKind::ArgList);
                    return m.complete(self, SyntaxKind::CallExpr);
                }
                arg_list.abandon(self);
                self.rollback_to(point);
                self.bump();
                self.expression();
                self.expect(SyntaxKind::RBracket);
                m.complete(self, SyntaxKind::IndexExpr)
            }
            SyntaxKind::Question => {
                self.bump();
                m.complete(self, SyntaxKind::TryExpr)
            }
            _ => unreachable!("postfix expression called with {op:?}"),
        }
    }

    fn nth_non_trivia_index(&self, n: usize) -> Option<usize> {
        let mut remaining = n;
        let mut i = self.pos;
        while i < self.tokens.len() {
            if !self.tokens[i].kind.is_trivia() {
                if remaining == 0 {
                    return Some(i);
                }
                remaining -= 1;
            }
            i += 1;
        }
        None
    }

    fn next_non_trivia_kind_after(&self, mut i: usize) -> SyntaxKind {
        i += 1;
        while i < self.tokens.len() {
            if !self.tokens[i].kind.is_trivia() {
                return self.tokens[i].kind;
            }
            i += 1;
        }
        SyntaxKind::Eof
    }

    fn type_arg_list_followed_by(&self, start: usize, follow: SyntaxKind) -> bool {
        if self.pending_split_greater > 0 {
            return false;
        }

        let Some(mut i) = self.nth_non_trivia_index(start) else {
            return false;
        };
        if self.tokens[i].kind != SyntaxKind::Less {
            return false;
        }

        let mut depth = 0usize;
        while i < self.tokens.len() {
            match self.tokens[i].kind {
                kind if kind.is_trivia() => {}
                SyntaxKind::Less => depth += 1,
                SyntaxKind::Greater => {
                    if depth == 0 {
                        return false;
                    }
                    depth -= 1;
                    if depth == 0 {
                        return self.next_non_trivia_kind_after(i) == follow;
                    }
                }
                SyntaxKind::Shr => {
                    if depth < 2 {
                        return false;
                    }
                    depth -= 2;
                    if depth == 0 {
                        return self.next_non_trivia_kind_after(i) == follow;
                    }
                }
                SyntaxKind::Eof => return false,
                // Tokens that cannot appear inside a type-argument list end
                // the search: without this, `while a < b.len() { .. c > (d) }`
                // pairs the condition's `<` with an unrelated `(`-followed
                // `>` in the loop body and misparses the condition as a
                // generic call.
                SyntaxKind::Dot
                | SyntaxKind::LParen
                | SyntaxKind::RParen
                | SyntaxKind::LBrace
                | SyntaxKind::RBrace
                | SyntaxKind::Semi => return false,
                _ => {}
            }
            i += 1;
        }

        false
    }

    fn arg_list(&mut self) {
        let _rdg = RiddleDepthGuard::enter();
        if riddle_call_depth() > MAX_NESTING_DEPTH * 12 {
            self.bail_out_nesting(true);
            return;
        }
        let m = self.start();
        self.bump();

        if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
            self.expression();
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RParen) {
                    break;
                }
                self.expression();
            }
        }

        self.expect(SyntaxKind::RParen);
        m.complete(self, SyntaxKind::ArgList);
    }

    // parse prefix, atom, block
    fn lhs(&mut self, restrictions: ExprRestrictions) -> Option<CompletedMarker> {
        self.attrs();
        if !self.enter_nesting() {
            return None;
        }
        let completed = self.lhs_inner(restrictions);
        self.exit_nesting();
        completed
    }

    fn lhs_inner(&mut self, restrictions: ExprRestrictions) -> Option<CompletedMarker> {
        let _rdg = RiddleDepthGuard::enter();
        match self.current() {
            // unary
            SyntaxKind::Amp => {
                let m = self.start();
                self.bump(); // &
                if self.at(SyntaxKind::Mut) {
                    self.bump(); // mut
                }
                let r_bp = prefix_binding_power(SyntaxKind::Amp);
                self.expr_bp_restricted(r_bp, restrictions);
                Some(m.complete(self, SyntaxKind::UnaryExpr))
            }
            SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star | SyntaxKind::Bang => {
                let m = self.start();
                let op = self.current();
                self.bump(); // operator
                let r_bp = prefix_binding_power(op);
                self.expr_bp_restricted(r_bp, restrictions);
                Some(m.complete(self, SyntaxKind::UnaryExpr))
            }

            SyntaxKind::Dot => {
                self.error_no_bump("expected expression before field access".to_string());
                let m = self.start();
                Some(m.complete(self, SyntaxKind::ErrorNode))
            }

            SyntaxKind::AmpAmp => {
                let m = self.start();
                let op = self.current();
                self.bump(); // &&
                let r_bp = prefix_binding_power(op);
                self.expr_bp_restricted(r_bp, restrictions);
                Some(m.complete(self, SyntaxKind::UnaryExpr))
            }

            SyntaxKind::Number => {
                let m = self.start();
                self.bump();
                Some(m.complete(self, SyntaxKind::NumberLit))
            }

            SyntaxKind::Float => {
                let m = self.start();
                self.bump();
                Some(m.complete(self, SyntaxKind::FloatLit))
            }

            SyntaxKind::String => {
                let m = self.start();
                self.bump();
                Some(m.complete(self, SyntaxKind::StringLit))
            }

            SyntaxKind::Char => {
                let m = self.start();
                self.bump();
                Some(m.complete(self, SyntaxKind::CharLit))
            }

            SyntaxKind::True | SyntaxKind::False => {
                let m = self.start();
                self.bump();
                Some(m.complete(self, SyntaxKind::BoolLit))
            }

            SyntaxKind::Ident
            | SyntaxKind::SelfKw
            | SyntaxKind::SuperKw
            | SyntaxKind::CrateKw
            | SyntaxKind::ColonColon => {
                let m = self.start();
                self.path();
                if self.at(SyntaxKind::Bang) {
                    Some(self.finish_macro_call(m))
                } else {
                    Some(m.complete(self, SyntaxKind::NameRef))
                }
            }

            SyntaxKind::LBrace => Some(self.block()),

            SyntaxKind::If => Some(self.if_expr()),

            SyntaxKind::While => Some(self.while_expr()),

            SyntaxKind::Loop => Some(self.loop_expr()),

            SyntaxKind::For => Some(self.for_expr()),

            SyntaxKind::Match => Some(self.match_expr()),

            SyntaxKind::Unsafe => Some(self.unsafe_expr()),

            SyntaxKind::Fun if self.nth(1) == SyntaxKind::LParen => {
                Some(self.removed_lambda_expr())
            }
            SyntaxKind::Move if self.nth(1) == SyntaxKind::Fun => Some(self.removed_lambda_expr()),
            SyntaxKind::Move if self.nth(1) == SyntaxKind::LBracket => {
                let point = self.speculation_point();
                match self.try_bracket_lambda_expr() {
                    Some(lambda) => Some(lambda),
                    None => {
                        self.rollback_to(point);
                        self.error("expected a lambda after 'move'".into());
                        None
                    }
                }
            }
            SyntaxKind::Move => {
                self.error("expected '[' after 'move'".into());
                None
            }

            SyntaxKind::LBracket => Some(self.bracket_lambda_or_array()),

            SyntaxKind::LParen => Some(self.paren_expr(restrictions)),

            _ => {
                self.error_no_bump(format!("expected expression, found {:?}", self.current()));
                None
            }
        }
    }

    fn array_expr(&mut self) -> CompletedMarker {
        let m = self.start();
        self.bump();

        if !self.at(SyntaxKind::RBracket) && !self.at(SyntaxKind::Eof) {
            self.expression();
            if self.at(SyntaxKind::Semi) {
                self.bump();
                self.expression();
            } else {
                while self.at(SyntaxKind::Comma) {
                    self.bump();
                    if self.at(SyntaxKind::RBracket) {
                        break;
                    }
                    self.expression();
                }
            }
        }

        self.expect(SyntaxKind::RBracket);
        m.complete(self, SyntaxKind::ArrayExpr)
    }

    fn paren_expr(&mut self, restrictions: ExprRestrictions) -> CompletedMarker {
        let _rdg = RiddleDepthGuard::enter();
        if riddle_call_depth() > MAX_NESTING_DEPTH * 12 {
            self.bail_out_nesting(true);
            let m = self.start();
            let done = m.complete(self, SyntaxKind::ParenExpr);
            return done;
        }
        let m = self.start();
        self.bump();
        if !self.at(SyntaxKind::RParen) {
            self.expr_bp_restricted(0, restrictions);
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RParen) {
                    break;
                }
                self.expr_bp_restricted(0, restrictions);
            }
        }
        self.expect(SyntaxKind::RParen);
        m.complete(self, SyntaxKind::ParenExpr)
    }

    fn struct_expr_field_list(&mut self) {
        self.expect(SyntaxKind::LBrace);

        if !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            self.struct_expr_field();
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RBrace) {
                    break;
                }
                self.struct_expr_field();
            }
        }

        self.expect(SyntaxKind::RBrace);
    }

    fn struct_expr_field(&mut self) {
        let m = self.start();

        self.expect(SyntaxKind::Ident);
        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.expression();
        }

        m.complete(self, SyntaxKind::StructExprField);
    }

    // == type ==

    fn ty(&mut self) {
        let _rdg = RiddleDepthGuard::enter();
        self.attrs();
        if !self.enter_nesting() {
            return;
        }
        self.ty_inner();
        self.exit_nesting();
    }

    fn ty_inner(&mut self) {
        match self.current() {
            SyntaxKind::Bang => {
                let m = self.start();
                self.bump();
                m.complete(self, SyntaxKind::NeverType);
            }
            SyntaxKind::Amp => {
                let m = self.start();
                self.bump(); // &
                if self.at(SyntaxKind::Mut) {
                    self.bump(); // mut
                }
                self.ty();
                m.complete(self, SyntaxKind::RefType);
            }
            SyntaxKind::AmpAmp => {
                let outer = self.start();
                let inner = self.start();
                self.bump();
                self.ty();
                inner.complete(self, SyntaxKind::RefType);
                outer.complete(self, SyntaxKind::RefType);
            }
            SyntaxKind::Star => {
                let m = self.start();
                self.bump(); // *
                let is_mut = self.at(SyntaxKind::Mut);
                if is_mut || self.at(SyntaxKind::Const) {
                    self.bump(); // const or mut
                } else {
                    self.error(format!(
                        "expected 'const' or 'mut' after '*' in pointer type, found {:?}",
                        self.current()
                    ));
                }
                self.ty();
                m.complete(self, SyntaxKind::PtrType);
            }
            SyntaxKind::LParen => {
                let m = self.start();
                self.bump();

                if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
                    self.ty();
                    while self.at(SyntaxKind::Comma) {
                        self.bump();
                        if self.at(SyntaxKind::RParen) {
                            break;
                        }
                        self.ty();
                    }
                }

                self.expect(SyntaxKind::RParen);
                m.complete(self, SyntaxKind::TupleType);
            }
            SyntaxKind::LBracket => {
                let m = self.start();
                self.bump();
                self.ty();

                if self.at(SyntaxKind::Semi) {
                    self.bump();
                    self.expression();
                }

                self.expect(SyntaxKind::RBracket);
                m.complete(self, SyntaxKind::ArrayType);
            }
            SyntaxKind::Number => {
                let m = self.start();
                self.bump();
                m.complete(self, SyntaxKind::ConstType);
            }
            SyntaxKind::Impl => self.impl_trait_type(),
            SyntaxKind::Dyn => self.dyn_trait_type(),
            SyntaxKind::Fun | SyntaxKind::Unsafe => self.removed_function_type(),
            SyntaxKind::Ident
            | SyntaxKind::SelfKw
            | SyntaxKind::SuperKw
            | SyntaxKind::CrateKw
            | SyntaxKind::ColonColon => {
                let m = self.start();
                self.path();
                if self.at(SyntaxKind::Bang) {
                    self.finish_macro_call(m);
                } else {
                    if self.at(SyntaxKind::Less) {
                        self.type_arg_list();
                    }
                    m.complete(self, SyntaxKind::NamedType);
                }
            }
            _ => self.error(format!("expected type, found {:?}", self.current())),
        }
    }

    fn removed_function_type(&mut self) {
        let m = self.start();
        self.error(
            "function type syntax has been removed; use `impl Fn(i32) -> i32` or an explicit `F: Fn(i32) -> i32` bound".into(),
        );
        self.optional_unsafe();
        self.expect(SyntaxKind::Fun);
        self.expect(SyntaxKind::LParen);
        if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
            self.ty();
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RParen) {
                    break;
                }
                self.ty();
            }
        }
        self.expect(SyntaxKind::RParen);
        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.ty();
        }
        m.complete(self, SyntaxKind::ErrorNode);
    }

    fn finish_macro_call(&mut self, marker: Marker) -> CompletedMarker {
        self.expect(SyntaxKind::Bang);
        match self.current() {
            SyntaxKind::LParen => {
                self.balanced_group(SyntaxKind::LParen, SyntaxKind::RParen);
            }
            SyntaxKind::LBrace => {
                self.balanced_group(SyntaxKind::LBrace, SyntaxKind::RBrace);
            }
            SyntaxKind::LBracket => {
                self.balanced_group(SyntaxKind::LBracket, SyntaxKind::RBracket);
            }
            _ => self.error(format!(
                "expected a delimited token tree after `!`, found {:?}",
                self.current()
            )),
        }
        marker.complete(self, SyntaxKind::MacroCall)
    }

    // == new items ==

    fn enum_decl(&mut self) {
        let m = self.start();

        self.optional_pub();
        self.expect(SyntaxKind::Enum);
        self.expect(SyntaxKind::Ident);
        if self.at(SyntaxKind::Less) {
            self.generic_params(true, true);
        }
        if self.at(SyntaxKind::Where) {
            self.where_clause();
        }
        self.expect(SyntaxKind::LBrace);

        if !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            self.enum_variant();
            while self.at(SyntaxKind::Comma) {
                self.bump();
                if self.at(SyntaxKind::RBrace) {
                    break;
                }
                self.enum_variant();
            }
        }

        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::EnumDecl);
    }

    fn enum_variant(&mut self) {
        self.attrs();
        let m = self.start();

        self.expect(SyntaxKind::Ident);

        // tuple variant: ident(type, type, ...)
        if self.at(SyntaxKind::LParen) {
            self.bump();
            if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
                self.ty();
                while self.at(SyntaxKind::Comma) {
                    self.bump();
                    if self.at(SyntaxKind::RParen) {
                        break;
                    }
                    self.ty();
                }
            }
            self.expect(SyntaxKind::RParen);
        }
        // struct variant: ident { field: ty, ... }
        else if self.at(SyntaxKind::LBrace) {
            self.struct_field_list();
        }

        m.complete(self, SyntaxKind::EnumVariant);
    }

    fn trait_decl(&mut self) {
        let m = self.start();

        self.optional_pub();
        self.expect(SyntaxKind::Trait);
        self.expect(SyntaxKind::Ident);
        if self.at(SyntaxKind::Less) {
            self.generic_params(true, true);
        }
        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.generic_bound();
            while self.at(SyntaxKind::Plus) {
                self.bump();
                self.generic_bound();
            }
        }
        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            self.trait_item();
        }

        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::TraitDecl);
    }

    fn trait_item(&mut self) {
        self.attrs();
        match self.current() {
            SyntaxKind::Pub => match self.nth(1) {
                SyntaxKind::Fun | SyntaxKind::Unsafe => {
                    self.func_decl();
                }
                SyntaxKind::TypeKw => self.type_alias_decl(false),
                _ => self.error(format!("expected trait item, found {:?}", self.current())),
            },
            SyntaxKind::Fun | SyntaxKind::Unsafe => {
                self.func_decl();
            }
            SyntaxKind::TypeKw => self.type_alias_decl(false),
            _ => {
                self.error(format!("expected trait item, found {:?}", self.current()));
            }
        }
    }

    fn func_sig(&mut self, allow_safe: bool) {
        let m = self.start();

        self.optional_pub();
        if allow_safe && self.at(SyntaxKind::Safe) {
            self.bump();
        } else {
            self.optional_unsafe();
        }
        self.expect(SyntaxKind::Fun);
        self.expect(SyntaxKind::Ident);
        if self.at(SyntaxKind::Less) {
            self.generic_params(true, false);
            self.error_no_bump(
                "extern function declarations cannot have generic parameters".into(),
            );
        }
        self.param_list();

        if self.at(SyntaxKind::Arrow) {
            self.bump();
            self.ty();
        }

        if self.at(SyntaxKind::Where) {
            self.where_clause();
        }

        self.expect(SyntaxKind::Semi);
        m.complete(self, SyntaxKind::FuncDecl);
    }

    fn impl_decl(&mut self) {
        let m = self.start();

        self.expect(SyntaxKind::Impl);

        // optional generic_params
        if self.at(SyntaxKind::Less) {
            self.generic_params(true, false);
        }

        if self.at_callable_trait_path() {
            self.impl_callable_trait_type();
        } else {
            self.ty();
        }

        // optional "for" ty
        if self.at(SyntaxKind::For) {
            self.bump();
            self.ty();
        }

        if self.at(SyntaxKind::Where) {
            self.where_clause();
        }

        self.expect(SyntaxKind::LBrace);

        while !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
            self.impl_item();
        }

        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::ImplDecl);
    }

    fn at_callable_trait_path(&self) -> bool {
        if self.current() != SyntaxKind::Ident || self.nth(1) != SyntaxKind::LParen {
            return false;
        }
        let token = &self.tokens[self.current_non_trivia_pos];
        matches!(&self.source[token.span.clone()], "Fn" | "FnMut" | "FnOnce")
    }

    fn impl_callable_trait_type(&mut self) {
        let m = self.start();
        self.path();
        self.callable_trait_args();
        m.complete(self, SyntaxKind::NamedType);
    }

    fn impl_item(&mut self) {
        self.attrs();
        match self.current() {
            SyntaxKind::Pub => match self.nth(1) {
                SyntaxKind::Fun | SyntaxKind::Unsafe => {
                    self.func_decl();
                }
                SyntaxKind::TypeKw => self.type_alias_decl(true),
                SyntaxKind::Const => self.const_decl(),
                _ => self.error(format!("expected impl item, found {:?}", self.current())),
            },
            SyntaxKind::Fun | SyntaxKind::Unsafe => {
                self.func_decl();
            }
            SyntaxKind::TypeKw => self.type_alias_decl(true),
            SyntaxKind::Const => self.const_decl(),
            _ => {
                self.error(format!("expected impl item, found {:?}", self.current()));
            }
        }
    }

    fn const_decl(&mut self) {
        let m = self.start();

        self.optional_pub();
        self.expect(SyntaxKind::Const);
        self.expect(SyntaxKind::Ident);
        self.expect(SyntaxKind::Colon);
        self.ty();

        if self.expect(SyntaxKind::Eq) {
            self.expression();
        }

        self.expect(SyntaxKind::Semi);
        m.complete(self, SyntaxKind::ConstDecl);
    }

    fn type_alias_decl(&mut self, require_value: bool) {
        let m = self.start();

        self.optional_pub();
        self.expect(SyntaxKind::TypeKw);
        self.expect(SyntaxKind::Ident);

        if require_value && self.expect(SyntaxKind::Eq) {
            self.ty();
        } else if !require_value && self.at(SyntaxKind::Eq) {
            self.bump();
            self.ty();
        }

        self.expect(SyntaxKind::Semi);
        m.complete(self, SyntaxKind::TypeAliasDecl);
    }

    fn generic_params(&mut self, allow_bounds: bool, allow_defaults: bool) {
        let m = self.start();

        self.expect(SyntaxKind::Less);
        self.generic_param(allow_bounds, allow_defaults);
        while self.at(SyntaxKind::Comma) {
            self.bump();
            self.generic_param(allow_bounds, allow_defaults);
        }
        self.expect(SyntaxKind::Greater);

        m.complete(self, SyntaxKind::GenericParams);
    }

    fn generic_param(&mut self, allow_bounds: bool, allow_defaults: bool) {
        if self.at(SyntaxKind::Const) {
            self.bump();
            self.expect(SyntaxKind::Ident);
            self.expect(SyntaxKind::Colon);
            self.ty();
            return;
        }

        self.expect(SyntaxKind::Ident);
        if allow_bounds && self.at(SyntaxKind::Colon) {
            self.bump();
            self.generic_bound();
            while self.at(SyntaxKind::Plus) {
                self.bump();
                self.generic_bound();
            }
        }
        if allow_defaults && self.at(SyntaxKind::Eq) {
            self.bump();
            self.ty();
        }
    }

    fn generic_bound(&mut self) {
        self.path();
        if self.at(SyntaxKind::LParen) {
            self.callable_trait_args();
        } else if self.at(SyntaxKind::Less) {
            self.bump();
            if !self.at(SyntaxKind::Greater) && !self.at(SyntaxKind::Eof) {
                self.generic_bound_arg();
                while self.at(SyntaxKind::Comma) {
                    self.bump();
                    if self.at(SyntaxKind::Greater) {
                        break;
                    }
                    self.generic_bound_arg();
                }
            }
            if self.at(SyntaxKind::Shr) {
                self.split_shr_as_greater();
            } else {
                self.expect(SyntaxKind::Greater);
            }
        }
    }

    fn callable_trait_args(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::LParen);
        if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
            self.type_list();
        }
        self.expect(SyntaxKind::RParen);
        self.expect(SyntaxKind::Arrow);
        self.ty();
        m.complete(self, SyntaxKind::CallableTraitArgs);
    }

    fn impl_trait_type(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::Impl);
        self.generic_bound();
        m.complete(self, SyntaxKind::ImplTraitType);
    }

    fn dyn_trait_type(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::Dyn);
        self.generic_bound();
        m.complete(self, SyntaxKind::DynTraitType);
    }

    fn generic_bound_arg(&mut self) {
        if self.at(SyntaxKind::Ident) && self.nth(1) == SyntaxKind::Eq {
            self.bump();
            self.bump();
        }
        self.ty();
    }

    fn where_clause(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::Where);
        self.where_predicate();
        while self.at(SyntaxKind::Comma) {
            self.bump();
            if self.at(SyntaxKind::LBrace) || self.at(SyntaxKind::Semi) || self.at(SyntaxKind::Eof)
            {
                break;
            }
            self.where_predicate();
        }
        m.complete(self, SyntaxKind::WhereClause);
    }

    fn where_predicate(&mut self) {
        self.ty();
        self.expect(SyntaxKind::Colon);
        self.generic_bound();
        while self.at(SyntaxKind::Plus) {
            self.bump();
            self.generic_bound();
        }
    }

    fn type_list(&mut self) {
        self.ty();
        while self.at(SyntaxKind::Comma) {
            self.bump();
            if !self.at_type_start() {
                break;
            }
            self.ty();
        }
    }

    fn type_arg_list(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::Less);
        if !self.at(SyntaxKind::Greater) && !self.at(SyntaxKind::Eof) {
            self.type_list();
        }
        if self.at(SyntaxKind::Shr) {
            self.split_shr_as_greater();
        } else {
            self.expect(SyntaxKind::Greater);
        }
        m.complete(self, SyntaxKind::TypeArgList);
    }

    fn extern_decl(&mut self) {
        self.attrs();
        let m = self.start();
        self.optional_pub();
        let is_unsafe = self.at(SyntaxKind::Unsafe);
        self.optional_unsafe();
        self.expect(SyntaxKind::Extern);
        let _abi = self.expect(SyntaxKind::String); // "C"

        if self.at(SyntaxKind::Fun) {
            // extern "C" fun name(...) -> T { body }
            if !self.func_decl() {
                self.error(
                    "single-function extern declarations are not supported; use an unsafe extern block"
                        .into(),
                );
            }
            m.complete(self, SyntaxKind::ExternFnDecl);
        } else if self.at(SyntaxKind::LBrace) {
            // extern "C" { fun ...; fun ...; }
            if !is_unsafe {
                self.error_no_bump("extern blocks must use `unsafe extern`".into());
            }
            self.expect(SyntaxKind::LBrace);
            while !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
                self.attrs();
                if matches!(
                    self.current(),
                    SyntaxKind::Fun | SyntaxKind::Safe | SyntaxKind::Unsafe
                ) {
                    self.func_sig(true);
                } else {
                    self.error(format!(
                        "expected 'fun' in extern block, found {:?}",
                        self.current()
                    ));
                    break;
                }
            }
            self.expect(SyntaxKind::RBrace);
            m.complete(self, SyntaxKind::ExternBlock);
        } else {
            self.error(format!(
                "expected 'fun' or '{{' after extern \"C\", found {:?}",
                self.current()
            ));
            m.complete(self, SyntaxKind::ErrorNode);
        }
    }

    const fn at_type_start(&self) -> bool {
        matches!(
            self.current(),
            SyntaxKind::Ident
                | SyntaxKind::SelfKw
                | SyntaxKind::SuperKw
                | SyntaxKind::CrateKw
                | SyntaxKind::ColonColon
                | SyntaxKind::Amp
                | SyntaxKind::AmpAmp
                | SyntaxKind::Star
                | SyntaxKind::LParen
                | SyntaxKind::LBracket
                | SyntaxKind::Number
                | SyntaxKind::Bang
                | SyntaxKind::Fun
                | SyntaxKind::Unsafe
                | SyntaxKind::Impl
                | SyntaxKind::Dyn
        )
    }

    // == patterns ==

    /// A match-arm pattern: one or more `|`-separated patterns. A lone
    /// pattern keeps its own node; two or more wrap in an `OrPattern`. Rust
    /// also allows a leading `|`, which is consumed before the marker so a
    /// single alternative still lowers as the plain pattern it is equivalent
    /// to (and does not inherit the alternatives' no-binding rule).
    fn arm_pattern(&mut self) {
        if self.at(SyntaxKind::Pipe) {
            self.bump();
        }
        let m = self.start();
        self.pattern();
        if !self.at(SyntaxKind::Pipe) {
            m.abandon(self);
            return;
        }
        while self.at(SyntaxKind::Pipe) {
            self.bump();
            self.pattern();
        }
        m.complete(self, SyntaxKind::OrPattern);
    }

    fn pattern(&mut self) {
        let _rdg = RiddleDepthGuard::enter();
        self.pattern_inner();
    }

    fn pattern_inner(&mut self) {
        self.attrs();
        if !self.enter_nesting() {
            return;
        }
        self.pattern_inner_guarded();
        self.exit_nesting();
    }

    fn pattern_inner_guarded(&mut self) {
        if self.at(SyntaxKind::Mut) {
            // `mut name` — only a bare binding can be mutable.
            let m = self.start();
            self.bump();
            self.expect(SyntaxKind::Ident);
            m.complete(self, SyntaxKind::BindingPattern);
            return;
        }
        match self.current() {
            SyntaxKind::Amp => {
                let m = self.start();
                self.bump();
                if self.at(SyntaxKind::Mut) {
                    self.bump();
                }
                self.pattern();
                m.complete(self, SyntaxKind::ReferencePattern);
            }
            SyntaxKind::AmpAmp => {
                let outer = self.start();
                let inner = self.start();
                self.bump();
                if self.at(SyntaxKind::Mut) {
                    self.bump();
                }
                self.pattern();
                inner.complete(self, SyntaxKind::ReferencePattern);
                outer.complete(self, SyntaxKind::ReferencePattern);
            }
            SyntaxKind::Underscore => {
                let m = self.start();
                self.bump();
                m.complete(self, SyntaxKind::WildcardPattern);
            }
            SyntaxKind::Number
            | SyntaxKind::Float
            | SyntaxKind::String
            | SyntaxKind::Char
            | SyntaxKind::True
            | SyntaxKind::False => {
                let m = self.start();
                self.bump();
                m.complete(self, SyntaxKind::LiteralPattern);
            }
            SyntaxKind::LParen => {
                let m = self.start();
                self.bump();

                if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
                    self.pattern();
                    while self.at(SyntaxKind::Comma) {
                        self.bump();
                        if self.at(SyntaxKind::RParen) {
                            break;
                        }
                        self.pattern();
                    }
                }

                self.expect(SyntaxKind::RParen);
                m.complete(self, SyntaxKind::TuplePattern);
            }
            SyntaxKind::Ident
            | SyntaxKind::SelfKw
            | SyntaxKind::SuperKw
            | SyntaxKind::CrateKw
            | SyntaxKind::ColonColon => self.path_pattern(),
            _ => {
                self.error(format!("expected pattern, found {:?}", self.current()));
            }
        }
    }

    fn path_pattern(&mut self) {
        let m = self.start();
        self.path();

        if self.at(SyntaxKind::Bang) {
            self.finish_macro_call(m);
            return;
        }

        match self.current() {
            SyntaxKind::LParen => {
                // enum tuple pattern: Variant(a, b)
                self.bump();
                if !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
                    self.pattern();
                    while self.at(SyntaxKind::Comma) {
                        self.bump();
                        if self.at(SyntaxKind::RParen) {
                            break;
                        }
                        self.pattern();
                    }
                }
                self.expect(SyntaxKind::RParen);
                m.complete(self, SyntaxKind::EnumPattern);
            }
            SyntaxKind::LBrace => {
                // enum struct pattern: Variant { a, b: c }
                self.bump();
                if !self.at(SyntaxKind::RBrace) && !self.at(SyntaxKind::Eof) {
                    self.field_pattern();
                    while self.at(SyntaxKind::Comma) {
                        self.bump();
                        if self.at(SyntaxKind::RBrace) {
                            break;
                        }
                        self.field_pattern();
                    }
                }
                self.expect(SyntaxKind::RBrace);
                m.complete(self, SyntaxKind::EnumPattern);
            }
            _ => {
                // Plain ident binding or path pattern.
                m.complete(self, SyntaxKind::EnumPattern);
            }
        }
    }

    fn field_pattern(&mut self) {
        self.attrs();
        let m = self.start();
        self.expect(SyntaxKind::Ident);

        if self.at(SyntaxKind::Colon) {
            self.bump();
            self.pattern();
        }

        m.complete(self, SyntaxKind::StructPattern);
    }
}

// == pratt binding power ==

const fn is_expr_with_block(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Block
            | SyntaxKind::IfStmt
            | SyntaxKind::WhileStmt
            | SyntaxKind::LoopExpr
            | SyntaxKind::ForExpr
            | SyntaxKind::MatchExpr
            | SyntaxKind::UnsafeExpr
    )
}

/// prefix binding power for `rhs`
const fn prefix_binding_power(op: SyntaxKind) -> u8 {
    match op {
        SyntaxKind::Plus
        | SyntaxKind::Minus
        | SyntaxKind::Amp
        | SyntaxKind::AmpAmp
        | SyntaxKind::Star
        | SyntaxKind::Bang => 22,
        _ => 0,
    }
}

/// infix operator (left bp, right bp)
///
/// left < right => left combination
///
/// left > right => right combination
///
/// Bitwise ordering mirrors Rust (and C's ordering among the bitwise ops
/// themselves): `<< >>` bind tightest, then `&`, then `^`, then `|`, all
/// above the comparison operators.
const fn infix_binding_power(op: SyntaxKind) -> Option<(u8, u8)> {
    match op {
        SyntaxKind::Eq
        | SyntaxKind::PlusEq
        | SyntaxKind::MinusEq
        | SyntaxKind::StarEq
        | SyntaxKind::SlashEq
        | SyntaxKind::PercentEq
        | SyntaxKind::AmpEq
        | SyntaxKind::PipeEq
        | SyntaxKind::CaretEq
        | SyntaxKind::ShlEq
        | SyntaxKind::ShrEq => Some((1, 1)),
        SyntaxKind::PipePipe => Some((2, 3)),
        SyntaxKind::AmpAmp => Some((4, 5)),
        SyntaxKind::EqEq | SyntaxKind::BangEq => Some((6, 7)),
        SyntaxKind::Less | SyntaxKind::Greater | SyntaxKind::LessEq | SyntaxKind::GreaterEq => {
            Some((8, 9))
        }
        SyntaxKind::Pipe => Some((10, 11)),
        SyntaxKind::Caret => Some((12, 13)),
        SyntaxKind::Amp => Some((14, 15)),
        SyntaxKind::Shl | SyntaxKind::Shr => Some((16, 17)),
        SyntaxKind::Plus | SyntaxKind::Minus => Some((18, 19)),
        SyntaxKind::Star | SyntaxKind::Slash | SyntaxKind::Percent => Some((20, 21)),
        _ => None,
    }
}

use crate::lexer::Token;
use crate::{Parse, ParseError, SyntaxKind};
use rowan::GreenNodeBuilder;
use std::cell::Cell;
use std::ops::Range;

/// Maximum nesting depth for expressions and blocks.
const MAX_NESTING_DEPTH: usize = 48;

thread_local! {
    static RIDDLE_CALL_DEPTH: Cell<usize> = const { Cell::new(0) };
}

fn riddle_call_depth() -> usize {
    RIDDLE_CALL_DEPTH.get()
}

/// RAII guard tracking recursive calls into the parser, so deeply nested
/// input cannot overflow the stack through long call chains.
struct RiddleDepthGuard;

impl RiddleDepthGuard {
    fn enter() -> Self {
        RIDDLE_CALL_DEPTH.with(|d| d.set(d.get() + 1));
        Self
    }
}

impl Drop for RiddleDepthGuard {
    fn drop(&mut self) {
        RIDDLE_CALL_DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)] // AddSyntheticToken/Placeholder serve speculative parsing, not yet needed by the grammar
enum Event {
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

/// Marks a position where a node was started; the node's kind is backpatched
/// on `complete`, or the node is dropped on `abandon`.
#[derive(Debug)]
struct Marker {
    pos: usize,
    completed: bool,
}

impl Marker {
    fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.completed = true;
        match &mut p.events[self.pos] {
            Event::StartNode { kind: slot, .. } => *slot = kind,
            _ => unreachable!(),
        }
        p.events.push(Event::FinishNode);
        CompletedMarker { pos: self.pos }
    }

    /// Drops the node without emitting it. If tokens were already parsed into
    /// it, a Placeholder is left behind so they stay attached to the parent.
    #[allow(dead_code)] // reserved for speculative parsing
    fn abandon(mut self, p: &mut Parser) {
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

/// A completed node that can be wrapped in a new parent via `precede()`.
#[derive(Debug, Clone, Copy)]
struct CompletedMarker {
    pos: usize,
}

impl CompletedMarker {
    /// Insert a new parent node before this node in the hierarchy.
    ///
    /// Used for Pratt parsing: first the value `1` is parsed; later it turns
    /// out the expression is actually `1 + 2`, so the `1` is wrapped inside a
    /// `BinaryExpr`.
    fn precede(self, p: &mut Parser) -> Marker {
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

pub fn parse(text: &str, tokens: &[Token]) -> Parse {
    let mut p = Parser {
        text,
        tokens,
        pos: 0,
        events: Vec::new(),
        errors: Vec::new(),
    };
    p.parse_file();
    build_tree(&p.events, text, tokens, p.errors)
}

struct Parser<'a> {
    text: &'a str,
    tokens: &'a [Token],
    pos: usize,
    events: Vec<Event>,
    errors: Vec<ParseError>,
}

impl Parser<'_> {
    fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Returns the kind of the `n`-th non-trivia token from the current position.
    fn nth(&self, n: usize) -> SyntaxKind {
        let mut idx = self.pos;
        let mut seen = 0;
        while idx < self.tokens.len() {
            let kind = self.tokens[idx].kind;
            if !kind.is_trivia() {
                if seen == n {
                    return kind;
                }
                seen += 1;
            }
            idx += 1;
        }
        SyntaxKind::Eof
    }

    /// Consumes and emits all consecutive trivia tokens starting at the
    /// current position.
    fn eat_trivia(&mut self) {
        while self.pos < self.tokens.len() && self.tokens[self.pos].kind.is_trivia() {
            self.events.push(Event::AddToken);
            self.pos += 1;
        }
    }

    /// Advances the parser by one token.
    fn bump(&mut self) {
        self.eat_trivia();
        if self.pos < self.tokens.len() {
            self.events.push(Event::AddToken);
            self.pos += 1;
        }
    }

    /// Checks whether the current token's syntax kind matches the given `kind`.
    fn at(&self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    /// Attempts to consume the current token if it matches `kind`.
    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Expects the next token to be of the given `kind`.
    fn expect(&mut self, kind: SyntaxKind) {
        if !self.eat(kind) {
            self.error_no_bump(format!("expected {kind}, found {}", self.current()));
        }
    }

    /// Consumes the next token if it matches the expected [`SyntaxKind`].
    fn expect_what(&mut self, kind: SyntaxKind, what: &str) {
        if !self.eat(kind) {
            self.error_no_bump(format!("expected {what}, found {}", self.current()));
        }
    }

    /// Returns the range of the current token.
    fn get_range(&self) -> Range<usize> {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].range.clone()
        } else {
            self.text.len()..self.text.len()
        }
    }

    /// Records an error and wraps the offending token in an ERROR_NODE.
    fn error(&mut self, message: String) {
        let range = self.get_range();
        self.errors.push(ParseError { message, range });
        if !self.at(SyntaxKind::Eof) {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::ErrorNode);
        }
    }

    /// Records an error without consuming the current token, so the caller's
    /// recovery loop can reinterpret it.
    fn error_no_bump(&mut self, message: String) {
        let range = self.get_range();
        self.errors.push(ParseError { message, range });
    }

    /// Opens a node whose kind is backpatched later via `Marker::complete`.
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

    /// Rejects nesting deeper than the limit: the remaining tokens are folded
    /// into one error node instead of recursing until the stack overflows.
    fn bail_out_nesting(&mut self) {
        self.error_no_bump(format!(
            "maximum nesting depth of {MAX_NESTING_DEPTH} exceeded"
        ));
        let m = self.start();
        while !self.at(SyntaxKind::Eof) && !self.at(SyntaxKind::RBrace) {
            self.bump();
        }
        m.complete(self, SyntaxKind::ErrorNode);
    }

    // --- grammar rules ---

    fn parse_file(&mut self) {
        let m = self.start();
        loop {
            self.eat_trivia();
            match self.current() {
                SyntaxKind::Eof => break,
                _ => self.parse_item(),
            }
        }
        m.complete(self, SyntaxKind::SourceFile);
    }

    fn parse_item(&mut self) {
        match self.current() {
            SyntaxKind::KwFun => self.parse_item_func(),
            _ => self.error("unknown token".into()),
        }
    }

    fn parse_item_func(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::KwFun);
        self.parse_name("<function name>");
        self.parse_param_list();
        self.parse_block();
        m.complete(self, SyntaxKind::Function);
    }

    fn parse_name(&mut self, what: &str) {
        self.eat_trivia();
        let m = self.start();
        self.expect_what(SyntaxKind::Ident, what);
        m.complete(self, SyntaxKind::Name);
    }

    fn parse_param_list(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::LParen);
        while !self.at(SyntaxKind::RParen) && !self.at(SyntaxKind::Eof) {
            if self.at(SyntaxKind::Ident) {
                self.parse_name("<parameter name>");
            } else {
                self.error("expected parameter or `)`".into());
            }
        }
        self.expect(SyntaxKind::RParen);
        m.complete(self, SyntaxKind::ParamList);
    }

    fn parse_stmt(&mut self) {
        match self.current() {
            SyntaxKind::KwLet => self.parse_stmt_let(),
            _ => self.parse_stmt_expr(),
        }
    }

    fn parse_block(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::LBrace);
        loop {
            self.eat_trivia();
            match self.current() {
                SyntaxKind::RBrace | SyntaxKind::Eof => break,
                _ => self.parse_stmt(),
            }
        }
        self.expect(SyntaxKind::RBrace);
        m.complete(self, SyntaxKind::Block);
    }

    fn parse_stmt_let(&mut self) {
        let m = self.start();
        self.expect(SyntaxKind::KwLet);
        self.parse_name("<variable name>");
        self.expect(SyntaxKind::Eq);
        self.parse_expr(0);
        self.expect(SyntaxKind::Semicolon);
        m.complete(self, SyntaxKind::LetStmt);
    }

    fn parse_stmt_expr(&mut self) {
        let Some(expr) = self.parse_expr(0) else {
            return;
        };
        let m = expr.precede(self);
        self.eat(SyntaxKind::Semicolon);
        m.complete(self, SyntaxKind::ExprStmt);
    }

    /// Parses an expression using Pratt parsing.
    ///
    /// `min_bp` is the minimum binding power for the current recursion level:
    /// only infix operators with a binding power of at least `min_bp` are
    /// folded into the current expression; weaker operators are left for the
    /// caller to handle, which is how precedence and associativity are
    /// expressed. Top-level callers pass `0` to accept any operator.
    fn parse_expr(&mut self, min_bp: u8) -> Option<CompletedMarker> {
        let _rdg = RiddleDepthGuard::enter();
        if riddle_call_depth() > MAX_NESTING_DEPTH * 12 {
            self.bail_out_nesting();
            return None;
        }

        self.eat_trivia();
        // prefix and atom
        let mut lhs = match self.current() {
            SyntaxKind::IntNumber | SyntaxKind::String => {
                let m = self.start();
                self.bump();
                m.complete(self, SyntaxKind::Literal)
            }
            SyntaxKind::Ident => {
                let m = self.start();
                self.bump();
                m.complete(self, SyntaxKind::NameRef)
            }
            // group expr
            SyntaxKind::LParen => {
                let m = self.start();
                self.bump();
                self.parse_expr(0);
                self.expect(SyntaxKind::RParen);
                m.complete(self, SyntaxKind::ParenExpr)
            }
            _ => {
                self.error("expected expression".into());
                return None;
            }
        };

        // infix
        loop {
            self.eat_trivia();
            let (l_bp, r_bp) = match self.current() {
                SyntaxKind::Plus | SyntaxKind::Minus => (1, 2),
                SyntaxKind::Star | SyntaxKind::Slash => (3, 4),
                _ => break,
            };
            if l_bp < min_bp {
                break;
            }
            let m = lhs.precede(self);
            self.bump(); // operator
            self.parse_expr(r_bp);
            lhs = m.complete(self, SyntaxKind::BinaryExpr);
        }

        Some(lhs)
    }
}

/// Replays the recorded events through a `GreenNodeBuilder`, expanding
/// forward-parent chains so that nodes wrapped via `precede()` nest correctly.
fn build_tree(
    events: &[Event],
    text: &str,
    tokens: &[Token],
    errors: Vec<ParseError>,
) -> Parse {
    let mut builder = GreenNodeBuilder::new();
    let mut token_idx: usize = 0;
    let mut forward_parents = vec![];
    let mut visited = vec![false; events.len()];

    for i in 0..events.len() {
        if visited[i] {
            continue;
        }

        match &events[i] {
            Event::StartNode { .. } => {
                // collect forward_parent chain
                forward_parents.clear();
                let mut cur = i;
                loop {
                    match &events[cur] {
                        Event::StartNode {
                            kind,
                            forward_parent,
                        } => {
                            forward_parents.push(*kind);
                            visited[cur] = true;
                            match forward_parent {
                                Some(offset) => cur += offset,
                                None => break,
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                // The chain is innermost-first; open nodes outermost-first.
                for &kind in forward_parents.iter().rev() {
                    if kind != SyntaxKind::Tombstone {
                        builder.start_node(kind.to_raw());
                    }
                }
            }
            Event::FinishNode => {
                builder.finish_node();
            }
            Event::AddToken => {
                if token_idx < tokens.len() {
                    let tok = &tokens[token_idx];
                    builder.token(tok.kind.to_raw(), &text[tok.range.clone()]);
                    token_idx += 1;
                }
            }
            Event::AddSyntheticToken {
                kind,
                text,
                consume,
            } => {
                builder.token(kind.to_raw(), text);
                if *consume {
                    token_idx += 1;
                }
            }
            Event::Placeholder => {}
        }
    }

    Parse {
        green: builder.finish(),
        errors,
    }
}

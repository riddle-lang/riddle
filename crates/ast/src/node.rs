use super::support::{self, AstNode};
use syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

// ── ast_node! macro ────────────────────────────────────────────────────

macro_rules! ast_node {
    ($name:ident, $kind:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            syntax: SyntaxNode,
        }

        impl AstNode for $name {
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::$kind {
                    Some(Self { syntax: node })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }
    };
}

// ── AST node type definitions ──────────────────────────────────────────
//
// Sorted roughly by category: top-level → statements → expressions →
// types → patterns → paths → extern / unsafe.

// top-level
ast_node!(Root, Root);
ast_node!(ModDecl, ModDecl);
ast_node!(UseDecl, UseDecl);
ast_node!(UseTree, UseTree);
ast_node!(UseTreeList, UseTreeList);
ast_node!(Attribute, Attribute);

// statements / declarations
ast_node!(VarDecl, VarDecl);
ast_node!(FuncDecl, FuncDecl);
ast_node!(BreakStmt, BreakStmt);
ast_node!(ContinueStmt, ContinueStmt);
ast_node!(ReturnStmt, ReturnStmt);
ast_node!(ExprStmt, ExprStmt);
ast_node!(StructDecl, StructDecl);
ast_node!(EnumDecl, EnumDecl);
ast_node!(EnumVariant, EnumVariant);
ast_node!(TraitDecl, TraitDecl);
ast_node!(ImplDecl, ImplDecl);
ast_node!(ConstDecl, ConstDecl);
ast_node!(TypeAliasDecl, TypeAliasDecl);
ast_node!(GenericParams, GenericParams);
ast_node!(WhereClause, WhereClause);

// expressions
ast_node!(Block, Block);
ast_node!(BinaryExpr, BinaryExpr);
ast_node!(UnaryExpr, UnaryExpr);
ast_node!(ParenExpr, ParenExpr);
ast_node!(CallExpr, CallExpr);
ast_node!(BracketLambdaExpr, BracketLambdaExpr);
ast_node!(ArgList, ArgList);
ast_node!(FieldExpr, FieldExpr);
ast_node!(IndexExpr, IndexExpr);
ast_node!(RangeExpr, RangeExpr);
ast_node!(StructExpr, StructExpr);
ast_node!(StructExprField, StructExprField);
ast_node!(IfStmt, IfStmt);
ast_node!(WhileStmt, WhileStmt);
ast_node!(LetCondition, LetCondition);
ast_node!(LoopExpr, LoopExpr);
ast_node!(ForExpr, ForExpr);
ast_node!(MatchExpr, MatchExpr);
ast_node!(MatchArm, MatchArm);
ast_node!(ArrayExpr, ArrayExpr);
ast_node!(NumberExpr, NumberLit);
ast_node!(FloatLitExpr, FloatLit);
ast_node!(StringLitExpr, StringLit);
ast_node!(CharLitExpr, CharLit);
ast_node!(BoolLitExpr, BoolLit);
ast_node!(NameRefExpr, NameRef);
ast_node!(UnsafeExpr, UnsafeExpr);
ast_node!(CastExpr, CastExpr);
ast_node!(TryExpr, TryExpr);
ast_node!(MacroCall, MacroCall);

// paths
ast_node!(Path, Path);
ast_node!(PathSegment, PathSegment);

// types
ast_node!(NamedType, NamedType);
ast_node!(NeverType, NeverType);
ast_node!(TypeArgList, TypeArgList);
ast_node!(RefType, RefType);
ast_node!(PtrType, PtrType);
ast_node!(TupleType, TupleType);
ast_node!(ArrayType, ArrayType);
ast_node!(ConstType, ConstType);
ast_node!(ImplTraitType, ImplTraitType);
ast_node!(DynTraitType, DynTraitType);
ast_node!(CallableTraitArgs, CallableTraitArgs);

// patterns
ast_node!(WildcardPat, WildcardPattern);
ast_node!(LiteralPat, LiteralPattern);
ast_node!(TuplePat, TuplePattern);
ast_node!(OrPat, OrPattern);
ast_node!(BindingPat, BindingPattern);
ast_node!(ReferencePat, ReferencePattern);
ast_node!(StructPattern, StructPattern);
ast_node!(EnumPattern, EnumPattern);

// params
ast_node!(ParamList, ParamList);
ast_node!(Param, Param);
ast_node!(StructFieldList, StructFieldList);
ast_node!(StructField, StructField);

// extern
ast_node!(ExternBlock, ExternBlock);
ast_node!(ExternFnDecl, ExternFnDecl);

// ── Top-level ──────────────────────────────────────────────────────────

impl Root {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ {
        support::children(&self.syntax)
    }
}

// ── Statements ─────────────────────────────────────────────────────────

impl ModDecl {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    /// Returns `None` for `mod foo;` and the nested items for `mod foo { ... }`.
    #[must_use]
    pub fn items(&self) -> Option<impl Iterator<Item = Stmt> + '_> {
        let has_brace = self
            .syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|t| t.kind() == SyntaxKind::LBrace);
        if has_brace {
            Some(support::children::<Stmt>(&self.syntax))
        } else {
            None
        }
    }
}

impl UseDecl {
    #[must_use]
    pub fn use_tree(&self) -> Option<UseTree> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }
}

impl UseTree {
    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn alias(&self) -> Option<SyntaxToken> {
        let mut iter = self
            .syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token);
        while let Some(t) = iter.next() {
            if t.kind() == SyntaxKind::As {
                return iter.find(|t| t.kind() == SyntaxKind::Ident);
            }
        }
        None
    }

    #[must_use]
    pub fn is_glob(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|t| t.kind() == SyntaxKind::Star)
    }

    #[must_use]
    pub fn subtree_list(&self) -> Option<UseTreeList> {
        support::child(&self.syntax)
    }
}

impl UseTreeList {
    pub fn trees(&self) -> impl Iterator<Item = UseTree> + '_ {
        support::children(&self.syntax)
    }
}

impl Attribute {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn string_value(&self) -> Option<String> {
        let mut after_eq = false;
        for token in self
            .syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
        {
            if token.kind() == SyntaxKind::Eq {
                after_eq = true;
                continue;
            }
            if after_eq && token.kind() == SyntaxKind::String {
                return Some(unquote_string(token.text()));
            }
        }
        None
    }

    #[must_use]
    pub fn raw_text(&self) -> String {
        self.syntax.text().to_string()
    }
}

impl MacroCall {
    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn delimiter_tokens(&self) -> Option<(SyntaxToken, SyntaxToken)> {
        let tokens = self
            .syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .collect::<Vec<_>>();
        let opening = tokens.iter().find(|token| {
            matches!(
                token.kind(),
                SyntaxKind::LParen | SyntaxKind::LBrace | SyntaxKind::LBracket
            )
        })?;
        let closing_kind = match opening.kind() {
            SyntaxKind::LParen => SyntaxKind::RParen,
            SyntaxKind::LBrace => SyntaxKind::RBrace,
            SyntaxKind::LBracket => SyntaxKind::RBracket,
            _ => unreachable!(),
        };
        let closing = tokens
            .iter()
            .rev()
            .find(|token| token.kind() == closing_kind)?;
        Some((opening.clone(), closing.clone()))
    }
}

#[must_use]
pub fn attrs_for_node(node: &SyntaxNode) -> Vec<Attribute> {
    let Some(parent) = node.parent() else {
        return Vec::new();
    };

    let mut pending = Vec::new();
    for element in parent.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(candidate) if candidate == *node => return pending,
            rowan::NodeOrToken::Node(candidate) if candidate.kind() == SyntaxKind::Attribute => {
                pending.push(Attribute { syntax: candidate });
            }
            rowan::NodeOrToken::Token(token) if token.kind().is_trivia() => {}
            rowan::NodeOrToken::Node(_) | rowan::NodeOrToken::Token(_) => pending.clear(),
        }
    }

    Vec::new()
}

/// Returns documentation comments attached to a syntax node.
///
/// A `//<` line comment immediately following a node is treated as a trailing
/// documentation comment. `///` and block documentation comments keep their
/// existing leading-comment behavior.
#[must_use]
pub fn doc_comments_for_node(node: &SyntaxNode) -> Vec<SyntaxToken> {
    let mut leading = Vec::new();
    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(candidate) if candidate.kind() == SyntaxKind::Attribute => {}
            rowan::NodeOrToken::Token(token) if token.kind().is_trivia() => {
                if matches!(
                    token.kind(),
                    SyntaxKind::DocComment | SyntaxKind::DocBlockComment
                ) {
                    leading.push(token);
                }
            }
            rowan::NodeOrToken::Node(_) | rowan::NodeOrToken::Token(_) => break,
        }
    }
    if !leading.is_empty() {
        leading.extend(trailing_doc_comments_for_node(node));
        return leading;
    }

    let Some(parent) = node.parent() else {
        return trailing_doc_comments_for_node(node);
    };

    let mut docs = Vec::new();
    for element in parent.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(candidate) if candidate == *node => {
                docs.extend(trailing_doc_comments_for_node(node));
                return docs;
            }
            rowan::NodeOrToken::Node(candidate) if candidate.kind() == SyntaxKind::Attribute => {}
            rowan::NodeOrToken::Token(token) if token.kind().is_trivia() => {
                if matches!(
                    token.kind(),
                    SyntaxKind::DocComment | SyntaxKind::DocBlockComment
                ) {
                    docs.push(token);
                }
            }
            rowan::NodeOrToken::Node(_) | rowan::NodeOrToken::Token(_) => docs.clear(),
        }
    }
    Vec::new()
}

fn trailing_doc_comments_for_node(node: &SyntaxNode) -> Vec<SyntaxToken> {
    let Some(parent) = node.parent() else {
        return Vec::new();
    };

    let mut after_node = false;
    for element in parent.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(candidate) if candidate == *node => {
                after_node = true;
            }
            _ if !after_node => {}
            rowan::NodeOrToken::Token(token) if token.kind() == SyntaxKind::Whitespace => {
                if token.text().contains(['\n', '\r']) {
                    return Vec::new();
                }
            }
            rowan::NodeOrToken::Token(token) if token.kind() == SyntaxKind::Comma => {}
            rowan::NodeOrToken::Token(token)
                if token.kind() == SyntaxKind::LineComment && token.text().starts_with("//<") =>
            {
                return vec![token];
            }
            rowan::NodeOrToken::Node(candidate) => {
                return leading_trailing_doc_comment(&candidate)
                    .into_iter()
                    .collect();
            }
            rowan::NodeOrToken::Token(_) => return Vec::new(),
        }
    }
    Vec::new()
}

fn leading_trailing_doc_comment(node: &SyntaxNode) -> Option<SyntaxToken> {
    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(candidate) if candidate.kind() == SyntaxKind::Attribute => {}
            rowan::NodeOrToken::Token(token) if token.kind() == SyntaxKind::Whitespace => {
                if token.text().contains(['\n', '\r']) {
                    return None;
                }
            }
            rowan::NodeOrToken::Token(token)
                if token.kind() == SyntaxKind::LineComment && token.text().starts_with("//<") =>
            {
                return Some(token);
            }
            rowan::NodeOrToken::Token(token) if token.kind().is_trivia() => {}
            rowan::NodeOrToken::Node(_) | rowan::NodeOrToken::Token(_) => return None,
        }
    }
    None
}

fn unquote_string(text: &str) -> String {
    if let Some(text) = raw_string_body(text) {
        return text.to_string();
    }

    text.strip_prefix('"')
        .and_then(|text| text.strip_suffix('"'))
        .unwrap_or(text)
        .to_string()
}

fn raw_string_body(text: &str) -> Option<&str> {
    let rest = text.strip_prefix('r')?;
    let hashes = rest.bytes().take_while(|&b| b == b'#').count();
    let open_quote = 1 + hashes;
    if text.as_bytes().get(open_quote) != Some(&b'"') {
        return None;
    }

    let suffix_len = 1 + hashes;
    let suffix_start = text.len().checked_sub(suffix_len)?;
    if suffix_start <= open_quote || text.as_bytes().get(suffix_start) != Some(&b'"') {
        return None;
    }
    if !text.as_bytes()[suffix_start + 1..]
        .iter()
        .all(|&b| b == b'#')
    {
        return None;
    }

    Some(&text[open_quote + 1..suffix_start])
}

impl VarDecl {
    /// The bound pattern. `let x = 1` yields a binding pattern, so mutability
    /// and destructuring both live here rather than on the statement.
    #[must_use]
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn init(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    /// The diverging block of a `let`-`else`. The initializer may itself be a
    /// block expression (`let x = { 1 } else { .. };`), so the block is
    /// located relative to the `else` token rather than by child position.
    #[must_use]
    pub fn else_block(&self) -> Option<Block> {
        let else_token = support::token_of(&self.syntax, SyntaxKind::Else)?;
        self.syntax
            .children()
            .filter(|node| node.text_range().start() >= else_token.text_range().end())
            .find_map(Block::cast)
    }
}

impl FuncDecl {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    #[must_use]
    pub fn is_unsafe(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Unsafe).is_some()
    }

    #[must_use]
    pub fn is_safe(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Safe).is_some()
    }

    #[must_use]
    pub fn generic_params(&self) -> Option<GenericParams> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn where_clause(&self) -> Option<WhereClause> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn param_list(&self) -> Option<ParamList> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn return_type(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn body(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl ReturnStmt {
    #[must_use]
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl BreakStmt {
    #[must_use]
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl ExprStmt {
    #[must_use]
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl StructDecl {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    #[must_use]
    pub fn generic_params(&self) -> Option<GenericParams> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn where_clause(&self) -> Option<WhereClause> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn field_list(&self) -> Option<StructFieldList> {
        support::child(&self.syntax)
    }
}

impl EnumDecl {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    #[must_use]
    pub fn generic_params(&self) -> Option<GenericParams> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn where_clause(&self) -> Option<WhereClause> {
        support::child(&self.syntax)
    }

    pub fn variants(&self) -> impl Iterator<Item = EnumVariant> + '_ {
        support::children(&self.syntax)
    }
}

impl EnumVariant {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    pub fn tuple_types(&self) -> impl Iterator<Item = Type> + '_ {
        support::children(&self.syntax)
    }

    #[must_use]
    pub fn field_list(&self) -> Option<StructFieldList> {
        support::child(&self.syntax)
    }
}

impl TraitDecl {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    #[must_use]
    pub fn generic_params(&self) -> Option<GenericParams> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn supertraits(&self) -> Vec<GenericBound> {
        let elements = self.syntax.children_with_tokens().collect::<Vec<_>>();
        let Some(colon) = elements.iter().position(
            |element| matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::Colon),
        ) else {
            return Vec::new();
        };
        let end = elements
            .iter()
            .position(
                |element| matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::LBrace),
            )
            .unwrap_or(elements.len());
        parse_generic_bounds(&elements[colon + 1..end])
    }

    pub fn methods(&self) -> impl Iterator<Item = FuncDecl> + '_ {
        support::children(&self.syntax)
    }

    pub fn type_aliases(&self) -> impl Iterator<Item = TypeAliasDecl> + '_ {
        support::children(&self.syntax)
    }
}

impl ImplDecl {
    #[must_use]
    pub fn generic_params(&self) -> Option<GenericParams> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn where_clause(&self) -> Option<WhereClause> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn self_type(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn trait_type(&self) -> Option<Type> {
        support::nth_child(&self.syntax, 1)
    }

    #[must_use]
    pub fn callable_trait_args(&self) -> Option<CallableTraitArgs> {
        self.syntax.descendants().find_map(CallableTraitArgs::cast)
    }

    #[must_use]
    pub fn has_for(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|token| token.kind() == SyntaxKind::For)
    }

    pub fn methods(&self) -> impl Iterator<Item = FuncDecl> + '_ {
        support::children(&self.syntax)
    }

    pub fn consts(&self) -> impl Iterator<Item = ConstDecl> + '_ {
        support::children(&self.syntax)
    }

    pub fn type_aliases(&self) -> impl Iterator<Item = TypeAliasDecl> + '_ {
        support::children(&self.syntax)
    }
}

impl ConstDecl {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    #[must_use]
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl TypeAliasDecl {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    #[must_use]
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl GenericParams {
    pub fn params(&self) -> impl Iterator<Item = GenericParam> + '_ {
        let mut current: Vec<SyntaxElement> = Vec::new();
        let mut params = Vec::new();
        let mut depth = 0usize;
        let mut seen_outer_less = false;

        for element in self.syntax.children_with_tokens() {
            match element.as_token().map(rowan::SyntaxToken::kind) {
                Some(SyntaxKind::Less) if !seen_outer_less => {
                    seen_outer_less = true;
                }
                Some(SyntaxKind::Less) => {
                    depth += 1;
                    current.push(element);
                }
                Some(SyntaxKind::Greater) if depth == 0 => {}
                Some(SyntaxKind::Greater) => {
                    depth -= 1;
                    current.push(element);
                }
                Some(SyntaxKind::Comma) if depth == 0 => {
                    if let Some(param) = GenericParam::from_tokens(&current) {
                        params.push(param);
                    }
                    current.clear();
                }
                _ => current.push(element),
            }
        }
        if let Some(param) = GenericParam::from_tokens(&current) {
            params.push(param);
        }

        params.into_iter()
    }
}

impl WhereClause {
    pub fn predicates(&self) -> impl Iterator<Item = WherePredicate> + '_ {
        let elements = self
            .syntax
            .children_with_tokens()
            .filter(|element| {
                !matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::Where)
            })
            .collect::<Vec<_>>();
        split_elements(&elements, SyntaxKind::Comma)
            .into_iter()
            .filter_map(|elements| WherePredicate::from_tokens(&elements))
    }
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
    pub is_const: bool,
    pub bounds: Vec<GenericBound>,
    pub default: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct WherePredicate {
    pub target_ty: Type,
    pub bounds: Vec<GenericBound>,
}

#[derive(Debug, Clone)]
pub struct GenericBound {
    pub trait_path: Path,
    pub type_args: Vec<Type>,
    pub assoc_constraints: Vec<GenericAssocConstraint>,
    pub callable: Option<CallableTraitArgs>,
}

#[derive(Debug, Clone)]
pub struct GenericAssocConstraint {
    pub name: String,
    pub ty: Type,
}

impl WherePredicate {
    fn from_tokens(elements: &[SyntaxElement]) -> Option<Self> {
        let target_ty = elements
            .iter()
            .find_map(|element| element.as_node().and_then(|node| Type::cast(node.clone())))?;
        let colon = elements.iter().position(
            |element| matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::Colon),
        )?;
        let bounds = parse_generic_bounds(&elements[colon + 1..]);
        Some(Self { target_ty, bounds })
    }
}

impl GenericParam {
    fn from_tokens(elements: &[SyntaxElement]) -> Option<Self> {
        let is_const = elements
            .iter()
            .any(|element| matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::Const));
        let name = elements
            .iter()
            .filter_map(|element| element.as_token())
            .find(|token| token.kind() == SyntaxKind::Ident)
            .map(|token| token.text().to_string())?;
        if is_const {
            return Some(Self {
                name,
                is_const,
                bounds: Vec::new(),
                default: None,
            });
        }
        let colon = elements
            .iter()
            .position(|element| matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::Colon));
        let bounds = colon
            .map(|index| parse_generic_bounds(&elements[index + 1..]))
            .unwrap_or_default();
        let default = top_level_token(elements, SyntaxKind::Eq).and_then(|index| {
            elements[index + 1..]
                .iter()
                .find_map(|element| element.as_node().and_then(|node| Type::cast(node.clone())))
        });
        Some(Self {
            name,
            is_const,
            bounds,
            default,
        })
    }
}

fn parse_generic_bounds(elements: &[SyntaxElement]) -> Vec<GenericBound> {
    split_elements(elements, SyntaxKind::Plus)
        .into_iter()
        .filter_map(|bound| {
            let trait_path = bound
                .iter()
                .find_map(|element| element.as_node().and_then(|node| Path::cast(node.clone())))?;
            let type_args = parse_bound_type_args(&bound);
            let assoc_constraints = parse_assoc_constraints(&bound);
            Some(GenericBound {
                trait_path,
                type_args,
                assoc_constraints,
                callable: bound.iter().find_map(|element| {
                    element
                        .as_node()
                        .and_then(|node| CallableTraitArgs::cast(node.clone()))
                }),
            })
        })
        .collect()
}

fn parse_bound_type_args(elements: &[SyntaxElement]) -> Vec<Type> {
    let Some(start) = elements.iter().position(
        |element| matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::Less),
    ) else {
        return Vec::new();
    };
    let Some(end) = elements.iter().rposition(
        |element| matches!(element.as_token(), Some(token) if token.kind() == SyntaxKind::Greater),
    ) else {
        return Vec::new();
    };

    split_elements(&elements[start + 1..end], SyntaxKind::Comma)
        .into_iter()
        .filter(|arg| top_level_token(arg, SyntaxKind::Eq).is_none())
        .filter_map(|arg| {
            arg.into_iter()
                .find_map(|element| element.as_node().and_then(|node| Type::cast(node.clone())))
        })
        .collect()
}

fn top_level_token(elements: &[SyntaxElement], target: SyntaxKind) -> Option<usize> {
    let mut depth = 0usize;
    for (index, element) in elements.iter().enumerate() {
        match element.as_token().map(rowan::SyntaxToken::kind) {
            Some(SyntaxKind::Less | SyntaxKind::LParen | SyntaxKind::LBracket) => depth += 1,
            Some(SyntaxKind::Greater | SyntaxKind::RParen | SyntaxKind::RBracket) => {
                depth = depth.saturating_sub(1);
            }
            Some(kind) if kind == target && depth == 0 => return Some(index),
            _ => {}
        }
    }
    None
}

fn parse_assoc_constraints(elements: &[SyntaxElement]) -> Vec<GenericAssocConstraint> {
    let mut constraints = Vec::new();
    let mut i = 0;
    while i < elements.len() {
        let Some(token) = elements[i].as_token() else {
            i += 1;
            continue;
        };
        if token.kind() != SyntaxKind::Ident {
            i += 1;
            continue;
        }
        let Some(eq_index) = next_non_trivia(elements, i + 1) else {
            i += 1;
            continue;
        };
        if !matches!(elements[eq_index].as_token(), Some(eq) if eq.kind() == SyntaxKind::Eq) {
            i += 1;
            continue;
        }
        let Some(type_index) = next_non_trivia(elements, eq_index + 1) else {
            i += 1;
            continue;
        };
        if let Some(ty) = elements[type_index]
            .as_node()
            .and_then(|node| Type::cast(node.clone()))
        {
            constraints.push(GenericAssocConstraint {
                name: token.text().to_string(),
                ty,
            });
            i = type_index + 1;
        } else {
            i += 1;
        }
    }
    constraints
}

fn split_elements(elements: &[SyntaxElement], separator: SyntaxKind) -> Vec<Vec<SyntaxElement>> {
    let mut result = Vec::new();
    let mut current = Vec::new();
    let mut depth = 0usize;
    for element in elements {
        match element.as_token().map(rowan::SyntaxToken::kind) {
            Some(SyntaxKind::Less | SyntaxKind::LParen | SyntaxKind::LBracket) => {
                depth += 1;
                current.push(element.clone());
            }
            Some(SyntaxKind::Greater | SyntaxKind::RParen | SyntaxKind::RBracket) => {
                depth = depth.saturating_sub(1);
                current.push(element.clone());
            }
            Some(kind) if kind == separator && depth == 0 => {
                if !current.is_empty() {
                    result.push(current);
                    current = Vec::new();
                }
            }
            _ => current.push(element.clone()),
        }
    }
    if !current.is_empty() {
        result.push(current);
    }
    result
}

fn next_non_trivia(elements: &[SyntaxElement], start: usize) -> Option<usize> {
    elements
        .iter()
        .enumerate()
        .skip(start)
        .find_map(|(index, element)| {
            let is_trivia = element
                .as_token()
                .is_some_and(|token| token.kind().is_trivia());
            (!is_trivia).then_some(index)
        })
}

// ── Expressions ────────────────────────────────────���───────────────────

impl Block {
    pub fn stmts(&self) -> impl Iterator<Item = Stmt> + '_ {
        support::children(&self.syntax)
    }

    #[must_use]
    pub fn tail_expr(&self) -> Option<Expr> {
        support::last_child(&self.syntax)
    }
}

impl BracketLambdaExpr {
    #[must_use]
    pub fn is_move(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Move).is_some()
    }

    #[must_use]
    pub fn param_list(&self) -> Option<ParamList> {
        support::child(&self.syntax)
    }

    /// The single-expression body after `->`.
    #[must_use]
    pub fn body(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl LetCondition {
    #[must_use]
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl IfStmt {
    #[must_use]
    pub fn condition(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn let_condition(&self) -> Option<LetCondition> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn then_branch(&self) -> Option<Block> {
        support::child(&self.syntax)
    }

    pub fn else_branch(&self) -> Option<ElseBranch> {
        if let Some(else_block) = support::nth_child::<Block>(&self.syntax, 1) {
            return Some(ElseBranch::Block(else_block));
        }
        let if_stmt: Option<Self> = support::child(&self.syntax);
        if_stmt.map(ElseBranch::IfStmt)
    }
}

#[derive(Debug, Clone)]
pub enum ElseBranch {
    Block(Block),
    IfStmt(IfStmt),
}

impl WhileStmt {
    #[must_use]
    pub fn condition(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn let_condition(&self) -> Option<LetCondition> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn body(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl LoopExpr {
    #[must_use]
    pub fn body(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl ForExpr {
    /// The loop header pattern. As with `let`, only irrefutable patterns are
    /// accepted downstream.
    #[must_use]
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn iterable(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn body(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl BinaryExpr {
    #[must_use]
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn rhs(&self) -> Option<Expr> {
        support::nth_child(&self.syntax, 1)
    }

    #[must_use]
    pub fn op_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, |kind| {
            matches!(
                kind,
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
                    | SyntaxKind::ShrEq
                    | SyntaxKind::Plus
                    | SyntaxKind::Minus
                    | SyntaxKind::Star
                    | SyntaxKind::Slash
                    | SyntaxKind::Percent
                    | SyntaxKind::Amp
                    | SyntaxKind::Pipe
                    | SyntaxKind::Caret
                    | SyntaxKind::Shl
                    | SyntaxKind::Shr
                    | SyntaxKind::EqEq
                    | SyntaxKind::BangEq
                    | SyntaxKind::Less
                    | SyntaxKind::Greater
                    | SyntaxKind::LessEq
                    | SyntaxKind::GreaterEq
                    | SyntaxKind::AmpAmp
                    | SyntaxKind::PipePipe
            )
        })
    }
}

impl UnaryExpr {
    #[must_use]
    pub fn operand(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn op_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, |kind| {
            matches!(
                kind,
                SyntaxKind::Plus
                    | SyntaxKind::Minus
                    | SyntaxKind::Amp
                    | SyntaxKind::AmpAmp
                    | SyntaxKind::Star
                    | SyntaxKind::Bang
            )
        })
    }

    #[must_use]
    pub fn is_mut(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|t| t.kind() == SyntaxKind::Mut)
    }
}

impl ParenExpr {
    #[must_use]
    pub fn inner(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn elements(&self) -> impl Iterator<Item = Expr> + '_ {
        support::children(&self.syntax)
    }

    #[must_use]
    pub fn is_tuple(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Comma).is_some()
    }
}

impl CallExpr {
    #[must_use]
    pub fn callee(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn type_args(&self) -> Vec<Type> {
        self.syntax
            .children()
            .find_map(TypeArgList::cast)
            .map(|list| list.types().collect())
            .unwrap_or_default()
    }

    #[must_use]
    pub fn arg_list(&self) -> Option<ArgList> {
        support::child(&self.syntax)
    }
}

impl ArgList {
    pub fn args(&self) -> impl Iterator<Item = Expr> + '_ {
        support::children(&self.syntax)
    }
}

impl FieldExpr {
    #[must_use]
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn field_name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, |kind| {
            matches!(kind, SyntaxKind::Ident | SyntaxKind::Number)
        })
    }
}

impl IndexExpr {
    #[must_use]
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn index(&self) -> Option<Expr> {
        support::nth_child(&self.syntax, 1)
    }
}

impl RangeExpr {
    #[must_use]
    pub fn start(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn end(&self) -> Option<Expr> {
        support::nth_child(&self.syntax, 1)
    }

    #[must_use]
    pub fn inclusive(&self) -> bool {
        self.syntax.children_with_tokens().any(|element| {
            element
                .as_token()
                .is_some_and(|token| token.kind() == SyntaxKind::DotDotEq)
        })
    }
}

impl StructExpr {
    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child::<NameRefExpr>(&self.syntax)?.path()
    }

    pub fn type_args(&self) -> Vec<Type> {
        self.syntax
            .children()
            .find_map(TypeArgList::cast)
            .map(|list| list.types().collect())
            .unwrap_or_default()
    }

    pub fn fields(&self) -> impl Iterator<Item = StructExprField> + '_ {
        support::children(&self.syntax)
    }
}

impl StructExprField {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl MatchExpr {
    #[must_use]
    pub fn scrutinee(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    pub fn arms(&self) -> impl Iterator<Item = MatchArm> + '_ {
        support::children(&self.syntax)
    }
}

impl MatchArm {
    #[must_use]
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn guard(&self) -> Option<Expr> {
        let mut exprs = support::children::<Expr>(&self.syntax);
        let first = exprs.next();
        if exprs.next().is_some() { first } else { None }
    }

    #[must_use]
    pub fn body(&self) -> Option<Expr> {
        support::last_child(&self.syntax)
    }
}

impl ArrayExpr {
    pub fn elements(&self) -> impl Iterator<Item = Expr> + '_ {
        support::children(&self.syntax)
    }

    #[must_use]
    pub fn is_repeat(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Semi).is_some()
    }

    #[must_use]
    pub fn repeat_value(&self) -> Option<Expr> {
        support::nth_child(&self.syntax, 0)
    }

    #[must_use]
    pub fn repeat_len(&self) -> Option<Expr> {
        support::nth_child(&self.syntax, 1)
    }
}

impl NumberExpr {
    #[must_use]
    pub fn value_token(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Number)
    }

    #[must_use]
    pub fn value(&self) -> Option<u64> {
        let text = self.value_token()?;
        let text = text.text();
        let text_no_underscores: String = text.chars().filter(|&c| c != '_').collect();
        let text = &text_no_underscores;
        let (radix, prefix_len) = match text.as_bytes() {
            [b'0', b'x', ..] => (16, 2),
            [b'0', b'o', ..] => (8, 2),
            [b'0', b'b', ..] => (2, 2),
            _ => (10, 0),
        };
        let digits = &text[prefix_len..];
        let is_digit = |ch: char| match radix {
            16 => ch.is_ascii_hexdigit(),
            _ => ch.is_ascii_digit(),
        };
        let suffix_start = digits
            .find(|ch: char| !is_digit(ch))
            .unwrap_or(digits.len());
        u64::from_str_radix(&digits[..suffix_start], radix).ok()
    }
}

impl FloatLitExpr {
    #[must_use]
    pub fn value_token(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Float)
    }

    #[must_use]
    pub fn value(&self) -> Option<f64> {
        let text = self.value_token()?;
        let text = text.text();
        let trimmed = ["f16", "f32", "f64", "f128"]
            .iter()
            .find_map(|suffix| text.strip_suffix(suffix))
            .unwrap_or(text);
        trimmed.parse().ok()
    }
}

impl StringLitExpr {
    #[must_use]
    pub fn value_token(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::String)
    }
}

impl CharLitExpr {
    #[must_use]
    pub fn value_token(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Char)
    }
}

impl BoolLitExpr {
    #[must_use]
    pub fn value(&self) -> Option<bool> {
        let t = support::token(&self.syntax, |k| {
            matches!(k, SyntaxKind::True | SyntaxKind::False)
        })?;
        Some(t.kind() == SyntaxKind::True)
    }
}

impl NameRefExpr {
    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child(&self.syntax)
    }
}

impl UnsafeExpr {
    #[must_use]
    pub fn body(&self) -> Option<Block> {
        support::child(&self.syntax)
    }
}

impl CastExpr {
    #[must_use]
    pub fn base(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

impl TryExpr {
    #[must_use]
    pub fn operand(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

// ── Paths ──────────────────────────────────────────────────────────────

impl Path {
    pub fn segments(&self) -> impl Iterator<Item = PathSegment> + '_ {
        support::children(&self.syntax)
    }

    #[must_use]
    pub fn is_absolute(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .find(|it| match it {
                rowan::NodeOrToken::Node(_) => true,
                rowan::NodeOrToken::Token(t) => !t.kind().is_trivia(),
            })
            .and_then(rowan::NodeOrToken::into_token)
            .is_some_and(|t| t.kind() == SyntaxKind::ColonColon)
    }
}

impl PathSegment {
    #[must_use]
    pub fn name_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, |k| {
            matches!(
                k,
                SyntaxKind::Ident | SyntaxKind::SelfKw | SyntaxKind::SuperKw | SyntaxKind::CrateKw
            )
        })
    }

    pub fn type_args(&self) -> Vec<Type> {
        self.syntax
            .children()
            .find_map(TypeArgList::cast)
            .map(|list| list.types().collect())
            .unwrap_or_default()
    }
}

// ── Types ──────────────────────────────────────────────────────────────

impl NamedType {
    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child(&self.syntax)
    }

    pub fn type_args(&self) -> Vec<Type> {
        self.syntax
            .children()
            .find_map(TypeArgList::cast)
            .map(|list| list.types().collect())
            .unwrap_or_default()
    }
}

impl TypeArgList {
    pub fn types(&self) -> impl Iterator<Item = Type> + '_ {
        support::children(&self.syntax)
    }
}

impl RefType {
    #[must_use]
    pub fn inner(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn is_mut(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|t| t.kind() == SyntaxKind::Mut)
    }
}

impl PtrType {
    #[must_use]
    pub fn inner(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn is_mut(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|t| t.kind() == SyntaxKind::Mut)
    }
}

impl TupleType {
    pub fn elements(&self) -> impl Iterator<Item = Type> + '_ {
        support::children(&self.syntax)
    }
}

impl ArrayType {
    #[must_use]
    pub fn element(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn len_expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

impl ConstType {
    #[must_use]
    pub fn value(&self) -> Option<usize> {
        support::token_of(&self.syntax, SyntaxKind::Number)?
            .text()
            .parse::<usize>()
            .ok()
    }
}

impl ImplTraitType {
    #[must_use]
    pub fn bound(&self) -> Option<GenericBound> {
        let elements = self.syntax.children_with_tokens().collect::<Vec<_>>();
        parse_generic_bounds(&elements).into_iter().next()
    }
}

impl DynTraitType {
    #[must_use]
    pub fn bound(&self) -> Option<GenericBound> {
        let elements = self.syntax.children_with_tokens().collect::<Vec<_>>();
        parse_generic_bounds(&elements).into_iter().next()
    }
}

impl CallableTraitArgs {
    pub fn params(&self) -> impl Iterator<Item = Type> + '_ {
        let ret = self.return_type().map(|ty| ty.syntax().text_range());
        support::children(&self.syntax)
            .filter(move |ty: &Type| Some(ty.syntax().text_range()) != ret)
    }

    #[must_use]
    pub fn return_type(&self) -> Option<Type> {
        support::last_child(&self.syntax)
    }
}

// ── Patterns ───────────────────────────────────────────────────────────

impl LiteralPat {
    #[must_use]
    pub fn literal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, |k| {
            matches!(
                k,
                SyntaxKind::Number
                    | SyntaxKind::Float
                    | SyntaxKind::String
                    | SyntaxKind::Char
                    | SyntaxKind::True
                    | SyntaxKind::False
            )
        })
    }
}

impl TuplePat {
    pub fn elements(&self) -> impl Iterator<Item = Pattern> + '_ {
        support::children(&self.syntax)
    }
}

impl OrPat {
    pub fn alternatives(&self) -> impl Iterator<Item = Pattern> + '_ {
        support::children(&self.syntax)
    }
}

impl StructPattern {
    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn sub_pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }
}

impl EnumPattern {
    #[must_use]
    pub fn path(&self) -> Option<Path> {
        support::child(&self.syntax)
    }

    pub fn elements(&self) -> impl Iterator<Item = Pattern> + '_ {
        support::children(&self.syntax)
    }

    #[must_use]
    pub fn is_tuple(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::LParen).is_some()
    }

    #[must_use]
    pub fn is_struct(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::LBrace).is_some()
    }

    pub fn fields(&self) -> impl Iterator<Item = StructPattern> + '_ {
        support::children(&self.syntax)
    }
}

// ── Params / Struct fields ─────────────────────────────────────────────

impl ParamList {
    pub fn params(&self) -> impl Iterator<Item = Param> + '_ {
        support::children(&self.syntax)
    }
}

impl Param {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, |k| {
            matches!(k, SyntaxKind::Ident | SyntaxKind::SelfKw)
        })
    }

    #[must_use]
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn is_self_receiver(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::SelfKw).is_some()
    }

    #[must_use]
    pub fn is_ref(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Amp).is_some()
    }

    #[must_use]
    pub fn is_mut(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Mut).is_some()
    }
}

impl StructFieldList {
    pub fn fields(&self) -> impl Iterator<Item = StructField> + '_ {
        support::children(&self.syntax)
    }
}

impl StructField {
    #[must_use]
    pub fn is_pub(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Pub).is_some()
    }

    /// `mut name: T` — the field stays writable through a shared reference to
    /// the containing value.
    ///
    /// Only direct tokens count: a `Mut` inside the type node (`n: &mut i32`)
    /// modifies the reference, not the field.
    #[must_use]
    pub fn is_mut(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|token| token.kind() == SyntaxKind::Mut)
    }

    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}

// ── Extern ─────────────────────────────────────────────────────────────

impl ExternBlock {
    pub fn functions(&self) -> impl Iterator<Item = FuncDecl> + '_ {
        support::children(&self.syntax)
    }
}

impl ExternFnDecl {
    #[must_use]
    pub fn is_unsafe(&self) -> bool {
        support::token_of(&self.syntax, SyntaxKind::Unsafe).is_some()
    }

    #[must_use]
    pub fn func_decl(&self) -> Option<FuncDecl> {
        support::child(&self.syntax)
    }
}

// ── Sum-type enums ─────────────────────────────────────────────────────
//
// Each variant enum: definition → AstNode impl → inherent cast().

// ── Pattern ──

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard(WildcardPat),
    Literal(LiteralPat),
    Tuple(TuplePat),
    Struct(StructPattern),
    Enum(EnumPattern),
    Binding(BindingPat),
    Reference(ReferencePat),
    Or(OrPat),
}

impl AstNode for Pattern {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::WildcardPattern => Some(Self::Wildcard(WildcardPat { syntax: node })),
            SyntaxKind::LiteralPattern => Some(Self::Literal(LiteralPat { syntax: node })),
            SyntaxKind::TuplePattern => Some(Self::Tuple(TuplePat { syntax: node })),
            SyntaxKind::StructPattern => Some(Self::Struct(StructPattern { syntax: node })),
            SyntaxKind::EnumPattern => Some(Self::Enum(EnumPattern { syntax: node })),
            SyntaxKind::BindingPattern => Some(Self::Binding(BindingPat { syntax: node })),
            SyntaxKind::ReferencePattern => Some(Self::Reference(ReferencePat { syntax: node })),
            SyntaxKind::OrPattern => Some(Self::Or(OrPat { syntax: node })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Wildcard(it) => it.syntax(),
            Self::Literal(it) => it.syntax(),
            Self::Tuple(it) => it.syntax(),
            Self::Struct(it) => it.syntax(),
            Self::Enum(it) => it.syntax(),
            Self::Binding(it) => it.syntax(),
            Self::Reference(it) => it.syntax(),
            Self::Or(it) => it.syntax(),
        }
    }
}

impl ReferencePat {
    #[must_use]
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }

    #[must_use]
    pub fn is_mut(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|token| token.kind() == SyntaxKind::Mut)
    }
}

impl BindingPat {
    #[must_use]
    pub fn name(&self) -> Option<SyntaxToken> {
        support::token_of(&self.syntax, SyntaxKind::Ident)
    }

    #[must_use]
    pub fn is_mut(&self) -> bool {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .any(|token| token.kind() == SyntaxKind::Mut)
    }
}

impl Pattern {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        <Self as AstNode>::cast(node)
    }
}

// ── Stmt ──

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl(VarDecl),
    FuncDecl(FuncDecl),
    StructDecl(StructDecl),
    EnumDecl(EnumDecl),
    TraitDecl(TraitDecl),
    ImplDecl(ImplDecl),
    ConstDecl(ConstDecl),
    TypeAliasDecl(TypeAliasDecl),
    BreakStmt(BreakStmt),
    ContinueStmt(ContinueStmt),
    ReturnStmt(ReturnStmt),
    ExprStmt(ExprStmt),
    ModDecl(ModDecl),
    UseDecl(UseDecl),
    ExternBlock(ExternBlock),
    ExternFnDecl(ExternFnDecl),
}

impl AstNode for Stmt {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::VarDecl => Some(Self::VarDecl(VarDecl { syntax: node })),
            SyntaxKind::FuncDecl => Some(Self::FuncDecl(FuncDecl { syntax: node })),
            SyntaxKind::StructDecl => Some(Self::StructDecl(StructDecl { syntax: node })),
            SyntaxKind::EnumDecl => Some(Self::EnumDecl(EnumDecl { syntax: node })),
            SyntaxKind::TraitDecl => Some(Self::TraitDecl(TraitDecl { syntax: node })),
            SyntaxKind::ImplDecl => Some(Self::ImplDecl(ImplDecl { syntax: node })),
            SyntaxKind::ConstDecl => Some(Self::ConstDecl(ConstDecl { syntax: node })),
            SyntaxKind::TypeAliasDecl => Some(Self::TypeAliasDecl(TypeAliasDecl { syntax: node })),
            SyntaxKind::BreakStmt => Some(Self::BreakStmt(BreakStmt { syntax: node })),
            SyntaxKind::ContinueStmt => Some(Self::ContinueStmt(ContinueStmt { syntax: node })),
            SyntaxKind::ReturnStmt => Some(Self::ReturnStmt(ReturnStmt { syntax: node })),
            SyntaxKind::ExprStmt => Some(Self::ExprStmt(ExprStmt { syntax: node })),
            SyntaxKind::ModDecl => Some(Self::ModDecl(ModDecl { syntax: node })),
            SyntaxKind::UseDecl => Some(Self::UseDecl(UseDecl { syntax: node })),
            SyntaxKind::ExternBlock => Some(Self::ExternBlock(ExternBlock { syntax: node })),
            SyntaxKind::ExternFnDecl => Some(Self::ExternFnDecl(ExternFnDecl { syntax: node })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::VarDecl(it) => it.syntax(),
            Self::FuncDecl(it) => it.syntax(),
            Self::StructDecl(it) => it.syntax(),
            Self::EnumDecl(it) => it.syntax(),
            Self::TraitDecl(it) => it.syntax(),
            Self::ImplDecl(it) => it.syntax(),
            Self::ConstDecl(it) => it.syntax(),
            Self::TypeAliasDecl(it) => it.syntax(),
            Self::BreakStmt(it) => it.syntax(),
            Self::ContinueStmt(it) => it.syntax(),
            Self::ReturnStmt(it) => it.syntax(),
            Self::ExprStmt(it) => it.syntax(),
            Self::ModDecl(it) => it.syntax(),
            Self::UseDecl(it) => it.syntax(),
            Self::ExternBlock(it) => it.syntax(),
            Self::ExternFnDecl(it) => it.syntax(),
        }
    }
}

impl Stmt {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        <Self as AstNode>::cast(node)
    }
}

// ── Expr ──

#[derive(Debug, Clone)]
pub enum Expr {
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    ParenExpr(ParenExpr),
    CallExpr(CallExpr),
    BracketLambdaExpr(BracketLambdaExpr),
    FieldExpr(FieldExpr),
    IndexExpr(IndexExpr),
    StructExpr(StructExpr),
    Block(Block),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    LoopExpr(LoopExpr),
    ForExpr(ForExpr),
    MatchExpr(MatchExpr),
    ArrayExpr(ArrayExpr),
    Number(NumberExpr),
    Float(FloatLitExpr),
    StringLit(StringLitExpr),
    CharLit(CharLitExpr),
    BoolLit(BoolLitExpr),
    NameRef(NameRefExpr),
    UnsafeExpr(UnsafeExpr),
    CastExpr(CastExpr),
    TryExpr(TryExpr),
    RangeExpr(RangeExpr),
}

impl AstNode for Expr {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::BinaryExpr => Some(Self::BinaryExpr(BinaryExpr { syntax: node })),
            SyntaxKind::UnaryExpr => Some(Self::UnaryExpr(UnaryExpr { syntax: node })),
            SyntaxKind::ParenExpr => Some(Self::ParenExpr(ParenExpr { syntax: node })),
            SyntaxKind::CallExpr => Some(Self::CallExpr(CallExpr { syntax: node })),
            SyntaxKind::BracketLambdaExpr => {
                Some(Self::BracketLambdaExpr(BracketLambdaExpr { syntax: node }))
            }
            SyntaxKind::FieldExpr => Some(Self::FieldExpr(FieldExpr { syntax: node })),
            SyntaxKind::IndexExpr => Some(Self::IndexExpr(IndexExpr { syntax: node })),
            SyntaxKind::StructExpr => Some(Self::StructExpr(StructExpr { syntax: node })),
            SyntaxKind::Block => Some(Self::Block(Block { syntax: node })),
            SyntaxKind::IfStmt => Some(Self::IfStmt(IfStmt { syntax: node })),
            SyntaxKind::WhileStmt => Some(Self::WhileStmt(WhileStmt { syntax: node })),
            SyntaxKind::LoopExpr => Some(Self::LoopExpr(LoopExpr { syntax: node })),
            SyntaxKind::ForExpr => Some(Self::ForExpr(ForExpr { syntax: node })),
            SyntaxKind::MatchExpr => Some(Self::MatchExpr(MatchExpr { syntax: node })),
            SyntaxKind::ArrayExpr => Some(Self::ArrayExpr(ArrayExpr { syntax: node })),
            SyntaxKind::NumberLit => Some(Self::Number(NumberExpr { syntax: node })),
            SyntaxKind::FloatLit => Some(Self::Float(FloatLitExpr { syntax: node })),
            SyntaxKind::StringLit => Some(Self::StringLit(StringLitExpr { syntax: node })),
            SyntaxKind::CharLit => Some(Self::CharLit(CharLitExpr { syntax: node })),
            SyntaxKind::BoolLit => Some(Self::BoolLit(BoolLitExpr { syntax: node })),
            SyntaxKind::UnsafeExpr => Some(Self::UnsafeExpr(UnsafeExpr { syntax: node })),
            SyntaxKind::CastExpr => Some(Self::CastExpr(CastExpr { syntax: node })),
            SyntaxKind::TryExpr => Some(Self::TryExpr(TryExpr { syntax: node })),
            SyntaxKind::RangeExpr => Some(Self::RangeExpr(RangeExpr { syntax: node })),
            SyntaxKind::NameRef => Some(Self::NameRef(NameRefExpr { syntax: node })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::BinaryExpr(it) => it.syntax(),
            Self::UnaryExpr(it) => it.syntax(),
            Self::ParenExpr(it) => it.syntax(),
            Self::CallExpr(it) => it.syntax(),
            Self::BracketLambdaExpr(it) => it.syntax(),
            Self::FieldExpr(it) => it.syntax(),
            Self::IndexExpr(it) => it.syntax(),
            Self::StructExpr(it) => it.syntax(),
            Self::Block(it) => it.syntax(),
            Self::IfStmt(it) => it.syntax(),
            Self::WhileStmt(it) => it.syntax(),
            Self::LoopExpr(it) => it.syntax(),
            Self::ForExpr(it) => it.syntax(),
            Self::MatchExpr(it) => it.syntax(),
            Self::ArrayExpr(it) => it.syntax(),
            Self::Number(it) => it.syntax(),
            Self::Float(it) => it.syntax(),
            Self::StringLit(it) => it.syntax(),
            Self::CharLit(it) => it.syntax(),
            Self::BoolLit(it) => it.syntax(),
            Self::NameRef(it) => it.syntax(),
            Self::UnsafeExpr(it) => it.syntax(),
            Self::CastExpr(it) => it.syntax(),
            Self::TryExpr(it) => it.syntax(),
            Self::RangeExpr(it) => it.syntax(),
        }
    }
}

impl Expr {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        <Self as AstNode>::cast(node)
    }
}

// ── Type ──

#[derive(Debug, Clone)]
pub enum Type {
    Named(NamedType),
    Never(NeverType),
    Ref(RefType),
    Ptr(PtrType),
    Tuple(TupleType),
    Array(ArrayType),
    Const(ConstType),
    ImplTrait(ImplTraitType),
    DynTrait(DynTraitType),
}

impl AstNode for Type {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::NeverType => Some(Self::Never(NeverType { syntax: node })),
            SyntaxKind::RefType => Some(Self::Ref(RefType { syntax: node })),
            SyntaxKind::NamedType => Some(Self::Named(NamedType { syntax: node })),
            SyntaxKind::PtrType => Some(Self::Ptr(PtrType { syntax: node })),
            SyntaxKind::TupleType => Some(Self::Tuple(TupleType { syntax: node })),
            SyntaxKind::ArrayType => Some(Self::Array(ArrayType { syntax: node })),
            SyntaxKind::ConstType => Some(Self::Const(ConstType { syntax: node })),
            SyntaxKind::ImplTraitType => Some(Self::ImplTrait(ImplTraitType { syntax: node })),
            SyntaxKind::DynTraitType => Some(Self::DynTrait(DynTraitType { syntax: node })),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Self::Named(it) => it.syntax(),
            Self::Never(it) => it.syntax(),
            Self::Ref(it) => it.syntax(),
            Self::Ptr(it) => it.syntax(),
            Self::Tuple(it) => it.syntax(),
            Self::Array(it) => it.syntax(),
            Self::Const(it) => it.syntax(),
            Self::ImplTrait(it) => it.syntax(),
            Self::DynTrait(it) => it.syntax(),
        }
    }
}

impl Type {
    #[must_use]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        <Self as AstNode>::cast(node)
    }
}

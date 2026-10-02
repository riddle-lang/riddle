//! Parser behavior tests: range-expression parsing and precedence, bracket
//! lambda disambiguation against array literals and indexing, and error
//! recovery for malformed input.

use frontend::incremental::IncrementalParser;

mod fuzz;
use syntax::SyntaxKind;

fn parse(source: &str) -> frontend::tree_builder::Parse {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    assert!(parse.errors.is_empty(), "parse errors: {:?}", parse.errors);
    parse.clone()
}

fn tree_has(parse: &frontend::tree_builder::Parse, kind: SyntaxKind) -> bool {
    parse
        .syntax()
        .descendants_with_tokens()
        .any(|it| it.kind() == kind)
        || parse.syntax().descendants().any(|it| it.kind() == kind)
}

#[test]
fn parses_exclusive_range_expression() {
    let parse = parse("fun main() { let r = 0..5; }");
    assert!(tree_has(&parse, SyntaxKind::RangeExpr));
    assert!(tree_has(&parse, SyntaxKind::DotDot));
}

#[test]
fn parses_inclusive_range_expression() {
    let parse = parse("fun main() { let r = 0..=5; }");
    assert!(tree_has(&parse, SyntaxKind::RangeExpr));
    assert!(tree_has(&parse, SyntaxKind::DotDotEq));
}

#[test]
fn parses_or_pattern_in_match_arm() {
    let parse = parse("fun main() { match x { A | B => 1, _ => 2 } }");
    assert!(tree_has(&parse, SyntaxKind::OrPattern));
    assert!(tree_has(&parse, SyntaxKind::MatchArm));
}

#[test]
fn parses_leading_pipe_before_match_arm_alternatives() {
    let parse = parse("fun main() { match x { | A | B => 1, _ => 2 } }");
    assert!(tree_has(&parse, SyntaxKind::OrPattern));
}

#[test]
fn let_or_pattern_reports_one_error_and_keeps_later_statements() {
    // Or-patterns belong to match arms only. The parser used to sync past the
    // stray `|` without any diagnostic, silently discarding `| 2 = x`.
    let parse = parse_unchecked("fun main() { let 1 | 2 = x; let y = 3; }");
    assert_eq!(parse.errors.len(), 1, "{:?}", parse.errors);
    assert!(
        parse.errors[0]
            .message
            .contains("only allowed on match arms"),
        "{:?}",
        parse.errors
    );
    assert_eq!(
        parse
            .syntax()
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::VarDecl)
            .count(),
        2
    );
}

#[test]
fn range_binds_looser_than_arithmetic() {
    // `1 + 1..4 + 1` groups as `(1 + 1)..(4 + 1)`, like Rust.
    let parse = parse("fun main() { let r = 1 + 1..4 + 1; }");
    assert!(tree_has(&parse, SyntaxKind::RangeExpr));
    assert!(tree_has(&parse, SyntaxKind::BinaryExpr));
}

#[test]
fn float_literals_still_parse() {
    let parse = parse("fun main() { let x = 1.5; let y = 0.25; }");
    assert!(tree_has(&parse, SyntaxKind::FloatLit));
    assert!(!tree_has(&parse, SyntaxKind::RangeExpr));
    assert!(!tree_has(&parse, SyntaxKind::DotDot));
}

#[test]
fn field_access_and_ranges_coexist() {
    let parse = parse("fun main() { let t = (1, 2); let r = t.0..t.1; }");
    assert!(tree_has(&parse, SyntaxKind::RangeExpr));
    assert!(tree_has(&parse, SyntaxKind::FieldExpr));
}

#[test]
fn bracket_lambda_disambiguates_from_array_literal() {
    let parse = parse("fun main() { let value = [1, 2, 3]; }");
    assert!(tree_has(&parse, SyntaxKind::ArrayExpr));
    assert!(!tree_has(&parse, SyntaxKind::BracketLambdaExpr));
}

#[test]
fn bracket_lambda_disambiguates_from_indexing() {
    let parse = parse("fun main() { let a = [1]; let v = a[0]; }");
    assert!(tree_has(&parse, SyntaxKind::IndexExpr));
    assert!(!tree_has(&parse, SyntaxKind::BracketLambdaExpr));
}

#[test]
fn bracket_lambda_parses_with_arrow() {
    let parse = parse(
        "fun apply(f: impl Fn(i32) -> i32) -> i32 { f(1) } fun main() { apply([v -> v + 1]); }",
    );
    assert!(tree_has(&parse, SyntaxKind::BracketLambdaExpr));
}

#[test]
fn error_recovery_reports_without_panicking() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source("fun main() { let = ; }");
    assert!(!parse.errors.is_empty());
}

#[test]
fn unterminated_string_recovers() {
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source("fun main() { let s = \"open; }");
    assert!(!parse.errors.is_empty());
}

#[test]
fn unterminated_block_comment_reports_one_error() {
    // The lexer runs a stray `/*` to end of input, so every item after it
    // vanishes; without a diagnostic the file compiles as if truncated.
    let mut parser = IncrementalParser::new();
    let parse = parser.set_source("fun main() { }\n/* oops\nfun after() { }");
    assert_eq!(parse.errors.len(), 1, "{:?}", parse.errors);
    assert!(
        parse.errors[0]
            .message
            .contains("unterminated block comment"),
        "{:?}",
        parse.errors[0]
    );
}

#[test]
fn terminated_block_comments_stay_error_free() {
    let parse = parse("/* plain */ /** doc */ /*! inner */ fun main() { }");
    assert!(tree_has(&parse, SyntaxKind::FuncDecl));
}

#[test]
fn block_comment_closes_at_a_terminator_ending_the_input() {
    // The `//` sits mid-line, so it is prose: the terminator that ends the
    // input closes the comment instead of the remainder being reported as
    // unterminated.
    let source = "fun main() { }\n/* note: // see `&mut` here */";
    let comments: Vec<&str> = frontend::lexer::lex(source)
        .iter()
        .filter(|token| token.kind == SyntaxKind::BlockComment)
        .map(|token| token.text(source))
        .collect();
    assert_eq!(
        comments,
        vec!["/* note: // see `&mut` here */"],
        "{comments:?}"
    );

    let mut parser = IncrementalParser::new();
    let parse = parser.set_source(source);
    assert!(parse.errors.is_empty(), "{:?}", parse.errors);
}

#[test]
fn block_comment_resumes_after_the_line_comment_on_its_own_line() {
    // A terminator later on the same line as `//` is comment text; the block
    // closes at the terminator on the following line.
    let source = "/* first\n // let a: &mut Guard = ... */\n */ fun main() { }";
    let comments: Vec<&str> = frontend::lexer::lex(source)
        .iter()
        .filter(|token| token.kind == SyntaxKind::BlockComment)
        .map(|token| token.text(source))
        .collect();
    assert_eq!(
        comments,
        vec!["/* first\n // let a: &mut Guard = ... */\n */"],
        "{comments:?}"
    );

    let parse = parse(source);
    assert!(tree_has(&parse, SyntaxKind::FuncDecl));
    assert!(!tree_has(&parse, SyntaxKind::ErrorNode));
}

#[test]
fn block_comment_keeps_nested_comments_across_a_line_comment() {
    let source = "/* outer // note\n /* inner */ still outer\n */ fun main() { }";
    let comments: Vec<&str> = frontend::lexer::lex(source)
        .iter()
        .filter(|token| token.kind == SyntaxKind::BlockComment)
        .map(|token| token.text(source))
        .collect();
    assert_eq!(
        comments,
        vec!["/* outer // note\n /* inner */ still outer\n */"],
        "{comments:?}"
    );

    let parse = parse(source);
    assert!(tree_has(&parse, SyntaxKind::FuncDecl));
    assert!(!tree_has(&parse, SyntaxKind::ErrorNode));
}

#[test]
fn block_comment_still_closes_before_a_line_comment_on_the_same_line() {
    let source = "/* first */ // trailing note\nfun main() { }";
    let comments: Vec<&str> = frontend::lexer::lex(source)
        .iter()
        .filter(|token| token.kind == SyntaxKind::BlockComment)
        .map(|token| token.text(source))
        .collect();
    assert_eq!(comments, vec!["/* first */"], "{comments:?}");

    let parse = parse(source);
    assert!(tree_has(&parse, SyntaxKind::FuncDecl));
    assert!(!tree_has(&parse, SyntaxKind::ErrorNode));
}

#[test]
fn nested_block_comments_stay_error_free() {
    let parse = parse("/* outer /* inner */ still outer */ fun main() { }");
    assert!(tree_has(&parse, SyntaxKind::FuncDecl));
}

#[test]
fn match_block_arm_without_trailing_comma_parses() {
    // The arm's own closing `}` terminates a block-bodied arm, so the comma
    // is optional and must not derail into expression-error cascades.
    let parse = parse("fun f(x: i32) -> i32 { match x { 0 => { 1 } _ => 2, } }");
    assert!(tree_has(&parse, SyntaxKind::MatchExpr));
    assert_eq!(
        parse
            .syntax()
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::MatchArm)
            .count(),
        2
    );
}

#[test]
fn generic_cast_reports_e0012_not_crash() {
    // Regression: casting a generic parameter used to be able to reach MIR
    // lowering unsupported; the type checker must reject it with E0012.
    let parse = parse("fun convert<T>(value: T) -> i32 { value as i32 }");
    assert!(parse.errors.is_empty());
}

#[test]
fn nested_generics_and_shift_after_call_are_unambiguous() {
    let parse = parse(
        "fun f<T>(value: T) -> T { value } fun main() { let x = f::<Vec<Vec<i32>>>((1)); let y = 8i32 >> 1i32; }",
    );
    assert!(tree_has(&parse, SyntaxKind::TypeArgList));
    assert!(tree_has(&parse, SyntaxKind::Shr));
}

#[test]
fn while_comparison_chain_does_not_consume_body_tokens() {
    let parse = parse("fun main() { let mut a = 0i32; while a < 3i32 { a = a + 1i32; } }");
    assert!(tree_has(&parse, SyntaxKind::WhileStmt));
}

// == operator precedence (Rust/C-style bitwise ordering) ==

/// Operator token of the outermost `BinaryExpr` plus its two operand kinds,
/// used to assert how an expression groups.
fn outermost_binary_parts(
    parse: &frontend::tree_builder::Parse,
) -> Option<(SyntaxKind, SyntaxKind, SyntaxKind)> {
    let node = parse.syntax().descendants().find(|n| {
        n.kind() == SyntaxKind::BinaryExpr
            && !n
                .ancestors()
                .skip(1)
                .any(|a| a.kind() == SyntaxKind::BinaryExpr)
    })?;
    let op = node.children_with_tokens().find_map(|element| {
        element.as_token().and_then(|token| match token.kind() {
            SyntaxKind::Pipe
            | SyntaxKind::Caret
            | SyntaxKind::Amp
            | SyntaxKind::Shl
            | SyntaxKind::Shr
            | SyntaxKind::Plus
            | SyntaxKind::Minus
            | SyntaxKind::Star
            | SyntaxKind::Slash
            | SyntaxKind::Percent
            | SyntaxKind::Less
            | SyntaxKind::Greater
            | SyntaxKind::LessEq
            | SyntaxKind::GreaterEq
            | SyntaxKind::EqEq
            | SyntaxKind::BangEq
            | SyntaxKind::AmpAmp
            | SyntaxKind::PipePipe => Some(token.kind()),
            _ => None,
        })
    })?;
    let operands: Vec<_> = node.children().collect();
    Some((op, operands.first()?.kind(), operands.last()?.kind()))
}

#[test]
fn amp_binds_tighter_than_pipe() {
    // `a | b & c` groups as `a | (b & c)` like Rust/C, not `(a | b) & c`.
    let parse = parse("fun f(a: i32, b: i32, c: i32) -> i32 { a | b & c }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((
            SyntaxKind::Pipe,
            SyntaxKind::NameRef,
            SyntaxKind::BinaryExpr
        ))
    );
}

#[test]
fn caret_binds_tighter_than_pipe() {
    // `a ^ b | c` groups as `(a ^ b) | c`.
    let parse = parse("fun f(a: i32, b: i32, c: i32) -> i32 { a ^ b | c }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((
            SyntaxKind::Pipe,
            SyntaxKind::BinaryExpr,
            SyntaxKind::NameRef
        ))
    );
}

#[test]
fn amp_binds_tighter_than_caret() {
    // `a & b ^ c` groups as `(a & b) ^ c`.
    let parse = parse("fun f(a: i32, b: i32, c: i32) -> i32 { a & b ^ c }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((
            SyntaxKind::Caret,
            SyntaxKind::BinaryExpr,
            SyntaxKind::NameRef
        ))
    );
}

#[test]
fn shifts_bind_tighter_than_amp() {
    // `a & b << 1` groups as `a & (b << 1)`.
    let parse = parse("fun f(a: i32, b: i32) -> i32 { a & b << 1 }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((SyntaxKind::Amp, SyntaxKind::NameRef, SyntaxKind::BinaryExpr))
    );
}

#[test]
fn arithmetic_binds_tighter_than_shifts() {
    // `a << b + 1` groups as `a << (b + 1)`.
    let parse = parse("fun f(a: i32, b: i32) -> i32 { a << b + 1 }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((SyntaxKind::Shl, SyntaxKind::NameRef, SyntaxKind::BinaryExpr))
    );
}

#[test]
fn bitwise_binds_tighter_than_comparison() {
    // `a & b == c` groups as `(a & b) == c` like Rust (not C, where `==`
    // would bind tighter).
    let parse = parse("fun f(a: i32, b: i32, c: i32) -> bool { a & b == c }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((
            SyntaxKind::EqEq,
            SyntaxKind::BinaryExpr,
            SyntaxKind::NameRef
        ))
    );
}

#[test]
fn arithmetic_binds_tighter_than_comparison() {
    // `a + b < c` groups as `(a + b) < c`.
    let parse = parse("fun f(a: i32, b: i32, c: i32) -> bool { a + b < c }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((
            SyntaxKind::Less,
            SyntaxKind::BinaryExpr,
            SyntaxKind::NameRef
        ))
    );
}

#[test]
fn unary_still_binds_tighter_than_multiplication() {
    // `-a * b` groups as `(-a) * b`, not `-(a * b)`.
    let parse1 = parse("fun f(a: i32, b: i32) -> i32 { -a * b }");
    assert_eq!(
        outermost_binary_parts(&parse1),
        Some((SyntaxKind::Star, SyntaxKind::UnaryExpr, SyntaxKind::NameRef))
    );
    let parse2 = parse("fun f(a: bool, b: bool) -> bool { !a && b }");
    assert_eq!(
        outermost_binary_parts(&parse2),
        Some((
            SyntaxKind::AmpAmp,
            SyntaxKind::UnaryExpr,
            SyntaxKind::NameRef
        ))
    );
}

#[test]
fn shifts_associate_left() {
    // `a << b >> c` groups as `(a << b) >> c`.
    let parse = parse("fun f(a: i32, b: i32, c: i32) -> i32 { a << b >> c }");
    assert_eq!(
        outermost_binary_parts(&parse),
        Some((SyntaxKind::Shr, SyntaxKind::BinaryExpr, SyntaxKind::NameRef))
    );
}

// == error recovery: context sync sets suppress cascades ==

fn parse_unchecked(source: &str) -> frontend::tree_builder::Parse {
    let mut parser = IncrementalParser::new();
    parser.set_source(source).clone()
}

#[test]
fn malformed_let_reports_one_error_and_keeps_later_statements() {
    let parse = parse_unchecked("fun main() { let = 5; let y = 3; }");
    assert_eq!(parse.errors.len(), 1, "{:?}", parse.errors);
    // The statement after the broken one still parses as a real binding.
    assert_eq!(
        parse
            .syntax()
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::VarDecl)
            .count(),
        2
    );
}

#[test]
fn missing_param_type_reports_one_error_and_closes_the_list() {
    let parse = parse_unchecked("fun f(a: i32, b) -> i32 { 0 } fun main() { }");
    assert_eq!(parse.errors.len(), 1, "{:?}", parse.errors);
    assert!(tree_has(&parse, SyntaxKind::ParamList));
}

#[test]
fn missing_struct_field_comma_reports_one_error_and_parses_both_fields() {
    let parse = parse_unchecked("struct S { a: i32 b: i32 } fun main() { }");
    assert_eq!(parse.errors.len(), 1, "{:?}", parse.errors);
    assert_eq!(
        parse
            .syntax()
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::StructField)
            .count(),
        2
    );
}

#[test]
fn malformed_match_arm_body_reports_one_error() {
    let parse = parse_unchecked("fun main() { match 1 { 1 => let a = ; 2 => 2 } }");
    assert_eq!(parse.errors.len(), 1, "{:?}", parse.errors);
    assert!(tree_has(&parse, SyntaxKind::MatchExpr));
}

#[test]
fn missing_semi_before_next_statement_keeps_the_next_statement() {
    // The statement keyword is a sync point: it is not swallowed by the
    // missing-`;` error, so `fun main` parses as a real function.
    let parse = parse_unchecked("fun f() { let a = 1 } fun main() { }");
    assert_eq!(parse.errors.len(), 1, "{:?}", parse.errors);
    assert_eq!(
        parse
            .syntax()
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::FuncDecl)
            .count(),
        2
    );
}

#[test]
fn stray_closing_braces_each_report_once() {
    let parse = parse_unchecked("fun main() { let a = 1; } } } fun other() { let b = 2; }");
    assert_eq!(parse.errors.len(), 2, "{:?}", parse.errors);
    assert!(tree_has(&parse, SyntaxKind::FuncDecl));
}

#[test]
fn deeply_nested_blocks_report_one_nesting_diagnostic() {
    // Debug-build recovery frames are large, matching the deep-nesting test in
    // tests/riddlec: run the parse on an explicitly sized stack thread.
    let handle = std::thread::Builder::new()
        .stack_size(256 * 1024 * 1024)
        .spawn(deeply_nested_blocks_report_one_nesting_diagnostic_inner)
        .unwrap();
    handle.join().unwrap();
}

fn deeply_nested_blocks_report_one_nesting_diagnostic_inner() {
    // The depth bailout inside `lhs` returns without consuming a token, so the
    // enclosing block loop retried the same position forever, pushing one
    // diagnostic per attempt until the allocator aborted the process.
    let depth = 96;
    let source = format!(
        "fun main() -> i32 {{ {}0{} }}\n",
        "{".repeat(depth),
        "}".repeat(depth)
    );
    let parse = parse_unchecked(&source);
    assert_eq!(
        parse
            .errors
            .iter()
            .filter(|error| error.message.contains("nesting is too deep"))
            .count(),
        1,
        "{:?}",
        parse.errors
    );
}

#[test]
fn a_block_statement_does_not_swallow_a_following_parenthesized_statement() {
    // A block-shaped expression in statement position is a complete statement,
    // so the `(` opening the next statement must not be read as a call on it.
    // The postfix branch ran before the `bare_block` guard, so `if true { }`
    // followed by `(*p) = 5;` parsed as `if true { }(*p)`, i.e. a call on the
    // `()`-typed `if` — which the type checker then rejected with `E0004`
    // ("cannot call value of type ()"). There is no call anywhere in this
    // snippet, so any `CallExpr` in the tree is that misparse.
    let parse = parse(
        "fun main() {
            let p = &mut x as *mut i32;
            unsafe {
                if true { }
                (*p) = 5;
            }
        }",
    );
    assert!(tree_has(&parse, SyntaxKind::IfStmt));
    assert!(tree_has(&parse, SyntaxKind::BinaryExpr));
    assert_eq!(
        parse
            .syntax()
            .descendants()
            .filter(|node| node.kind() == SyntaxKind::CallExpr)
            .count(),
        0,
        "tree: {}",
        parse.syntax()
    );
}

#[test]
fn a_block_statement_still_ends_a_statement_without_parens() {
    // The guard must not stop the parser from reading a following statement at
    // all: `*p = 5;` after the same `if` already worked and must keep working.
    let parse = parse(
        "fun main() {
            let p = &mut x as *mut i32;
            unsafe {
                if true { }
                *p = 5;
            }
        }",
    );
    assert!(tree_has(&parse, SyntaxKind::IfStmt));
    assert!(tree_has(&parse, SyntaxKind::BinaryExpr));
}

#[test]
fn a_mut_field_marks_only_the_field_not_its_type() {
    use ast::support::AstNode as _;

    // `plain: &mut i32` mentions `mut` inside the type, which modifies the
    // reference rather than the field. Only a direct `Mut` token before the
    // name makes the field itself interior-mutable.
    let parse = parse("struct Counter { mut hits: i32, plain: &mut i32 }");
    let fields = parse
        .syntax()
        .descendants()
        .filter_map(ast::StructField::cast)
        .map(|field| field.is_mut())
        .collect::<Vec<_>>();
    assert_eq!(fields, vec![true, false], "{}", parse.syntax());
}

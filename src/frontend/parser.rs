use pest::Parser;
use pest_derive::Parser;
use crate::frontend::ast::{AstNode, Literal, FuncParam, StructField, EnumVariant};
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RiddleParser;

pub fn parse(input: &str) -> Result<AstNode, pest::error::Error<Rule>> {
    let pairs = RiddleParser::parse(Rule::program, input)?;
    Ok(parse_pair(pairs.into_iter().next().unwrap()))
}

fn parse_pair(pair: Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::program => {
            let stmts = pair.into_inner()
                .filter(|p| p.as_rule() != Rule::EOI)
                .map(parse_pair)
                .collect();
            AstNode::Program(stmts)
        }
        Rule::var_decl => {
            let mut name = String::new();
            let mut type_expr = None;
            let mut value = None;
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::identifier => name = p.as_str().to_string(),
                    Rule::type_expr => type_expr = Some(Box::new(parse_pair(p.into_inner().next().unwrap()))),
                    Rule::value_expr => value = Some(Box::new(parse_pair(p.into_inner().next().unwrap()))),
                    _ => unreachable!(),
                }
            }
            AstNode::VarDecl { name, type_expr, value }
        }
        Rule::func_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut params = Vec::new();
            let mut return_type = None;
            let mut body = None;

            for p in inner {
                match p.as_rule() {
                    Rule::func_param => {
                        let mut p_inner = p.into_inner();
                        let p_name = p_inner.next().unwrap().as_str().to_string();
                        let p_type = Box::new(parse_pair(p_inner.next().unwrap().into_inner().next().unwrap()));
                        params.push(FuncParam { name: p_name, type_expr: p_type });
                    }
                    Rule::type_expr => return_type = Some(Box::new(parse_pair(p.into_inner().next().unwrap()))),
                    Rule::block => body = Some(Box::new(parse_pair(p))),
                    _ => unreachable!(),
                }
            }
            AstNode::FuncDecl { name, params, return_type, body: body.unwrap() }
        }
        Rule::struct_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut fields = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::struct_field => {
                        let mut f_inner = p.into_inner();
                        let f_name = f_inner.next().unwrap().as_str().to_string();
                        let f_type = Box::new(parse_pair(f_inner.next().unwrap().into_inner().next().unwrap()));
                        fields.push(StructField { name: f_name, type_expr: f_type });
                    }
                    _ => unreachable!(),
                }
            }
            AstNode::StructDecl { name, fields }
        }
        Rule::enum_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut variants = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::enum_variant => {
                        let mut v_inner = p.into_inner();
                        let v_name = v_inner.next().unwrap().as_str().to_string();
                        if let Some(suffix) = v_inner.next() {
                            match suffix.as_rule() {
                                Rule::enum_tuple => {
                                    let args = suffix.into_inner().map(parse_pair).collect();
                                    variants.push(EnumVariant::Tuple(v_name, args));
                                }
                                Rule::enum_struct => {
                                    let fields = suffix.into_inner().map(|f| {
                                        let mut f_inner = f.into_inner();
                                        let f_name = f_inner.next().unwrap().as_str().to_string();
                                        let f_type = Box::new(parse_pair(f_inner.next().unwrap().into_inner().next().unwrap()));
                                        StructField { name: f_name, type_expr: f_type }
                                    }).collect();
                                    variants.push(EnumVariant::Struct(v_name, fields));
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            variants.push(EnumVariant::Unit(v_name));
                        }
                    }
                    _ => unreachable!(),
                }
            }
            AstNode::EnumDecl { name, variants }
        }
        Rule::block => {
            let mut inner = pair.into_inner().peekable();
            let mut statements = Vec::new();
            let mut final_expr = None;
            while let Some(p) = inner.next() {
                if p.as_rule() == Rule::binary_expr && inner.peek().is_none() {
                    final_expr = Some(Box::new(parse_pair(p)));
                } else {
                    statements.push(parse_pair(p));
                }
            }
            AstNode::Block { statements, final_expr }
        }
        Rule::expr_stmt => {
            AstNode::ExprStmt(Box::new(parse_pair(pair.into_inner().next().unwrap())))
        }
        Rule::binary_expr => {
            let mut inner = pair.into_inner();
            let mut lhs = parse_pair(inner.next().unwrap());
            while let Some(op_pair) = inner.next() {
                let op = op_pair.as_str().to_string();
                let rhs = parse_pair(inner.next().unwrap());
                lhs = AstNode::BinaryExpr {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                };
            }
            lhs
        }
        Rule::postfix_expr => {
            let mut inner = pair.into_inner();
            let base = Box::new(parse_pair(inner.next().unwrap()));
            let suffixes = inner.map(parse_pair).collect::<Vec<_>>();
            if suffixes.is_empty() {
                *base
            } else {
                AstNode::PostfixExpr { base, suffixes }
            }
        }
        Rule::call_suffix => {
            let args = pair.into_inner().map(parse_pair).collect();
            AstNode::CallSuffix(args)
        }
        Rule::identifier => AstNode::Identifier(pair.as_str().to_string()),
        Rule::bool => AstNode::Literal(Literal::Bool(pair.as_str().parse().unwrap())),
        Rule::number => AstNode::Literal(Literal::Int(pair.as_str().parse().unwrap())),
        Rule::float => AstNode::Literal(Literal::Float(pair.as_str().parse().unwrap())),
        Rule::string => {
            let mut inner = pair.into_inner();
            let content = inner.next().map(|p| p.as_str().to_string()).unwrap_or_default();
            AstNode::Literal(Literal::Str(content))
        }
        _ => todo!("Rule {:?} not implemented", pair.as_rule()),
    }
}
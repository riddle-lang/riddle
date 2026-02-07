use crate::error::{Result, RiddleError, Span};
use crate::frontend::ast::{
    AstNode, AstNodeKind, EnumVariant, ExternFunc, FuncParam, Literal, StructField, TraitItem,
};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RiddleParser;

pub fn parse(input: &str) -> Result<AstNode> {
    let pairs = RiddleParser::parse(Rule::program, input)
        .map_err(|e| RiddleError::Parse(format!("{}", e), None))?;
    parse_pair(
        pairs
            .into_iter()
            .next()
            .ok_or_else(|| RiddleError::Parse("Empty program".to_string(), None))?,
    )
}

fn get_span(pair: &Pair<Rule>) -> Span {
    Span {
        start: pair.as_span().start(),
        end: pair.as_span().end(),
    }
}

fn parse_extern_item(pair: Pair<Rule>) -> Result<ExternFunc> {
    let mut inner = pair.into_inner();
    let name = inner
        .next()
        .ok_or_else(|| RiddleError::Parse("Extern item missing name".to_string(), None))?
        .as_str()
        .to_string();
    let mut params = Vec::new();
    let mut return_type = None;
    let mut is_variadic = false;
    for p in inner {
        match p.as_rule() {
            Rule::func_param => {
                let mut p_inner = p.into_inner();
                let p_name = p_inner
                    .next()
                    .ok_or_else(|| RiddleError::Parse("Param missing name".to_string(), None))?
                    .as_str()
                    .to_string();
                let p_type =
                    Box::new(parse_pair(p_inner.next().ok_or_else(|| {
                        RiddleError::Parse("Param missing type".to_string(), None)
                    })?)?);
                params.push(FuncParam {
                    name: p_name,
                    type_expr: p_type,
                });
            }
            Rule::variadic_marker => {
                is_variadic = true;
            }
            Rule::type_expr => return_type = Some(Box::new(parse_pair(p)?)),
            _ => unreachable!(),
        }
    }
    Ok(ExternFunc {
        name,
        params,
        return_type,
        is_variadic,
    })
}

fn parse_generic_params(pair: Pair<Rule>) -> Vec<String> {
    pair.into_inner().map(|p| p.as_str().to_string()).collect()
}

fn parse_generic_args(pair: Pair<Rule>) -> Result<Vec<AstNode>> {
    pair.into_inner()
        .map(parse_pair)
        .collect::<Result<Vec<_>>>()
}

fn parse_pair(pair: Pair<Rule>) -> Result<AstNode> {
    let span = get_span(&pair);
    let kind = match pair.as_rule() {
        Rule::program => {
            let stmts = pair
                .into_inner()
                .filter(|p| p.as_rule() != Rule::EOI)
                .map(parse_pair)
                .collect::<Result<Vec<_>>>()?;
            AstNodeKind::Program(stmts)
        }
        Rule::var_decl => {
            let mut name = String::new();
            let mut type_expr = None;
            let mut value = None;
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::identifier => name = p.as_str().to_string(),
                    Rule::type_expr => type_expr = Some(Box::new(parse_pair(p)?)),
                    Rule::value_expr => value = Some(Box::new(parse_pair(p)?)),
                    _ => unreachable!(),
                }
            }
            AstNodeKind::VarDecl {
                name,
                type_expr,
                value,
            }
        }
        Rule::func_decl => {
            let mut inner = pair.into_inner();
            let name = inner
                .next()
                .ok_or_else(|| RiddleError::Parse("Function missing name".to_string(), Some(span)))?
                .as_str()
                .to_string();
            let mut generic_params = Vec::new();
            let mut params = Vec::new();
            let mut return_type = None;
            let mut body = None;

            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::func_param => {
                        let mut p_inner = p.clone().into_inner();
                        let p_name = p_inner
                            .next()
                            .ok_or_else(|| {
                                RiddleError::Parse(
                                    "Param missing name".to_string(),
                                    Some(get_span(&p)),
                                )
                            })?
                            .as_str()
                            .to_string();
                        let p_type = Box::new(parse_pair(p_inner.next().ok_or_else(|| {
                            RiddleError::Parse("Param missing type".to_string(), Some(get_span(&p)))
                        })?)?);
                        params.push(FuncParam {
                            name: p_name,
                            type_expr: p_type,
                        });
                    }
                    Rule::type_expr => return_type = Some(Box::new(parse_pair(p)?)),
                    Rule::block => body = Some(Box::new(parse_pair(p)?)),
                    _ => unreachable!(),
                }
            }
            let func_name = name.clone();
            AstNodeKind::FuncDecl {
                name,
                generic_params,
                params,
                return_type,
                body: body.ok_or_else(|| {
                    RiddleError::Parse(format!("Function {} missing body", func_name), Some(span))
                })?,
            }
        }
        Rule::struct_decl => {
            let mut inner = pair.into_inner();
            let name = inner
                .next()
                .ok_or_else(|| RiddleError::Parse("Struct missing name".to_string(), Some(span)))?
                .as_str()
                .to_string();
            let mut generic_params = Vec::new();
            let mut fields = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::struct_field => {
                        let mut f_inner = p.clone().into_inner();
                        let f_name = f_inner
                            .next()
                            .ok_or_else(|| {
                                RiddleError::Parse(
                                    "Field missing name".to_string(),
                                    Some(get_span(&p)),
                                )
                            })?
                            .as_str()
                            .to_string();
                        let f_type = Box::new(parse_pair(f_inner.next().ok_or_else(|| {
                            RiddleError::Parse("Field missing type".to_string(), Some(get_span(&p)))
                        })?)?);
                        fields.push(StructField {
                            name: f_name,
                            type_expr: f_type,
                        });
                    }
                    _ => unreachable!(),
                }
            }
            AstNodeKind::StructDecl {
                name,
                generic_params,
                fields,
            }
        }
        Rule::enum_decl => {
            let mut inner = pair.into_inner();
            let name = inner
                .next()
                .ok_or_else(|| RiddleError::Parse("Enum missing name".to_string(), Some(span)))?
                .as_str()
                .to_string();
            let mut generic_params = Vec::new();
            let mut variants = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::enum_variant => {
                        let mut v_inner = p.clone().into_inner();
                        let v_name = v_inner
                            .next()
                            .ok_or_else(|| {
                                RiddleError::Parse(
                                    "Variant missing name".to_string(),
                                    Some(get_span(&p)),
                                )
                            })?
                            .as_str()
                            .to_string();
                        if let Some(suffix) = v_inner.next() {
                            match suffix.as_rule() {
                                Rule::enum_tuple => {
                                    let args = suffix
                                        .into_inner()
                                        .map(parse_pair)
                                        .collect::<Result<Vec<_>>>()?;
                                    variants.push(EnumVariant::Tuple(v_name, args));
                                }
                                Rule::enum_struct => {
                                    let fields = suffix
                                        .into_inner()
                                        .map(|f| {
                                            let mut f_inner = f.clone().into_inner();
                                            let f_name = f_inner
                                                .next()
                                                .ok_or_else(|| {
                                                    RiddleError::Parse(
                                                        "Field missing name".to_string(),
                                                        Some(get_span(&f)),
                                                    )
                                                })?
                                                .as_str()
                                                .to_string();
                                            let f_type = Box::new(parse_pair(
                                                f_inner.next().ok_or_else(|| {
                                                    RiddleError::Parse(
                                                        "Field missing type".to_string(),
                                                        Some(get_span(&f)),
                                                    )
                                                })?,
                                            )?);
                                            Ok(StructField {
                                                name: f_name,
                                                type_expr: f_type,
                                            })
                                        })
                                        .collect::<Result<Vec<_>>>()?;
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
            AstNodeKind::EnumDecl {
                name,
                generic_params,
                variants,
            }
        }
        Rule::trait_decl => {
            let mut inner = pair.into_inner();
            let name = inner
                .next()
                .ok_or_else(|| RiddleError::Parse("Trait missing name".to_string(), Some(span)))?
                .as_str()
                .to_string();
            let mut generic_params = Vec::new();
            let mut items = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::trait_item => {
                        let mut item_inner = p.clone().into_inner();
                        let item_name = item_inner
                            .next()
                            .ok_or_else(|| {
                                RiddleError::Parse(
                                    "Trait item missing name".to_string(),
                                    Some(get_span(&p)),
                                )
                            })?
                            .as_str()
                            .to_string();
                        let mut item_generic_params = Vec::new();
                        let mut params = Vec::new();
                        let mut return_type = None;
                        for param_p in item_inner {
                            match param_p.as_rule() {
                                Rule::generic_params => {
                                    item_generic_params = parse_generic_params(param_p)
                                }
                                Rule::func_param => {
                                    let mut p_inner = param_p.clone().into_inner();
                                    let p_name = p_inner
                                        .next()
                                        .ok_or_else(|| {
                                            RiddleError::Parse(
                                                "Param missing name".to_string(),
                                                Some(get_span(&param_p)),
                                            )
                                        })?
                                        .as_str()
                                        .to_string();
                                    let p_type =
                                        Box::new(parse_pair(p_inner.next().ok_or_else(|| {
                                            RiddleError::Parse(
                                                "Param missing type".to_string(),
                                                Some(get_span(&param_p)),
                                            )
                                        })?)?);
                                    params.push(FuncParam {
                                        name: p_name,
                                        type_expr: p_type,
                                    });
                                }
                                Rule::type_expr => {
                                    return_type = Some(Box::new(parse_pair(param_p)?))
                                }
                                _ => unreachable!(),
                            }
                        }
                        items.push(TraitItem {
                            name: item_name,
                            generic_params: item_generic_params,
                            params,
                            return_type,
                        });
                    }
                    _ => unreachable!(),
                }
            }
            AstNodeKind::TraitDecl {
                name,
                generic_params,
                items,
            }
        }
        Rule::impl_decl => {
            let mut generic_params = Vec::new();
            let mut first_type: Option<AstNode> = None;
            let mut second_type: Option<AstNode> = None;
            let mut items: Vec<AstNode> = Vec::new();

            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::type_expr => {
                        if first_type.is_none() {
                            first_type = Some(parse_pair(p)?);
                        } else {
                            second_type = Some(parse_pair(p)?);
                        }
                    }
                    Rule::func_decl => {
                        items.push(parse_pair(p)?);
                    }
                    _ => unreachable!(),
                }
            }

            let (trait_type, target_type) = if let Some(target) = second_type {
                (first_type.map(Box::new), Box::new(target))
            } else {
                (
                    None,
                    Box::new(first_type.ok_or_else(|| {
                        RiddleError::Parse("Impl missing target type".to_string(), Some(span))
                    })?),
                )
            };

            AstNodeKind::ImplDecl {
                generic_params,
                trait_type,
                target_type,
                items,
            }
        }
        Rule::extern_block => {
            let mut abi = None;
            let mut functions = Vec::new();
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::string => {
                        let s = p.as_str();
                        abi = Some(s[1..s.len() - 1].to_string());
                    }
                    Rule::extern_item => {
                        functions.push(parse_extern_item(p)?);
                    }
                    _ => unreachable!(),
                }
            }
            AstNodeKind::ExternBlock { abi, functions }
        }
        Rule::extern_decl => {
            let mut abi = None;
            let mut function = None;
            for p in pair.into_inner() {
                match p.as_rule() {
                    Rule::string => {
                        let s = p.as_str();
                        abi = Some(s[1..s.len() - 1].to_string());
                    }
                    Rule::extern_item => {
                        function = Some(parse_extern_item(p)?);
                    }
                    _ => unreachable!(),
                }
            }
            AstNodeKind::ExternDecl {
                abi,
                function: function.ok_or_else(|| {
                    RiddleError::Parse("Extern decl missing function".to_string(), Some(span))
                })?,
            }
        }
        Rule::block => {
            let mut inner = pair.into_inner().peekable();
            let mut statements = Vec::new();
            let mut final_expr = None;
            while let Some(p) = inner.next() {
                if p.as_rule() == Rule::assign_expr && inner.peek().is_none() {
                    final_expr = Some(Box::new(parse_pair(p)?));
                } else {
                    statements.push(parse_pair(p)?);
                }
            }
            AstNodeKind::Block {
                statements,
                final_expr,
            }
        }
        Rule::return_stmt => {
            AstNodeKind::Return(Box::new(parse_pair(pair.into_inner().next().ok_or_else(
                || RiddleError::Parse("Return missing value".to_string(), Some(span)),
            )?)?))
        }
        Rule::expr_stmt => {
            return parse_pair(pair.into_inner().next().ok_or_else(|| {
                RiddleError::Parse("Empty expression statement".to_string(), Some(span))
            })?);
        }
        Rule::assign_expr | Rule::compare_expr | Rule::add_expr | Rule::mul_expr => {
            let mut inner = pair.into_inner();
            let mut lhs = parse_pair(inner.next().ok_or_else(|| {
                RiddleError::Parse("Binary expression missing LHS".to_string(), Some(span))
            })?)?;

            while let Some(op_pair) = inner.next() {
                let op = op_pair.as_str().trim().to_string();
                let rhs = parse_pair(inner.next().ok_or_else(|| {
                    RiddleError::Parse("Binary expression missing RHS".to_string(), Some(span))
                })?)?;
                let new_span = Span {
                    start: lhs.span.start,
                    end: rhs.span.end,
                };
                lhs = AstNode {
                    kind: AstNodeKind::BinaryExpr {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    },
                    span: new_span,
                };
            }
            return Ok(lhs);
        }
        Rule::unary_expr => {
            let mut inner = pair.into_inner();
            let mut ops = Vec::new();
            let mut last_pair = None;
            for p in inner {
                if p.as_rule() == Rule::unary_op {
                    ops.push(p);
                } else {
                    last_pair = Some(p);
                    break;
                }
            }

            let mut expr = parse_pair(last_pair.ok_or_else(|| {
                RiddleError::Parse("Unary expression missing base".to_string(), Some(span))
            })?)?;

            for op_pair in ops.into_iter().rev() {
                let op = op_pair.as_str().trim().to_string();
                let op_span = get_span(&op_pair);
                let new_span = Span {
                    start: op_span.start,
                    end: expr.span.end,
                };
                expr = AstNode {
                    kind: AstNodeKind::UnaryExpr {
                        op,
                        expr: Box::new(expr),
                    },
                    span: new_span,
                };
            }
            return Ok(expr);
        }
        Rule::cast_expr => {
            let mut inner = pair.into_inner();
            let mut expr = parse_pair(inner.next().ok_or_else(|| {
                RiddleError::Parse("Cast expression missing base".to_string(), Some(span))
            })?)?;

            while let Some(target_type_pair) = inner.next() {
                let target_type = parse_pair(target_type_pair)?;
                let new_span = Span {
                    start: expr.span.start,
                    end: target_type.span.end,
                };
                expr = AstNode {
                    kind: AstNodeKind::CastExpr {
                        expr: Box::new(expr),
                        target_type: Box::new(target_type),
                    },
                    span: new_span,
                };
            }
            return Ok(expr);
        }
        Rule::postfix_expr => {
            let mut inner = pair.into_inner();
            let mut expr = parse_pair(inner.next().ok_or_else(|| {
                RiddleError::Parse("Postfix expression missing base".to_string(), Some(span))
            })?)?;

            for p in inner {
                let p_span = get_span(&p);
                match p.as_rule() {
                    Rule::call_suffix => {
                        let args = p.into_inner().map(parse_pair).collect::<Result<Vec<_>>>()?;
                        let new_span = Span {
                            start: expr.span.start,
                            end: p_span.end,
                        };
                        expr = AstNode {
                            kind: AstNodeKind::Call {
                                func: Box::new(expr),
                                args,
                            },
                            span: new_span,
                        };
                    }
                    Rule::member_access => {
                        let member = p
                            .into_inner()
                            .next()
                            .ok_or_else(|| {
                                RiddleError::Parse(
                                    "Member access missing member name".to_string(),
                                    Some(p_span),
                                )
                            })?
                            .as_str()
                            .to_string();
                        let new_span = Span {
                            start: expr.span.start,
                            end: p_span.end,
                        };
                        expr = AstNode {
                            kind: AstNodeKind::MemberAccess {
                                object: Box::new(expr),
                                member,
                            },
                            span: new_span,
                        };
                    }
                    Rule::index_access => {
                        let index = parse_pair(p.into_inner().next().ok_or_else(|| {
                            RiddleError::Parse("Index access missing index".to_string(), Some(p_span))
                        })?)?;
                        let new_span = Span {
                            start: expr.span.start,
                            end: p_span.end,
                        };
                        expr = AstNode {
                            kind: AstNodeKind::IndexAccess {
                                object: Box::new(expr),
                                index: Box::new(index),
                            },
                            span: new_span,
                        };
                    }
                    Rule::struct_inst_suffix => {
                        let mut fields = Vec::new();
                        for f in p.into_inner() {
                            if f.as_rule() == Rule::struct_inst_field {
                                let mut f_inner = f.clone().into_inner();
                                let f_name = f_inner
                                    .next()
                                    .ok_or_else(|| {
                                        RiddleError::Parse(
                                            "Field missing name".to_string(),
                                            Some(get_span(&f)),
                                        )
                                    })?
                                    .as_str()
                                    .to_string();
                                let f_expr = parse_pair(f_inner.next().ok_or_else(|| {
                                    RiddleError::Parse(
                                        "Field missing value".to_string(),
                                        Some(get_span(&f)),
                                    )
                                })?)?;
                                fields.push((f_name, f_expr));
                            }
                        }
                        let name = match &expr.kind {
                            AstNodeKind::Identifier(n) => n.clone(),
                            AstNodeKind::Path(parts) => parts.join("::"),
                            _ => {
                                return Err(RiddleError::Parse(
                                    "Struct instantiation must follow an identifier or path"
                                        .to_string(),
                                    Some(expr.span),
                                ));
                            }
                        };
                        let new_span = Span {
                            start: expr.span.start,
                            end: p_span.end,
                        };
                        expr = AstNode {
                            kind: AstNodeKind::StructInst { name, fields },
                            span: new_span,
                        };
                    }
                    _ => unreachable!(),
                }
            }
            return Ok(expr);
        }
        Rule::identifier => AstNodeKind::Identifier(pair.as_str().to_string()),
        Rule::if_stmt => {
            let mut inner = pair.into_inner();
            let cond = Box::new(parse_pair(inner.next().ok_or_else(|| {
                RiddleError::Parse("If statement missing condition".to_string(), Some(span))
            })?)?);
            let then_block = Box::new(parse_pair(inner.next().ok_or_else(|| {
                RiddleError::Parse("If statement missing then block".to_string(), Some(span))
            })?)?);
            let else_block = if let Some(else_pair) = inner.next() {
                Some(Box::new(parse_pair(else_pair)?))
            } else {
                None
            };
            AstNodeKind::If {
                cond,
                then_block,
                else_block,
            }
        }
        Rule::path => {
            let parts: Vec<String> = pair
                .into_inner()
                .map(|p| p.as_str().trim().to_string())
                .collect();
            if parts.len() == 1 {
                AstNodeKind::Identifier(parts[0].clone())
            } else {
                AstNodeKind::Path(parts)
            }
        }
        Rule::type_expr => {
            return parse_pair(pair.into_inner().next().ok_or_else(|| {
                RiddleError::Parse("Empty type expression".to_string(), Some(span))
            })?);
        }
        Rule::path_type => {
            let mut inner = pair.into_inner();
            let path_pair = inner.next().ok_or_else(|| {
                RiddleError::Parse("Path type missing path".to_string(), Some(span))
            })?;
            let parts: Vec<String> = path_pair
                .into_inner()
                .map(|p| p.as_str().trim().to_string())
                .collect();
            let args = if let Some(args_pair) = inner.next() {
                parse_generic_args(args_pair)?
            } else {
                Vec::new()
            };
            AstNodeKind::TypeExpr { path: parts, args }
        }
        Rule::array_type => {
            let mut inner = pair.into_inner();
            let elem_type = Box::new(parse_pair(inner.next().ok_or_else(|| {
                RiddleError::Parse("Array type missing element type".to_string(), Some(span))
            })?)?);
            let size = inner
                .next()
                .ok_or_else(|| {
                    RiddleError::Parse("Array type missing size".to_string(), Some(span))
                })?
                .as_str()
                .parse()
                .map_err(|e| {
                    RiddleError::Parse(format!("Invalid array size: {}", e), Some(span))
                })?;
            AstNodeKind::ArrayType(elem_type, size)
        }
        Rule::pointer_type => {
            let inner = pair.into_inner().next().ok_or_else(|| {
                RiddleError::Parse("Pointer type missing inner type".to_string(), Some(span))
            })?;
            AstNodeKind::PointerType(Box::new(parse_pair(inner)?))
        }
        Rule::value_expr => {
            return parse_pair(pair.into_inner().next().ok_or_else(|| {
                RiddleError::Parse("Empty value expression".to_string(), Some(span))
            })?);
        }
        Rule::bool => AstNodeKind::Literal(Literal::Bool(
            pair.as_str()
                .parse()
                .map_err(|e| RiddleError::Parse(format!("{}", e), Some(span)))?,
        )),
        Rule::list_literal => {
            let elements = pair
                .into_inner()
                .map(parse_pair)
                .collect::<Result<Vec<_>>>()?;
            AstNodeKind::ListLiteral(elements)
        }
        Rule::number => AstNodeKind::Literal(Literal::Int(
            pair.as_str()
                .parse()
                .map_err(|e| RiddleError::Parse(format!("{}", e), Some(span)))?,
        )),
        Rule::float => AstNodeKind::Literal(Literal::Float(
            pair.as_str()
                .parse()
                .map_err(|e| RiddleError::Parse(format!("{}", e), Some(span)))?,
        )),
        Rule::string => {
            let s = pair.as_str();
            AstNodeKind::Literal(Literal::Str(s[1..s.len() - 1].to_string()))
        }
        Rule::cstr => {
            let s = pair.into_inner().next().unwrap().as_str();
            AstNodeKind::Literal(Literal::CStr(s[1..s.len() - 1].to_string()))
        }
        Rule::cint => {
            let n = pair.into_inner().next().unwrap().as_str();
            AstNodeKind::Literal(Literal::CInt(
                n.parse()
                    .map_err(|e| RiddleError::Parse(format!("{}", e), Some(span)))?,
            ))
        }
        _ => {
            return Err(RiddleError::Parse(
                format!("Rule {:?} not implemented", pair.as_rule()),
                Some(span),
            ));
        }
    };
    Ok(AstNode { kind, span })
}

use crate::frontend::ast::{
    AstNode, EnumVariant, ExternFunc, FuncParam, Literal, StructField, TraitItem,
};
use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct RiddleParser;

pub fn parse(input: &str) -> Result<AstNode, pest::error::Error<Rule>> {
    let pairs = RiddleParser::parse(Rule::program, input)?;
    Ok(parse_pair(pairs.into_iter().next().unwrap()))
}

fn parse_extern_item(pair: Pair<Rule>) -> ExternFunc {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let mut params = Vec::new();
    let mut return_type = None;
    for p in inner {
        match p.as_rule() {
            Rule::func_param => {
                let mut p_inner = p.into_inner();
                let p_name = p_inner.next().unwrap().as_str().to_string();
                let p_type = Box::new(parse_pair(p_inner.next().unwrap()));
                params.push(FuncParam {
                    name: p_name,
                    type_expr: p_type,
                });
            }
            Rule::type_expr => {
                return_type = Some(Box::new(parse_pair(p)))
            }
            _ => unreachable!(),
        }
    }
    ExternFunc {
        name,
        params,
        return_type,
    }
}

fn parse_generic_params(pair: Pair<Rule>) -> Vec<String> {
    pair.into_inner()
        .map(|p| p.as_str().to_string())
        .collect()
}

fn parse_generic_args(pair: Pair<Rule>) -> Vec<AstNode> {
    pair.into_inner().map(parse_pair).collect()
}

fn parse_pair(pair: Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::program => {
            let stmts = pair
                .into_inner()
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
                    Rule::type_expr => {
                        type_expr = Some(Box::new(parse_pair(p)))
                    }
                    Rule::value_expr => {
                        value = Some(Box::new(parse_pair(p)))
                    }
                    _ => unreachable!(),
                }
            }
            AstNode::VarDecl {
                name,
                type_expr,
                value,
            }
        }
        Rule::func_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut generic_params = Vec::new();
            let mut params = Vec::new();
            let mut return_type = None;
            let mut body = None;

            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::func_param => {
                        let mut p_inner = p.into_inner();
                        let p_name = p_inner.next().unwrap().as_str().to_string();
                        let p_type = Box::new(parse_pair(p_inner.next().unwrap()));
                        params.push(FuncParam {
                            name: p_name,
                            type_expr: p_type,
                        });
                    }
                    Rule::type_expr => {
                        return_type = Some(Box::new(parse_pair(p)))
                    }
                    Rule::block => body = Some(Box::new(parse_pair(p))),
                    _ => unreachable!(),
                }
            }
            AstNode::FuncDecl {
                name,
                generic_params,
                params,
                return_type,
                body: body.unwrap(),
            }
        }
        Rule::struct_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut generic_params = Vec::new();
            let mut fields = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::struct_field => {
                        let mut f_inner = p.into_inner();
                        let f_name = f_inner.next().unwrap().as_str().to_string();
                        let f_type = Box::new(parse_pair(f_inner.next().unwrap()));
                        fields.push(StructField {
                            name: f_name,
                            type_expr: f_type,
                        });
                    }
                    _ => unreachable!(),
                }
            }
            AstNode::StructDecl {
                name,
                generic_params,
                fields,
            }
        }
        Rule::enum_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut generic_params = Vec::new();
            let mut variants = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
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
                                    let fields = suffix
                                        .into_inner()
                                        .map(|f| {
                                            let mut f_inner = f.into_inner();
                                            let f_name =
                                                f_inner.next().unwrap().as_str().to_string();
                                            let f_type = Box::new(parse_pair(f_inner.next().unwrap()));
                                            StructField {
                                                name: f_name,
                                                type_expr: f_type,
                                            }
                                        })
                                        .collect();
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
            AstNode::EnumDecl {
                name,
                generic_params,
                variants,
            }
        }
        Rule::trait_decl => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let mut generic_params = Vec::new();
            let mut items = Vec::new();
            for p in inner {
                match p.as_rule() {
                    Rule::generic_params => generic_params = parse_generic_params(p),
                    Rule::trait_item => {
                        let mut item_inner = p.into_inner();
                        let item_name = item_inner.next().unwrap().as_str().to_string();
                        let mut item_generic_params = Vec::new();
                        let mut params = Vec::new();
                        let mut return_type = None;
                        for param_p in item_inner {
                            match param_p.as_rule() {
                                Rule::generic_params => {
                                    item_generic_params = parse_generic_params(param_p)
                                }
                                Rule::func_param => {
                                    let mut p_inner = param_p.into_inner();
                                    let p_name = p_inner.next().unwrap().as_str().to_string();
                                    let p_type = Box::new(parse_pair(p_inner.next().unwrap()));
                                    params.push(FuncParam {
                                        name: p_name,
                                        type_expr: p_type,
                                    });
                                }
                                Rule::type_expr => {
                                    return_type = Some(Box::new(parse_pair(param_p)))
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
            AstNode::TraitDecl {
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
                            first_type = Some(parse_pair(p));
                        } else {
                            second_type = Some(parse_pair(p));
                        }
                    }
                    Rule::func_decl => {
                        items.push(parse_pair(p));
                    }
                    _ => unreachable!(),
                }
            }

            let (trait_type, target_type) = if let Some(target) = second_type {
                (first_type.map(Box::new), Box::new(target))
            } else {
                (None, Box::new(first_type.unwrap()))
            };

            AstNode::ImplDecl {
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
                        functions.push(parse_extern_item(p));
                    }
                    _ => unreachable!(),
                }
            }
            AstNode::ExternBlock { abi, functions }
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
                        function = Some(parse_extern_item(p));
                    }
                    _ => unreachable!(),
                }
            }
            AstNode::ExternDecl {
                abi,
                function: function.unwrap(),
            }
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
            AstNode::Block {
                statements,
                final_expr,
            }
        }
        Rule::return_stmt => {
            AstNode::Return(Box::new(parse_pair(pair.into_inner().next().unwrap())))
        }
        Rule::expr_stmt => parse_pair(pair.into_inner().next().unwrap()),
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
            let mut expr = parse_pair(inner.next().unwrap());

            for p in inner {
                match p.as_rule() {
                    Rule::call_suffix => {
                        let args = p.into_inner().map(parse_pair).collect();
                        expr = AstNode::Call {
                            func: Box::new(expr),
                            args,
                        };
                    }
                    Rule::struct_inst => {
                        if let AstNode::Identifier(name) = expr {
                            let mut fields = Vec::new();
                            for f in p.into_inner() {
                                if f.as_rule() == Rule::struct_inst_field {
                                    let mut f_inner = f.into_inner();
                                    let f_name = f_inner.next().unwrap().as_str().to_string();
                                    let f_expr = parse_pair(f_inner.next().unwrap());
                                    fields.push((f_name, f_expr));
                                }
                            }
                            expr = AstNode::StructInst { name, fields };
                        } else if let AstNode::Path(parts) = expr {
                            let name = parts.join("::");
                            let mut fields = Vec::new();
                            for f in p.into_inner() {
                                if f.as_rule() == Rule::struct_inst_field {
                                    let mut f_inner = f.into_inner();
                                    let f_name = f_inner.next().unwrap().as_str().to_string();
                                    let f_expr = parse_pair(f_inner.next().unwrap());
                                    fields.push((f_name, f_expr));
                                }
                            }
                            expr = AstNode::StructInst { name, fields };
                        } else {
                            panic!("Struct instantiation must follow an identifier or path");
                        }
                    }
                    Rule::member_access => {
                        let member = p.into_inner().next().unwrap().as_str().to_string();
                        expr = AstNode::MemberAccess {
                            object: Box::new(expr),
                            member,
                        };
                    }
                    _ => unreachable!(),
                }
            }
            expr
        }
        Rule::identifier => AstNode::Identifier(pair.as_str().to_string()),
        Rule::path => {
            let parts: Vec<String> = pair
                .into_inner()
                .map(|p| p.as_str().trim().to_string())
                .collect();
            if parts.len() == 1 {
                AstNode::Identifier(parts[0].clone())
            } else {
                AstNode::Path(parts)
            }
        }
        Rule::type_expr => {
            let mut inner = pair.into_inner();
            let path_pair = inner.next().unwrap();
            let parts: Vec<String> = path_pair
                .into_inner()
                .map(|p| p.as_str().trim().to_string())
                .collect();
            let args = if let Some(args_pair) = inner.next() {
                parse_generic_args(args_pair)
            } else {
                Vec::new()
            };
            AstNode::TypeExpr { path: parts, args }
        }
        Rule::value_expr => parse_pair(pair.into_inner().next().unwrap()),
        Rule::bool => AstNode::Literal(Literal::Bool(pair.as_str().parse().unwrap())),
        Rule::number => AstNode::Literal(Literal::Int(pair.as_str().parse().unwrap())),
        Rule::float => AstNode::Literal(Literal::Float(pair.as_str().parse().unwrap())),
        Rule::string => {
            let s = pair.as_str();
            AstNode::Literal(Literal::Str(s[1..s.len() - 1].to_string()))
        }
        _ => todo!("Rule {:?} not implemented", pair.as_rule()),
    }
}

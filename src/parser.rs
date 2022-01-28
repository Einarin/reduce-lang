use crate::tokenize::{Token, TokenType};
use crate::ast::*;

pub fn parse<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut T) -> Result<Scope<'a>,String> {
    let mut scope = Scope::new();
    while let Some(token) = tokens.next() {
        let statement = parse_statement(token,tokens)?;
        //println!("{:?}",statement);
        /*if statement != Expression::EndOfExpression {
            result.push(statement);
        }*/
        match &statement {
            Expression::EndOfExpression => continue,
            Expression::Import(ref import) => scope.imports.push(import.clone()),
            Expression::Declaration(ref decl) => scope.data.push(decl.clone()),
            Expression::InfixOperation(ref infix) => match *(infix.lhs) {
                Expression::Declaration(ref decl) => {
                    let mut declCopy = decl.clone();
                    declCopy.value = Some(infix.rhs.clone());
                    scope.data.push(declCopy);
                },
                _ => ()
            },
            _ => ()
        }
        scope.statements.push(statement)
    }
    Ok(scope)
}

fn parse_statement<'a, T: Iterator<Item = Token<'a>>>(token: Token<'a>, tokens: &mut T) -> Result<Expression<'a>,String> {
    match token.ttype {
        TokenType::Keyword => match token.text {
            "use" => Ok(Expression::Import(parse_import(token,tokens)?)),
            "let" | "var" => {
                let declaration = Expression::Declaration(parse_var_declaration(token,tokens)?);
                if let Some(token) = tokens.next() {
                    match (token.ttype,token.text) {
                        (TokenType::Punctuation,";") => Ok(declaration),
                        (TokenType::Operator,"=") => Ok(Expression::InfixOperation(InfixOperation {
                            lhs: Box::new(declaration),
                            operator: token,
                            rhs: if let Some(rhs_start) = tokens.next() {
                                Box::new(parse_expr(rhs_start,tokens)?)
                            } else {
                                return Err(format!("Unexpected end of input after {:?}",token));
                            }
                        })),
                        _ => Err(format!("Unexpected after declaration {:?}",token))
                    }
                } else {
                    Err(format!("Unexpected end of input after declaration starting with {:?}",token))
                }
            },
            "fn" => Ok(Expression::FnDeclaration(parse_fn(token, tokens)?)),
            _ => panic!("Unimplemented keyword {}", token.text)
        },
        TokenType::Id | TokenType::Punctuation => parse_expr(token, tokens),
        _ => panic!("Unimplemented start of statement with token {:?}",token)
    }
}

fn parse_import<'a, T: Iterator<Item = Token<'a>>>(keyword: Token<'a>, iter: &mut T) -> Result<Import<'a>,String> {
    let mut path = Vec::new();
    while let Some(token) = iter.next() {
        match token.ttype {
            TokenType::Id => path.push(token),
            _ => return Err(format!("Expected Identifier, found {:?}",token)),
        };
        if let Some(token) = iter.next() {
            match (token.ttype,token.text) {
                (TokenType::Punctuation,":") => (),
                (TokenType::Punctuation,";") => {
                    return Ok(Import {
                        path
                    })
                },
                _ => panic!("Unexpected token {:?} in use statement",token)
            }
        }
    }
    Err(format!("Unexpected end of input while parsing input at {}:{}",keyword.line,keyword.offset))
}

fn parse_fn<'a, T: Iterator<Item = Token<'a>>>(keyword: Token<'a>, iter: &mut T) -> Result<FnDeclaration<'a>,String> {
    if keyword.ttype != TokenType::Keyword || keyword.text != "fn" {
        panic!("Expected fn, got {:?}", keyword);
    }
    if let Some(fn_name) = iter.next() {
        if fn_name.ttype != TokenType::Id {
            return Err(format!("Expected function name, found {:?}",fn_name));
        }
        let mut fn_decl = FnDeclaration {
            name: fn_name,
            args: Vec::new(),
            return_type: None,
            body: Scope::new()
        };

        if let Some(arg_open_braket) = iter.next() {
            if arg_open_braket.ttype != TokenType::Punctuation || arg_open_braket.text != "(" {
                return Err(format!("Unexpected token {:?}, expected a '('",arg_open_braket));
            };
            while let Some(token) = iter.next() {
                match token.ttype {
                    TokenType::Punctuation => match token.text {
                        ")" => break,
                        _ => panic!("Unexpected punctuation {:?}, expected a ')'",token)
                    },
                    TokenType::Id => {
                        let mut var = Variable {
                            name: token,
                            var_type: None
                        };
                        if let Some(token) = iter.next() {
                            if token.ttype != TokenType::Punctuation {
                                return Err(format!("Unexpected token {:?}, expected a ':', ',', or ')'",token));
                            }
                            match token.text {
                                "," => {
                                    fn_decl.args.push(var);
                                    continue
                                },
                                ")" => {
                                    fn_decl.args.push(var);
                                    break
                                },
                                ":" => {
                                    fn_decl.args.push(var);
                                    todo!()
                                },
                                _ => panic!("Unexpected token {:?}",token)
                            }
                        }
                    },
                    _ => panic!("Unexpected token {:?}",token)
                }
            }
            if let Some(body_start) = iter.next() {
                fn_decl.body = parse_fn_body(body_start,iter)?;
                Ok(fn_decl)
            } else {
                Err(format!("Unexpected end of input while parsing input at {}:{}",fn_name.line,fn_name.offset))    
            }
        } else {
            Err(format!("Unexpected end of input while parsing input at {}:{}",fn_name.line,fn_name.offset))    
        }
    } else {
        Err(format!("Unexpected end of input while parsing input at {}:{}",keyword.line,keyword.offset))
    }
    
}

fn parse_fn_body<'a, T: Iterator<Item = Token<'a>>>(keyword: Token<'a>, iter: &mut T) -> Result<Scope<'a>,String> {
    todo!()
}

fn parse_var_declaration<'a, T: Iterator<Item = Token<'a>>>(keyword: Token<'a>, iter: &mut T) -> Result<Declaration<'a>,String> {
    if keyword.ttype != TokenType::Keyword {
        panic!("Token {:?} wasn't a keyword!",keyword);
    }
    let constant = match keyword.text {
        "let" => Ok(true),
        "var" => Ok(false),
        _ => Err(format!("Logic error, tried to parse a variable declaration starting with keyword {}", keyword.text))
    };
    Ok(Declaration{
        variable: Variable {
            name: iter.next().unwrap(),
            var_type: None,
        },
        value: None,
        constant: constant?,
    })
}

fn parse_expr<'a, T: Iterator<Item = Token<'a>>>(token: Token<'a>, iter: &mut T) -> Result<Expression<'a>,String> {
    match token.ttype {
        TokenType::Id => parse_expr_starting_with_id(token,iter),
        TokenType::NumberLiteral | TokenType::StringLiteral => parse_expr_starting_with_literal(token,iter),
        TokenType::Punctuation => match token.text {
            ";" | ")" | "]" | "}" | "," => Ok(Expression::EndOfExpression),
            "(" => {
                if let Some(inner_start) = iter.next() {
                    let inner = parse_expr(inner_start, iter)?;
                    if let Some(next) = iter.next() {
                        match next.ttype {
                            TokenType::Punctuation => match next.text {
                                ")" => Ok(inner),
                                _ => panic!("unimplemented at {:?}",next)
                            },
                            TokenType::Operator => {
                                let rhs = if let Some(rhs_start) = iter.next() {
                                    parse_expr(rhs_start,iter)
                                } else {
                                    return Err(format!("Unexpected end of input after {:?}",next));
                                }?;
                                /*if let Expression::InfixOperation(op) = rhs {
                                    if eval_left_to_right(token.text,op.operator.text) {
                                        return Ok(Expression::InfixOperation(InfixOperation{
                                            lhs: Box::new(Expression::InfixOperation(InfixOperation{
                                                lhs: Box::new(Expression::Literal(literal)),
                                                operator: token,
                                                rhs: op.lhs,
                                            })),
                                            operator: op.operator,
                                            rhs: op.rhs,
                                        }))   
                                    }      
                                    return Ok(Expression::InfixOperation(InfixOperation{
                                    lhs: Box::new(Expression::Literal(literal)),
                                    operator: token,
                                    rhs: Box::new(Expression::InfixOperation(op)),
                                }))  
                                }*/
                                return Ok(Expression::InfixOperation(InfixOperation{
                                    lhs: Box::new(inner),
                                    operator: next,
                                    rhs: Box::new(rhs),
                                }))
                            },
                            _ => panic!("unimplemented at {:?}",next)
                        }
                    } else {
                        Err(format!("Unexpected EOF after ( at {:?}",token))
                    }
                } else {
                    Err(format!("Unexpected EOF after ( at {:?}",token))
                }
            }
            _ => panic!("unimplemented at {:?}",token)
        },
        _ => panic!("unimplemented at {:?}",token)//Err(format!("unexpected token {:?}", token))
    }
}

fn parse_expr_starting_with_literal<'a, T: Iterator<Item = Token<'a>>>(literal: Token<'a>, iter: &mut T) -> Result<Expression<'a>,String> {
    match literal.ttype {
        TokenType::NumberLiteral | TokenType::StringLiteral => (),
        _ => panic!("Token {:?} wasn't a literal!", literal)
    };
    while let Some(token) = iter.next() {
        match token.ttype {
            TokenType::Punctuation => match token.text {
                ";" | ")" | "]" | "}" | "," => return Ok(Expression::Literal(literal)),
                _ => return Err(format!("Unexpected punctuation {:?}",token))
            },
            TokenType::Operator => {
                let rhs = if let Some(rhs_start) = iter.next() {
                    parse_expr(rhs_start,iter)
                } else {
                    return Err(format!("Unexpected end of input after {:?}",token));
                }?;
                /*if let Expression::InfixOperation(op) = rhs {
                    if eval_left_to_right(token.text,op.operator.text) {
                        return Ok(Expression::InfixOperation(InfixOperation{
                            lhs: Box::new(Expression::InfixOperation(InfixOperation{
                                lhs: Box::new(Expression::Literal(literal)),
                                operator: token,
                                rhs: op.lhs,
                            })),
                            operator: op.operator,
                            rhs: op.rhs,
                        }))   
                    }      
                    return Ok(Expression::InfixOperation(InfixOperation{
                    lhs: Box::new(Expression::Literal(literal)),
                    operator: token,
                    rhs: Box::new(Expression::InfixOperation(op)),
                }))  
                }*/
                return Ok(Expression::InfixOperation(InfixOperation{
                    lhs: Box::new(Expression::Literal(literal)),
                    operator: token,
                    rhs: Box::new(rhs),
                }))
            },
            _ => panic!("Unimplemented at {:?}",token)
        }
    }
    Err(format!("unexpected end of input after {:?}",literal))
}

fn parse_expr_starting_with_id<'a, T: Iterator<Item = Token<'a>>>(identifier: Token<'a>, iter: &mut T) -> Result<Expression<'a>,String> {
    if identifier.ttype != TokenType::Id {
        panic!("Token {:?} wasn't an Id!", identifier);
    }
    let mut lhs = Expression::Variable(Variable::used(identifier));
    while let Some(token) = iter.next() {
        match token.ttype {
            TokenType::Punctuation => match token.text {
                "(" => return parse_callable(lhs, token, iter),
                "[" => {
                    if let Some(index_expr_start) = iter.next() {
                        let index_expr = parse_expr(index_expr_start, iter)?;
                        if let Some(index_end) = iter.next() {
                            if index_end.ttype == TokenType::Punctuation && index_end.text == "]" {
                                panic!("Not implemented yet!")
                            }
                        } else {
                            return Err(format!("Expected ] to balance {:?}",token))
                        }
                    } else {
                        return Err(format!("unexpected end of input after {:?}",token))
                    }
                },
                ";" | ")" | "]" | "}" | "," => return Ok(lhs),
                _ => return Err(format!("Unexpected punctuation {:?}",token))
            },
            TokenType::Operator => {
                let rhs = if let Some(rhs_start) = iter.next() {
                    parse_expr(rhs_start,iter)
                } else {
                    return Err(format!("Unexpected end of input after {:?}",token));
                }?;
                let mut result = InfixOperation{
                    lhs: Box::new(lhs),
                    operator: token,
                    rhs: Box::new(rhs),
                };
                /*if let Expression::InfixOperation(op) = &*result.rhs {
                    if eval_left_to_right(token.text,op.operator.text) {
                        return Ok(Expression::InfixOperation(InfixOperation{
                            lhs: Box::new(Expression::InfixOperation(InfixOperation{
                                lhs: result.lhs,
                                operator: token,
                                rhs: op.lhs.clone(),
                            })),
                            operator: op.operator,
                            rhs: op.rhs.clone(),
                        }));
                    } else {
                        return Ok(Expression::InfixOperation(result));
                    }
                } else {*/
                    return Ok(Expression::InfixOperation(result));
                //}
            },
            _ => panic!("Unimplemented at {:?}",token)//return Err(format!("Unexpected token type at {:?}", token))
        }
    }
    Err(format!("unexpected end of input after {:?}",identifier))
}

fn eval_left_to_right(left_op: &str, right_op: &str) -> bool {
    let res = precedence(left_op) >= precedence(right_op);
    if res {
        println!("Evaluating {} before {}", left_op, right_op);
    } else {
        println!("Evaluating {} after {}", left_op, right_op);
    }
    res
}

fn precedence(op: &str) -> u32 {
    match op {
        "*" => 10,
        "/" => 9,
        "+" => 8,
        "-" => 7,
         _ => panic!("Unimplemented precedence check for {}",op)
    }
}

fn parse_callable<'a, T: Iterator<Item = Token<'a>>>(name: Expression<'a>, openParen: Token<'a>, iter: &mut T) -> Result<Expression<'a>,String> {
    //println!("parse_callable: {:?}",name);
    let mut args = Vec::new();
    while let Some(token) = iter.next() {
        match token.ttype {
            TokenType::Punctuation => match token.text {
                ")" | ";" => return Ok(Expression::Callable(Callable {
                    name: Box::new(name),
                    args,
                })),
                "," => continue,
                _ => panic!("Not implemented at {:?}",token)
            },
            _ => {
                let subexpr = parse_expr(token, iter)?;
                if let Expression::EndOfExpression = subexpr {
                    return Ok(Expression::Callable(Callable {
                        name: Box::new(name),
                        args,
                    }));
                } else {
                    args.push(subexpr);
                }
            }
        }
    }
    Err(format!("Expected ) to balance {:?}",openParen))
}
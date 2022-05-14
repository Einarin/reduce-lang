use std::iter::Peekable;
use crate::tokenize::{Token, TokenType};
use crate::ast::*;
use crate::types::DynamicType;

pub fn parse_scope<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut Peekable<T>) -> Result<Scope<'a>,String> {
    let mut scope = Scope::new();
    while let Some(token) = tokens.peek() {
        println!("Parse working from {:?}",token);
        let statement = parse_statement(tokens)?;
        println!("{:?}",statement);
        /*if statement != Expression::EndOfExpression {
            result.push(statement);
        }*/
        match &statement {
            Expression::EndOfExpression => continue,
            Expression::EndOfScope => break,
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

fn parse_statement<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut Peekable<T>) -> Result<Expression<'a>,String> {
    let token = *tokens.peek().unwrap();
    println!("Start of parse statement at {:?}",token);
    match token.ttype {
        TokenType::Keyword => match token.text {
            "use" => Ok(Expression::Import(parse_import(tokens)?)),
            "let" | "var" => {
                let declaration = Expression::Declaration(parse_var_declaration(tokens)?);
                if let Some(token) = tokens.peek() {
                    match (token.ttype,token.text) {
                        (TokenType::Punctuation,";") => Ok(declaration),
                        (TokenType::Operator,"=") => Ok(Expression::InfixOperation(InfixOperation {
                            lhs: Box::new(declaration),
                            operator: tokens.next().unwrap(),
                            rhs: Box::new(parse_expr(tokens)?)
                        })),
                        _ => Err(format!("Unexpected after declaration {:?}",token))
                    }
                } else {
                    Err(format!("Unexpected end of input after declaration starting with {:?}",token))
                }
            },
            "fn" => Ok(Expression::FnDeclaration(parse_fn(tokens)?)),
            "if" => parse_if(tokens),
            "while" => parse_while(tokens),
            _ => panic!("Unimplemented keyword {}", token.text)
        },
        TokenType::Id | TokenType::Punctuation => parse_expr(tokens),
        _ => panic!("Unimplemented start of statement with token {:?}",token)
    }
}

fn parse_import<'a, T: Iterator<Item = Token<'a>>>(iter: &mut Peekable<T>) -> Result<Import<'a>,String> {
    let start = iter.next().unwrap();
    if start.ttype != TokenType::Keyword || start.text != "use" {
        panic!("expected 'use', found {:?}",start);
    }
    let mut path = Vec::new();
    while let Some(token) = iter.peek() {
        match token.ttype {
            TokenType::Id => path.push(iter.next().unwrap()),
            _ => return Err(format!("Expected Identifier, found {:?}",token)),
        };
        if let Some(token) = iter.next() {
            match (token.ttype,token.text) {
                (TokenType::Punctuation,":") => {
                    if let Some(token) = iter.next() {
                        if token.ttype != TokenType::Punctuation || token.text != ":" {
                            panic!("expected ':', found {:?}",token);
                        }
                    } else {
                        panic!("Unexpected token {:?} in use statement",token);
                    }
                },
                (TokenType::Punctuation,";") => {
                    return Ok(Import {
                        path
                    })
                },
                _ => panic!("Unexpected token {:?} in use statement",token)
            }
        }
    }
    Err("Unexpected end of input while parsing import!".to_string())
}

fn parse_fn<'a, T: Iterator<Item = Token<'a>>>(iter: &mut Peekable<T>) -> Result<FnDeclaration<'a>,String> {
    let keyword = iter.next().unwrap();
    println!("starting function parse at {:?}",keyword);
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
                match body_start.ttype {
                    TokenType::Punctuation => match body_start.text {
                        "{" => {
                            fn_decl.body = parse_fn_body(body_start,iter)?;
                            Ok(fn_decl)
                        },
                        _ => Err(format!("Unexpected token {:?}, expected the start of a function body!",body_start))
                    },
                    _ => Err(format!("Unexpected token {:?}, expected the start of a function body!",body_start))
                }
                
            } else {
                Err(format!("Unexpected end of input while parsing input at {:?}",fn_name))    
            }
        } else {
            Err(format!("Unexpected end of input while parsing input at {:?}",fn_name))    
        }
    } else {
        Err(format!("Unexpected end of input while parsing input at {:?}",keyword))
    }
    
}

fn parse_fn_body<'a, T: Iterator<Item = Token<'a>>>(scope_start: Token<'a>, iter: &mut Peekable<T>) -> Result<Scope<'a>,String> {
    let result = parse_scope(iter);
    if let Ok(_) = result {
        if let Some(token) = iter.next() {
            if token.ttype == TokenType::Punctuation && token.text == "}" {
                println!("End of function boday at {:?}",token);
                result
            } else {
                Err(format!("Unexpected token, expected a '}}' signifying end of scope but got a {:?}", token))
            }
        } else {
            Err(format!("Unexpected end of input at end of scope starting at {:?}", scope_start))
        }
    } else {
        result
    }
}

fn parse_if<'a, T: Iterator<Item = Token<'a>>>(iter: &mut Peekable<T>) -> Result<Expression<'a>,String> {
    let keyword = iter.next().unwrap();
    let condition = parse_expr(iter)?;
    println!("IF condition: {:?}",condition);
    if let Some(token) = iter.next() {
        if token.ttype != TokenType::Punctuation || token.text != "{" {
            return Err(format!("Expected a {{, got a {:?}",token));
        }
    } else {
        return Err(format!("Unexpected EOF after {}",condition.location()));
    }
    let body = parse_scope(iter)?;
    if let Some(token) = iter.next() {
        if token.ttype != TokenType::Punctuation || token.text != "}" {
            return Err(format!("Expected a {{, got a {:?}",token));
        }
    } else {
        return Err(format!("Unexpected EOF after {}",condition.location()));
    }
    let mut else_body = None;
    if let Some(token) = iter.peek() {
        if token.ttype == TokenType::Keyword && token.text == "else" {
            let _else_token = iter.next();
            if let Some(token) = iter.next() {
                if token.ttype != TokenType::Punctuation || token.text != "{" {
                    return Err(format!("Expected a {{, got a {:?}",token));
                }
            } else {
                return Err(format!("Unexpected EOF after {}",condition.location()));
            }
            else_body = Some(Box::new(parse_scope(iter)?));
            if let Some(token) = iter.next() {
                if token.ttype != TokenType::Punctuation || token.text != "}" {
                    return Err(format!("Expected a {{, got a {:?}",token));
                }
            } else {
                return Err(format!("Unexpected EOF after {}",condition.location()));
            }
        }
    }
    Ok(Expression::If(If {
        keyword,
        condition: Box::new(condition),
        body: Box::new(body),
        else_body,
    }))
}

fn parse_while<'a, T: Iterator<Item = Token<'a>>>(iter: &mut Peekable<T>) -> Result<Expression<'a>,String> {
    let keyword = iter.next().unwrap();
    let condition = parse_expr(iter)?;
    println!("While condition: {:?}",condition);
    if let Some(token) = iter.next() {
        if token.ttype != TokenType::Punctuation || token.text != "{" {
            return Err(format!("Expected a {{, got a {:?}",token));
        }
    } else {
        return Err(format!("Unexpected EOF after {}",condition.location()));
    }
    let body = parse_scope(iter)?;
    if let Some(token) = iter.next() {
        if token.ttype != TokenType::Punctuation || token.text != "}" {
            return Err(format!("Expected a {{, got a {:?}",token));
        }
    } else {
        return Err(format!("Unexpected EOF after {}",condition.location()));
    }
    Ok(Expression::While(While {
        keyword,
        condition: Box::new(condition),
        body: Box::new(body)
    }))
}

fn parse_var_declaration<'a, T: Iterator<Item = Token<'a>>>(iter: &mut Peekable<T>) -> Result<Declaration<'a>,String> {
    let keyword = iter.next().unwrap();
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

fn parse_expr<'a, T: Iterator<Item = Token<'a>>>(iter: &mut Peekable<T>) -> Result<Expression<'a>,String> {
    let token = *iter.peek().unwrap();
    match token.ttype {
        TokenType::Id => parse_expr_starting_with_id(iter.next().unwrap(), iter),
        TokenType::NumberLiteral | TokenType::StringLiteral => parse_expr_starting_with_literal(iter.next().unwrap(), iter),
        TokenType::Punctuation => match token.text {
            ";" | ")" | "]" | "," =>  { iter.next(); Ok(Expression::EndOfExpression) },
            "}" => Ok(Expression::EndOfScope),
            "(" => {
                let open_paren = iter.next().unwrap();
                if let Some(inner_start) = iter.peek() {
                    let inner = parse_expr(iter)?;
                    if let Some(next) = iter.next() {
                        match next.ttype {
                            TokenType::Punctuation => match next.text {
                                ")" => Ok(inner),
                                _ => panic!("unimplemented at {:?}",next)
                            },
                            TokenType::Operator => {
                                let rhs = if let Some(rhs_start) = iter.peek() {
                                    parse_expr(iter)
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
        TokenType::Keyword => match token.text {
            "true" | "false" => parse_expr_starting_with_literal(iter.next().unwrap(), iter),
            _ => Err(format!("Unexpected keyword {:?}", token))
        }
        _ => panic!("unimplemented at {:?}",token)//Err(format!("unexpected token {:?}", token))
    }
}

fn parse_expr_starting_with_literal<'a, T: Iterator<Item = Token<'a>>>(literal: Token<'a>, iter: &mut Peekable<T>) -> Result<Expression<'a>,String> {
    if let Err(msg) = DynamicType::from_token(&literal) {
        panic!("Token {:?} wasn't a literal! {}", literal, msg);
    }
    loop {
        let token = if let Some(token) = iter.peek() {
            *token
        } else {
            return Err(format!("unexpected end of input after {:?}",literal));
        };
        match token.ttype {
            TokenType::Punctuation => match token.text {
                ";" | ")" | "]" | "," => {iter.next(); return Ok(Expression::Literal(literal))},
                "}" | "{" => return Ok(Expression::Literal(literal)),
                _ => return Err(format!("Unexpected punctuation {:?}",token))
            },
            TokenType::Operator => {
                let operator = iter.next().unwrap();
                let rhs = if let Some(rhs_start) = iter.peek() {
                    parse_expr(iter)
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
                    operator,
                    rhs: Box::new(rhs),
                }))
            },
            _ => panic!("Unimplemented at {:?}",token)
        }
    }
}

fn parse_expr_starting_with_id<'a, T: Iterator<Item = Token<'a>>>(identifier: Token<'a>, iter: &mut Peekable<T>) -> Result<Expression<'a>,String> {
    if identifier.ttype != TokenType::Id {
        panic!("Token {:?} wasn't an Id!", identifier);
    }
    let mut lhs = Expression::Variable(Variable::used(identifier));
    loop {
        let token = if let Some(token) = iter.peek() {
            *token
        } else {
            return Err(format!("unexpected end of input after {:?}",identifier));
        };
        println!("parse_expr_starting_with_id: next is {:?}",token);
        match token.ttype {
            TokenType::Punctuation => match token.text {
                "(" => return parse_callable(lhs, iter.next().unwrap(), iter),
                "[" => {
                    iter.next();
                    if let Some(index_expr_start) = iter.peek() {
                        let index_expr = parse_expr(iter)?;
                        if let Some(index_end) = iter.peek() {
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
                ";" | ")" | "]" | "," => {iter.next(); return Ok(lhs)},
                "}" | "{" => return Ok(lhs),
                _ => return Err(format!("Unexpected punctuation {:?}",token))
            },
            TokenType::Operator => {
                let operator = iter.next().unwrap();
                println!("operator: {:?}",operator);
                let rhs = if let Some(rhs_start) = iter.peek() {
                    parse_expr(iter)
                } else {
                    return Err(format!("Unexpected end of input after {:?}",token));
                }?;
                let mut result = InfixOperation{
                    lhs: Box::new(lhs),
                    operator,
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

fn parse_callable<'a, T: Iterator<Item = Token<'a>>>(name: Expression<'a>, openParen: Token<'a>, iter: &mut Peekable<T>) -> Result<Expression<'a>,String> {
    //println!("parse_callable: {:?}",name);
    let mut args = Vec::new();
    while let Some(token) = iter.peek() {
        match token.ttype {
            TokenType::Punctuation => match token.text {
                ")" | ";" => {
                    iter.next();
                    return Ok(Expression::Callable(Callable {
                        name: Box::new(name),
                        args,
                    }))
                },
                "," => {
                    iter.next();
                    continue
                },
                _ => panic!("Not implemented at {:?}",token)
            },
            _ => {
                let subexpr = parse_expr(iter)?;
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
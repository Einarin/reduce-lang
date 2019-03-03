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
        TokenType::NumberLiteral | TokenType::StringLiteral => Ok(Expression::Literal(token)),
        TokenType::Punctuation => match token.text {
            ";" | ")" | "]" | "}" | "," => Ok(Expression::EndOfExpression),
            _ => panic!("unimplemented at {:?}",token)
        },
        _ => panic!("unimplemented at {:?}",token)//Err(format!("unexpected token {:?}", token))
    }
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
            TokenType::Operator => return Ok(Expression::InfixOperation(InfixOperation{
                lhs: Box::new(lhs),
                operator: token,
                rhs:  if let Some(rhs_start) = iter.next() {
                    Box::new(parse_expr(rhs_start,iter)?)
                } else {
                    return Err(format!("Unexpected end of input after {:?}",token));
                },
            })),
            _ => panic!("Unimplemented at {:?}",token)//return Err(format!("Unexpected token type at {:?}", token))
        }
    }
    Err(format!("unexpected end of input after {:?}",identifier))
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
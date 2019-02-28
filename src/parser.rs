use crate::tokenize::{Token, TokenType};

pub struct Import<'a> {
    path: Vec<Token<'a>>,
}

struct Type<'a> {
    path: Vec<Token<'a>>,
}

struct Variable<'a> {
    name: Token<'a>,
    var_type: Option<Type<'a>>,
    constant: bool
}

struct Scope<'a> {
    imports: Import<'a>,
    variables: Vec<Variable<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        Scope {
            imports: Import {
                path: Vec::new()
            },
            variables: Vec::new(),
        }
    }
}

pub struct Declaration<'a> {
    variable: Variable<'a>,
    value: Option<Box<Expression<'a>>>
}

pub struct InfixOperation<'a> {
    rhs: Box<Expression<'a>>,
    operator: Token<'a>,
    lhs: Box<Expression<'a>>,
}

pub enum Expression<'a> {
    Import(Import<'a>),
    Declaration(Declaration<'a>),
    InfixOperation(InfixOperation<'a>),
    Value(Token<'a>),
}

pub fn parse<'a, T: Iterator<Item = Token<'a>>>(tokens: &mut T) -> Result<Vec<Expression<'a>>,String> {
    let mut result = Vec::new();
    while let Some(token) = tokens.next() {
        result.push(parse_statement(token,tokens)?);
    }
    Ok(result)
}

fn parse_statement<'a, T: Iterator<Item = Token<'a>>>(token: Token<'a>, tokens: &mut T) -> Result<Expression<'a>,String> {
    match token.token {
        TokenType::Keyword(name) => match name {
            "use" => Ok(Expression::Import(parse_import(token,tokens)?)),
            "let" | "var" => Ok(Expression::Declaration(parse_var_declaration(token,tokens)?)),
            _ => panic!("Unimplemented keyword {}", name)
        },
        _ => panic!("Unimplemented start of statement with token {:?}",token)
    }
}

fn parse_import<'a, T: Iterator<Item = Token<'a>>>(keyword: Token<'a>, iter: &mut T) -> Result<Import<'a>,String> {
    let mut path = Vec::new();
    while let Some(token) = iter.next() {
        match token.token {
            TokenType::Id(_) => path.push(token),
            _ => return Err(format!("Expected Identifier, found {:?}",token)),
        };
        if let Some(token) = iter.next() {
            match token.token {
                TokenType::Punctuation(":") => (),
                TokenType::Punctuation(";") => {
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
    let constant = if let TokenType::Keyword(name) = keyword.token {
        match name {
            "let" => true,
            "var" => false,
            _ => panic!("Logic error, tried to parse a variable declaration starting with keyword {}", name)
        }
    } else {
        panic!("Logic error, tried to parse a variable declaration starting with token {:?} that is not a keyword!", keyword)
    };
    Ok(Declaration{
        variable: Variable {
            name: iter.next().unwrap(),
            var_type: None,
            constant,
        },
        value: None
    })
}
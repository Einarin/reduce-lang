use crate::tokenize::{Token, TokenType};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Import<'a> {
    pub path: Vec<Token<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type<'a> {
    pub path: Vec<&'a str>,
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,"{}",self.path.join(":"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<'a> {
    pub name: Token<'a>,
    pub var_type: Option<Type<'a>>,
}

impl<'a> Variable<'a> {
    pub fn used(token: Token<'a>) -> Variable<'a> {
        Variable {
            name: token,
            var_type: None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope<'a> {
    pub imports: Vec<Import<'a>>,
    pub data: Vec<Declaration<'a>>,
    pub statements: Vec<Expression<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope {
            imports: Vec::new(),
            data: Vec::new(),
            statements: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration<'a> {
    pub variable: Variable<'a>,
    pub value: Option<Box<Expression<'a>>>,
    pub constant: bool
}

#[derive(Debug, Clone, PartialEq)]
pub struct InfixOperation<'a> {
    pub lhs: Box<Expression<'a>>,
    pub operator: Token<'a>,
    pub rhs: Box<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Callable<'a> {
    pub name: Box<Expression<'a>>,
    pub args: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    EndOfExpression,
    Import(Import<'a>),
    Declaration(Declaration<'a>),
    InfixOperation(InfixOperation<'a>),
    Literal(Token<'a>),
    Variable(Variable<'a>),
    Callable(Callable<'a>),
    Scope(Scope<'a>)
}

impl<'a> fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::EndOfExpression => write!(f,"EndOfExpression (Should this be here?)"),
            Expression::Import(import) => write!(f,"use {}", import.path.iter().map(|x|x.text).collect::<Vec<&str>>().join(":")),
            Expression::Declaration(decl) => {
                let mutability = if decl.constant {
                    "let"
                } else {
                    "var"
                };
                if let Some(ref expr) = decl.value {
                    write!(f,"{} {} = {}", mutability, decl.variable.name.text, expr)
                } else {
                    write!(f,"{} {}", mutability, decl.variable.name.text)
                }
            },
            Expression::InfixOperation(op) => write!(f,"{} {} {}", op.lhs, op.operator.text, op.rhs),
            Expression::Literal(lit) => write!(f,"{}",lit.text),
            Expression::Variable(var) => write!(f, "{}", var.name.text),
            Expression::Callable(call) => write!(f, "{}({})",call.name, call.args.iter().map(|x|format!("{}",x)).collect::<Vec<String>>().join(",")),
            Expression::Scope(scope) => panic!("Display not implemented for Scope"),
        }
    }
}
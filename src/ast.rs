use crate::tokenize::{Token, OwnedToken, FileLocation};
use std::{fmt, env::var};

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
        write!(f,"{}",self.path.join("::"))
    }
}

#[derive(Clone, PartialEq)]
pub struct Variable<'a> {
    pub name: Token<'a>,
    pub var_type: Option<Type<'a>>,
}

impl<'a> std::fmt::Debug for Variable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.var_type {
            Some(vtype) => write!(f,"{} {}", vtype, self.name.text),
            None => write!(f,"var {}", self.name.text)
        }
    }
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
pub struct FnDeclaration<'a> {
    pub name: Token<'a>,
    pub args: Vec<Variable<'a>>,
    pub return_type: Option<Type<'a>>,
    pub body: Scope<'a>,
}

#[derive(Clone, PartialEq)]
pub struct InfixOperation<'a> {
    pub lhs: Box<Expression<'a>>,
    pub operator: Token<'a>,
    pub rhs: Box<Expression<'a>>,
}

impl<'a> std::fmt::Debug for InfixOperation<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{:?} {} {:?}", self.lhs, self.operator.text, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Callable<'a> {
    pub name: Box<Expression<'a>>,
    pub args: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    EndOfExpression,
    EndOfScope,
    Import(Import<'a>),
    Declaration(Declaration<'a>),
    FnDeclaration(FnDeclaration<'a>),
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
            Expression::EndOfScope => write!(f,"EndOfScope (Should this be here?)"),
            Expression::Import(import) => write!(f,"use {}", import.path.iter().map(|x|x.text).collect::<Vec<&str>>().join("::")),
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
            Expression::FnDeclaration(decl) => {
                write!(f,"{}({:?})->{:?}",decl.name.text,decl.args,decl.return_type)
            }
            Expression::InfixOperation(op) => write!(f,"{} {} {}", op.lhs, op.operator.text, op.rhs),
            Expression::Literal(lit) => write!(f,"{}",lit.text),
            Expression::Variable(var) => write!(f, "{}", var.name.text),
            Expression::Callable(call) => write!(f, "{}({})",call.name, call.args.iter().map(|x|format!("{}",x)).collect::<Vec<String>>().join(",")),
            Expression::Scope(scope) => panic!("Display not implemented for Scope"),
        }
    }
}

impl<'a> Expression<'a> {
    pub fn location(&self) -> FileLocation {
        match self {
            Expression::EndOfExpression | Expression::EndOfScope => FileLocation { line: 0, offset: 0 },
            Expression::Import(import) => import.path[0].location,
            Expression::Declaration(decl) => decl.variable.name.location,
            Expression::FnDeclaration(fndecl) => fndecl.name.location,
            Expression::InfixOperation(op) => op.lhs.location(),
            Expression::Literal(lit) => lit.location,
            Expression::Variable(var) => var.name.location,
            Expression::Callable(call) => call.name.location(),
            Expression::Scope(scope) => scope.statements[0].location()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OwnedType {
    pub path: Vec<String>,
}

impl<'a> From<&Type<'a>> for OwnedType {
    fn from(var_type: &Type<'a>) -> Self {
        OwnedType{ path : var_type.path.iter().map(|s|s.to_string()).collect() }
    }
}

#[derive(Clone, PartialEq)]
pub struct OwnedVariable {
    pub name: OwnedToken,
    pub var_type: Option<OwnedType>,
}

impl<'a> From<&Variable<'a>> for OwnedVariable {
    fn from(variable: &Variable<'a>) -> Self {
        OwnedVariable {
            name: variable.name.into(),
            var_type: match &variable.var_type {
                Some(vtype) => Some(OwnedType::from(vtype)),
                None => None
            }
        }
    }
}

pub struct OwnedFnDeclaration {
    pub name: String,
    pub definition_location: (usize,usize),
    pub args: Vec<OwnedVariable>,
    pub return_type: Option<OwnedType>
}
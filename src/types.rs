use crate::tokenize::{Token,TokenType};
use core::num;
use std::ops::*;
use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Number {
    Integer(i64),
    Float(f64)
}

#[derive(Debug, Clone)]
pub enum DynamicType {
    Void,
    String(String),
    Number(Number)
}

pub type DynamicTypeResult = Result<DynamicType,String>;

impl Add for DynamicType {
    type Output = DynamicTypeResult;

    fn add(self, rhs: DynamicType) -> Self::Output {
        match (&self,&rhs) {
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Integer(lhs + rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(lhs + rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Float(lhs + *rhs as f64))),
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(*lhs as f64 + rhs))),
            _ => Err(format!("TypeError: Can't add {:?} + {:?}",self,rhs))
        }
    }
}

impl Sub for DynamicType {
    type Output = DynamicTypeResult;

    fn sub(self, rhs: DynamicType) -> Self::Output {
        match (&self,&rhs) {
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Integer(lhs - rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(lhs - rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Float(lhs - *rhs as f64))),
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(*lhs as f64 - rhs))),
            _ => Err(format!("TypeError: Can't add {:?} + {:?}",self,rhs))
        }
    }
}

impl Mul for DynamicType {
    type Output = DynamicTypeResult;

    fn mul(self, rhs: DynamicType) -> Self::Output {
        match(&self,&rhs) {
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Integer(lhs * rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(lhs * rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Float(lhs * *rhs as f64))),
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(*lhs as f64 * rhs))),
            _ => Err(format!("TypeError: Can't add {:?} + {:?}",self,rhs))
        }
    }
}

impl Div for DynamicType {
    type Output = DynamicTypeResult;

    fn div(self, rhs: DynamicType) -> Self::Output {
        match(&self,&rhs) {
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Integer(lhs / rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(lhs / rhs))),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Integer(rhs))) => Ok(DynamicType::Number(Number::Float(lhs / *rhs as f64))),
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Float(rhs))) => Ok(DynamicType::Number(Number::Float(*lhs as f64 / rhs))),
            _ => Err(format!("TypeError: Can't add {:?} + {:?}",self,rhs))
        }
    }
}

impl fmt::Display for DynamicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DynamicType::Void => write!(f,"(void)"),
            DynamicType::String(string) => write!(f,"{}",string),
            DynamicType::Number(number) => match number {
                Number::Integer(num) => write!(f,"{}",num),
                Number::Float(num) => write!(f,"{}",num),
            },
        }
    }
}

impl DynamicType {
    pub fn from_token<'a>(token: &Token<'a>) -> DynamicTypeResult {
        match token.ttype {
            TokenType::NumberLiteral => {
                if let Ok(integer) = token.text.parse::<i64>() {
                    Ok(DynamicType::Number(Number::Integer(integer)))
                } else if let Ok(float) = token.text.parse::<f64>() {
                    Ok(DynamicType::Number(Number::Float(float)))
                } else {
                    Err(format!("Failed to convert {} into a number!",token.text))
                }
            },
            TokenType::StringLiteral => {
                Ok(DynamicType::String(token.text[1..token.text.len()-1].to_string()))
            },
            _ => Err(format!("Failed to create value from {:?}", token))
        }
    }
}
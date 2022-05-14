use crate::tokenize::{Token,TokenType};
use core::num;
use std::default;
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
    Number(Number),
    Bool(bool)
}

pub type DynamicTypeResult = Result<DynamicType,String>;

impl Default for DynamicType {
    fn default() -> Self {
        Self::Void
    }
}

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

impl PartialEq<bool> for DynamicType {
    fn eq(&self, other: &bool) -> bool {
        match &self {
            DynamicType::Bool(bool) => bool == other,
            _ => panic!("TypeError: {:?} not comparable to bool",self)
        }
    }
}

impl PartialEq for DynamicType {
    fn eq(&self, other: &Self) -> bool {
        match(&self,&other) {
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Integer(rhs))) => *lhs == *rhs,
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Float(rhs))) => *lhs == *rhs,
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Integer(rhs))) => *lhs == *rhs as f64,
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Float(rhs))) => *lhs as f64 == *rhs,
            (DynamicType::Bool(lhs),DynamicType::Bool(rhs)) => lhs == rhs,
            (DynamicType::String(lhs),DynamicType::String(rhs)) => lhs == rhs,
            _ => panic!("TypeError: Can't compare {:?} == {:?}",self,other)
        }
    }
}

impl PartialOrd for DynamicType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match(&self,&other) {
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Integer(rhs))) => lhs.partial_cmp(rhs),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Float(rhs))) => lhs.partial_cmp(rhs),
            (DynamicType::Number(Number::Float(lhs)),DynamicType::Number(Number::Integer(rhs))) => lhs.partial_cmp(&(*rhs as f64)),
            (DynamicType::Number(Number::Integer(lhs)),DynamicType::Number(Number::Float(rhs))) => (*lhs as f64).partial_cmp(rhs),
            _ => None
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
            DynamicType::Bool(bool) => write!(f,"{}",bool),
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
            TokenType::Keyword => match token.text {
                "true" => Ok(DynamicType::Bool(true)),
                "false" => Ok(DynamicType::Bool(false)),
                _ => Err(format!("{:?} is not a value!", token))
            }
            _ => Err(format!("Failed to create value from {:?}", token))
        }
    }
}

impl From<i32> for DynamicType {
    fn from(val:i32) -> Self {
        DynamicType::Number(Number::Integer(val as i64))
    }
}
impl From<i64> for DynamicType {
    fn from(val:i64) -> Self {
        DynamicType::Number(Number::Integer(val))
    }
}
impl From<f32> for DynamicType {
    fn from(val:f32) -> Self {
        DynamicType::Number(Number::Float(val as f64))
    }
}
impl From<f64> for DynamicType {
    fn from(val:f64) -> Self {
        DynamicType::Number(Number::Float(val))
    }
}
impl From<bool> for DynamicType {
    fn from(val:bool) -> Self {
        DynamicType::Bool(val)
    }
}
impl From<String> for DynamicType {
    fn from(val:String) -> Self {
        DynamicType::String(val)
    }
}

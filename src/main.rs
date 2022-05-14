use std::{vec::Vec, rc::Rc, thread::{sleep_ms, sleep}, time, cell::RefCell, collections::HashMap};
mod tokenize;
mod parser;
mod ast;
mod types;
mod eval;
use tokenize::*;
use parser::*;
use ast::*;
use eval::*;
use types::*;

fn literal_types(token: &Token) -> &'static str {
    match token.ttype {
        TokenType::NumberLiteral => "Number",
        TokenType::StringLiteral => "String",
        TokenType::Keyword => match token.text {
            "true" | "false" => "Bool",
            _ => panic!("Keyword {:?} is not a literal!",token)
        }
        _ => panic!("Expected a literal token! Got {:?} instead", token)
    }
}

fn type_data(scope: &mut Scope) {
    for var in &mut scope.data {
        if let Some(expr) = &mut var.value {
            match **expr {
                Expression::Literal(token) => {
                    var.variable.var_type = Some(Type {
                        path: vec![literal_types(&token)]
                    })
                },
                _ => ()
            }
        }
    }
}

fn fib(n: i64) -> i64 {
    if n > 2 {
        let a = fib(n-1);
        let b = fib(n-2);
        a + b
    } else {
        let c = 1;
        c
    }
}

fn main() {
    let test = include_str!("hello.reduce");
    println!("=== TOKENIZING ===");
    let tokens: Vec<Token> = TokenIterator::new(test).collect();
    println!("{:?}",tokens);
    println!("=== PARSING ===");
    let mut ast = parse_scope(&mut tokens.into_iter().peekable()).unwrap();
    println!("{:?}",ast);
    println!("=== TYPE INFERENCE ===");
    type_data(&mut ast);
    println!("=== AST ===");
    println!("Imports:");
    for imp in &ast.imports {
        println!("\t{}",imp.path.iter().map(|x|x.text).collect::<Vec<&str>>().join("::"));
    }
    println!("Data:");
    for var in &ast.data {
        let typestr = if let Some(var_type) = &var.variable.var_type {
            format!("{}",var_type)
        } else {
            "None".into()
        };
        if let Some(ref value) = var.value {
            println!("\t{}: {} = {}",var.variable.name.text, typestr, value);
        } else {
            println!("\t{}: {}",var.variable.name.text, typestr);
        }
        
    }
    println!("Statements:");
    for expr in &ast.statements {
        println!("{} => {:?}",expr,expr);
    }
    println!("=== EVAL ===");
    let mut frame = StackFrame::new();
    let mut native = NativeFunctions::new();
    let mut defined = RefCell::new(HashMap::new());
    native.register_builtins();
    let result = eval_scope(&mut frame, &native, &mut defined, &ast);
    println!("result: {:?}", result);
    /*println!("invoking fib until it returns 100 or more");
    let mut num = 1;
    while let Some(result) = invoke_fn(&mut frame, &native, &mut defined, "fib", &vec![num.into()]) {
        match result {
            Ok(value) => {
                println!("{}",value);
                if value > 99.into() {
                    break;
                }
            },
            Err(msg) => println!("{}",msg)
        }
        num+=1;
    }*/
    let number = 16;
    println!("invoking fib({})!",number);
    let start = std::time::Instant::now();
    let result = invoke_fn(&mut StackFrame::new(), &native, &mut defined, "fib", &vec![number.into()]);
    let duration = start.elapsed();
    println!("Calculating result {:?} took {:?}",result.unwrap(),duration);
    let native_start = std::time::Instant::now();
    let native_result = fib(number);
    let native_duration = native_start.elapsed();
    println!("Calculating in rust returned {} in {:?}", native_result, native_duration);
}

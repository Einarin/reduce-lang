use std::vec::Vec;
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

fn main() {
    let test = include_str!("hello.reduce");
    println!("=== TOKENIZING ===");
    let tokens: Vec<Token> = TokenIterator::new(test).collect();
    println!("{:?}",tokens);
    println!("=== PARSING ===");
    let mut ast = parse(&mut tokens.into_iter()).unwrap();
    println!("=== TYPE INFERENCE ===");
    type_data(&mut ast);
    println!("=== AST ===");
    println!("Imports:");
    for imp in &ast.imports {
        println!("\t{}",imp.path.iter().map(|x|x.text).collect::<Vec<&str>>().join(":"));
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
    let mut context = Context::new();
    context.functions.insert("print".to_owned(),Box::new(|x: Vec<DynamicType>|{
        println!("{:?}",x[0]);
        Ok(DynamicType::Void)
    }));
    let result = eval_scope(&mut context, &ast);
    println!("result: {:?}", result);
}

use std::vec::Vec;
mod tokenize;
mod parser;
use tokenize::*;
use parser::*;

fn main() {
    let test = include_str!("hello.reduce");
    let tokens: Vec<Token> = TokenIterator::new(test).collect();
    println!("{:?}",tokens);
    parse(&mut tokens.into_iter()).unwrap();
}

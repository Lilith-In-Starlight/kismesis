use std::fs::read_to_string;

mod lexer;
mod parser;


fn main() {
    let tokens = lexer::tokenize(&read_to_string("main.ks").unwrap().replace('\r', ""));
    println!("{:#?}", parser::file(&tokens));
}

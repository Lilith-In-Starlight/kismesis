use std::fs::read_to_string;

mod compiler;

fn main() {
    let tokens = compiler::lexer::tokenize(&read_to_string("main.ks").unwrap().replace('\r', ""));
    println!("{:#?}", compiler::parser::file(&tokens));
}

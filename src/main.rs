use std::fs::read_to_string;

mod compiler;

fn main() {
    let output = compiler::compile_text(&read_to_string("main.ks").unwrap());
    println!("{}", output);
}

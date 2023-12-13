use lexer::Token;

mod lexer;
mod parser;


fn main() {
    let tokens = lexer::tokenize("<uwu bar='bleh'\n\tcasdfas<b | sadfasd>\n>");
}

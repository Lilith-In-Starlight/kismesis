use std::fs;

use self::{options::Settings, reporting::DrawingInfo, parser::types::ParsedFile};

mod errors;
pub(crate) mod html;
pub(crate) mod lexer;
pub(crate) mod options;
pub(crate) mod parser;
mod reporting;

pub fn compile_text(string: &str) -> String {
    let tokens = lexer::tokenize(string);
    let tree = match parser::file(tokens) {
        Ok(val) => val,
        Err((x, tokens)) => return reporting::draw_error(&x.unpack(), &DrawingInfo::from(&tokens)),
    };
    let settings = Settings::new();
    let html = html::generate_html(&tree, None, &settings);
    match html {
        Ok(x) => x.to_string_forced(),
        Err(x) => reporting::draw_packed_error(&x),
    }
}

pub fn compile_project() {
    let main_template: ParsedFile = parser::file(lexer::tokenize(&fs::read_to_string("templates/main.ks").unwrap())).unwrap();
    let mut file_to_template: ParsedFile = parser::file(lexer::tokenize(&fs::read_to_string("input/test.ks").unwrap())).unwrap();
    file_to_template.template = Some(&main_template);
    let mut errors = Vec::new();

    let settings = Settings::new();
    match html::generate_html(&file_to_template, None, &settings) {
        Ok(x) => println!("{}", x.to_string_forced()),
        Err(x) => errors.push(x),
    }
}

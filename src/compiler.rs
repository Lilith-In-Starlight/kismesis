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
    let html = html::generate_html(&tree, &settings);
    match html {
        Ok(x) => x,
        Err(x) => reporting::draw_packed_error(&x),
    }
}

fn compile_project() {
    let parsed_files: Vec<ParsedFile> = Vec::new();
    let mut errors = Vec::new();

    let settings = Settings::new();
    for x in parsed_files.iter() {
        match html::generate_html(x, &settings) {
            Ok(_) => continue,
            Err(x) => errors.push(x),
        }
        
    }
}

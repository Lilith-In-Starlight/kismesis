use self::{options::Settings, reporting::DrawingInfo};

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod html;
pub(crate) mod options;
mod errors;
mod reporting;

pub fn compile_text(string: &str) -> String {
	let tokens = lexer::tokenize(string);
	let tree = match parser::file(&tokens) {
		Ok((val, _)) => val,
		Err(x) => return reporting::draw_error(&x.unpack(), &DrawingInfo::from(&tokens)),
	};
	let settings = Settings::new();
	let html = html::generate_html(&tree, &settings);
	match html {
		Ok(x) => x,
		Err(x) => reporting::draw_packed_error(&x),
	}
}

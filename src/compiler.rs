use self::options::Settings;

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod html;
pub(crate) mod options;
mod reporting;

pub fn compile_text(string: &str) -> String {
	let tokens = lexer::tokenize(string);
	let tree = parser::file(&tokens).unwrap().0;
	let settings = Settings::new();
	let html = html::generate_html(&tree, &settings);
	match html {
		Ok(x) => x,
		Err(x) => reporting::draw_packed_error(&x),
	}
}

use self::options::Settings;

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod html;
pub(crate) mod options;

pub fn compile_text(string: &str) -> String {
	let tokens = lexer::tokenize(string);
	let tree = parser::file(&tokens).unwrap().0;
	let html = html::generate_html(&tree, Settings::new());
	println!("{:#?}", &html);
	html.unwrap()
}

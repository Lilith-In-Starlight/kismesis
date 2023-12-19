use self::options::Settings;

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod html;
pub(crate) mod options;

pub fn compile_text(string: &str) -> String {
	let tokens = lexer::tokenize(string);
	let tree = parser::file(&tokens).unwrap().0;
	let settings = Settings::new();
	let html = html::generate_html(&tree, &settings);
	println!("{:#?}", &html);
	html.unwrap()
}

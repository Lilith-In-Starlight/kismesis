use self::{options::Settings, html::Inside, reporting::DrawingInfo};

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
		Err(x) => match x {
			Inside::In(x) => {
				let x = x.first().unwrap();
				println!("{}", reporting::get_error_graph(&x.error, &DrawingInfo::from(&tokens)));
				panic!()
			},
			_ => panic!(),
		},
	}
}

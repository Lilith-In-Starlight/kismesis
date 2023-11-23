mod parser;
mod compiler_options;
mod lexer;
use compiler_options::CompilerOptions;

use self::parser::tag_stack::elements::{BodyElems, Param};

pub fn kiss_to_html(s: &str) -> Result<String, &'static str>{
	let parsed_file = parser::get_ast(s, CompilerOptions::default())?;
	let output = String::new();

	for node in parsed_file.body.iter() {
		println!("{}", ast_to_html(node, 0, &CompilerOptions::default()));
	}

	Ok(output)
}

pub fn ast_to_html(el: &BodyElems, indent_level: usize, compiler_options: &CompilerOptions) -> String {
	let mut output = String::new();
	for _ in 0..indent_level { output.push('\t') }
	match el {
		BodyElems::ContentTag { name, params, children } => {
			output.push('<');
			output.push_str(&name);

			if !params.is_empty() {
				output.push(' ');
				output.push_str(&get_param_string(params));
			}

			output.push('>');

			for child in children.iter() {
				output.push('\n');
				output.push_str(&ast_to_html(child, indent_level + 1, compiler_options));
			}

			if !children.is_empty() { 
				output.push('\n');
				for _ in 0..indent_level { output.push('\t') }
			}
			output.push_str("</");
			output.push_str(&name);
			output.push('>');
		}
		BodyElems::String(s) => output.push_str(s),
		BodyElems::MacroCall { .. } => todo!("Macro call into html"),
		BodyElems::ValueTag(_) => todo!("Value tags into html"),
	}
	output
}

fn get_param_string(params: &Vec<Param>) -> String {
	let mut output = String::new();
	for param in params.iter() {
		output.push(' ');
		output.push_str(&param.name);
		output.push_str("=\"");
		output.push_str(&param.value);
		output.push('"');
	}
	output
}
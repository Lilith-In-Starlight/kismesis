pub(crate) mod parser;
mod compiler_options;
pub(crate) mod lexer;

use crate::errors::{KismesisError, KissParserErrorState};
use compiler_options::CompilerOptions;

use self::parser::{tag_stack::elements::{BodyElems, Param}, TokenScanner};

pub fn kiss_to_html(s: &str) -> Result<String, (TokenScanner, Option<KissParserErrorState>, Vec<KissParserErrorState>)>{
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
			if compiler_options.is_only_closer(&name) {
				output.push_str("/ ");
			}
			output.push_str(&name);

			if !params.is_empty() {
				output.push(' ');
				output.push_str(&get_param_string(params));
			}

			output.push('>');

			// In case not having a body and being one sided ever stop being opposites
			if compiler_options.is_one_sided(&name) && !compiler_options.has_body(&name) { return output }
			
			if compiler_options.has_body(&name) {
				for child in children.iter() {
					if !compiler_options.is_inline(&name) { output.push('\n') }
					let new_indent_level = if compiler_options.is_inline(&name) {0} else {indent_level + 1};
					output.push_str(&ast_to_html(child, new_indent_level, compiler_options));
				}
			}

			if compiler_options.is_one_sided(&name) { return output }

			if !children.is_empty() && !compiler_options.is_inline(&name) { 
				if !compiler_options.is_inline(&name) { output.push('\n') }
				for _ in 0..indent_level { output.push('\t') }
			}
			output.push_str("</");
			output.push_str(&name);
			output.push('>');
		},
		BodyElems::String(s) => output.push_str(s),
		BodyElems::MacroDef { .. } => todo!("Error messages for the html builder"),
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
	let _ = output.remove(0);
	output
}

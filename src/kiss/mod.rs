pub(crate) mod parser;
mod compiler_options;
pub(crate) mod lexer;

use std::collections::HashMap;

use crate::errors::{UnrecoverableError, KismesisError, HtmlGenerationError, CompilerErrorReport, ErrorState};
use compiler_options::CompilerOptions;

use self::parser::{tag_stack::elements::{BodyElems, Param}, TokenScanner, MacroArray};

pub fn kiss_to_html(s: &str) -> Result<String, CompilerErrorReport<HtmlGenerationError>> {
	let parsed_file = parser::get_ast(s, CompilerOptions::default())?;
	let mut output = String::new();

	let errors: Vec<ErrorState<HtmlGenerationError>> = Vec::new();
 
	for node in parsed_file.body.iter() {
		let state = HtmlCreationState {
			macros: &parsed_file.macros,
			indent_level: 0,
			compiler_options: &CompilerOptions::default(),
			variable_scopes: Vec::new(),
		};
		match ast_to_html(node, &state) {
			Ok(x) => {
				output.push_str(&x);
				output.push('\n');
			},
			Err(err) => {
				println!("{:#?}", err);
				todo!("Handle errors when generating html");
			}
		}
	}
	println!("{}", output);
	Ok(output)
}

#[derive(Debug, Clone)]
pub struct HtmlCreationState<'a> {
	macros: &'a MacroArray,
	indent_level: usize,
	compiler_options: &'a CompilerOptions,
	variable_scopes: Vec<HashMap<String, String>>,
}

pub fn ast_to_html(el: &BodyElems, state: &HtmlCreationState) -> Result<String, HtmlGenerationError> {
	let mut output = String::new();
	match el {
		BodyElems::ContentTag { name, params, children } => {
			for _ in 0..state.indent_level { output.push('\t') }
			output.push('<');
			if state.compiler_options.is_only_closer(&name) {
				output.push_str("/ ");
			}
			output.push_str(&name);

			if !params.is_empty() {
				output.push(' ');
				output.push_str(&get_param_string(params));
			}

			output.push('>');

			// In case not having a body and being one sided ever stop being opposites
			if state.compiler_options.is_one_sided(&name) && !state.compiler_options.has_body(&name) { return Ok(output) }
			
			if state.compiler_options.has_body(&name) {
				for child in children.iter() {
					let mut new_state = state.clone();
					if !state.compiler_options.is_inline(&name) {
						output.push('\n');
						new_state.indent_level += 1;
					}
					let new_state = new_state;
					output.push_str(&ast_to_html(child, &new_state)?);
				}
			}

			if state.compiler_options.is_one_sided(&name) { return Ok(output) }

			if !children.is_empty() && !state.compiler_options.is_inline(&name) { 
				if !state.compiler_options.is_inline(&name) { output.push('\n') }
				for _ in 0..state.indent_level { output.push('\t') }
			}
			output.push_str("</");
			output.push_str(&name);
			output.push('>');
		},
		BodyElems::String(s) => output.push_str(s),
		BodyElems::MacroDef { .. } => todo!("Error messages for the html builder"),
		BodyElems::MacroCall { name, .. } => {
			for _ in 0..state.indent_level { output.push('\t') }
			let macro_template = state.macros.get_content().iter().filter(|x| x.get_name() == Some(name)).last();
			let Some(macro_template) = macro_template else { todo!("Create error display for undefined macros") };
			let mactemp_scope = create_scope_from(&macro_template)?;
			let maccall_scope = create_scope_from(&el)?;
			let children = match macro_template {
				BodyElems::MacroDef { children, .. } => children,
				_ => todo!("Create error display for non-macro template in macro array"),
			};
			for child in children.iter() {
				output = String::new();
				let mut new_state = state.clone();
				new_state.variable_scopes.push(mactemp_scope.clone());
				new_state.variable_scopes.push(maccall_scope.clone());
				output.push_str(&ast_to_html(child, &new_state)?);
			}
		},
		BodyElems::ValueTag(varname) => {
			for scope in state.variable_scopes.iter().rev() {
				if let Some(value) = scope.get(varname) {
					output.push_str(value);
					return Ok(output);
				}
			}
			return Err(KismesisError::UseOfUndefinedVariable.into());
		},
	}
	Ok(output)
}

fn create_scope_from(elem: &BodyElems) -> Result<HashMap<String, String>, UnrecoverableError> {
	let mut new_scope: HashMap<String, String> = HashMap::new();
	match elem {
		BodyElems::MacroCall { args, .. } | BodyElems::MacroDef { args, .. }=> {
			for arg in args.iter() {
				if let Some(value) = &arg.value {
					new_scope.insert(arg.name.clone(), value.clone());
				}
			}
		},
		_ => return Err(UnrecoverableError::CannotMakeIntoScope),
	}
	Ok(new_scope)
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

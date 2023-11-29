pub(crate) mod parser;
mod compiler_options;
pub(crate) mod lexer;

use std::collections::HashMap;

use crate::errors::{UnrecoverableError, KismesisError, HtmlGenerationError, CompilerErrorReport, ErrorState};
use compiler_options::CompilerOptions;

use self::parser::{tag_stack::elements::{BodyElems, Param}, MacroArray};

pub fn kiss_to_html(s: &str) -> Result<String, CompilerErrorReport<HtmlGenerationError>> {
	let (token_scanner, parsed_file) = parser::get_ast(s, CompilerOptions::default())?;
	let mut output = String::new();

	let mut errors: Vec<ErrorState<HtmlGenerationError>> = Vec::new();
 
	for node in parsed_file.body.iter() {
		let state = HtmlCreationState {
			macros: &parsed_file.macros,
			indent_level: 0,
			compiler_options: &CompilerOptions::default(),
			variable_scopes: Vec::new(),
		};
		match &mut ast_to_html(node, &state) {
			Ok(x) => {
				output.push_str(&x.0);
				errors.append(&mut x.1);
				output.push('\n');
			},
			Err(err) => errors.append(err),
		}
	}
	if !errors.is_empty() {
		return Err(CompilerErrorReport { scanner: token_scanner, unresolved: None, resolved: errors})
	}
	Ok(output)
}

#[derive(Debug, Clone)]
pub struct HtmlCreationState<'a> {
	macros: &'a MacroArray,
	indent_level: usize,
	compiler_options: &'a CompilerOptions,
	variable_scopes: Vec<HashMap<String, Option<String>>>,
}

pub fn ast_to_html(el: &BodyElems, state: &HtmlCreationState) -> Result<(String, Vec<ErrorState<HtmlGenerationError>>), Vec<ErrorState<HtmlGenerationError>>> {
	let mut output = String::new();
	let mut errors: Vec<ErrorState<HtmlGenerationError>> = Vec::new();
	match el {
		BodyElems::ContentTag { name, params, children, .. } => {
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
			if state.compiler_options.is_one_sided(&name) && !state.compiler_options.has_body(&name) { return Ok((output, errors)) }
			
			if state.compiler_options.has_body(&name) {
				for child in children.iter() {
					let mut new_state = state.clone();
					if !state.compiler_options.is_inline(&name) {
						output.push('\n');
						new_state.indent_level += 1;
					}
					let new_state = new_state;
					match ast_to_html(child, &new_state) {
						Ok(mut x) => {
							output.push_str(&x.0);
							errors.append(&mut x.1);
						},
						Err(mut x) => errors.append(&mut x),
					}
				}
			}

			if state.compiler_options.is_one_sided(&name) { return Ok((output, errors)) }

			if !children.is_empty() && !state.compiler_options.is_inline(&name) { 
				if !state.compiler_options.is_inline(&name) { output.push('\n') }
				for _ in 0..state.indent_level { output.push('\t') }
			}
			output.push_str("</");
			output.push_str(&name);
			output.push('>');
		},
		BodyElems::String(s) => output.push_str(s),
		BodyElems::MacroDef { line, pos_in_line, .. } => {
			return Err(vec![ErrorState {
				error: KismesisError::TriedMacroDefInTag.into(),
				line_position: *pos_in_line,
				line: *line,
				sub_errors: None,
			}])
		},
		BodyElems::MacroCall { name, line, pos_in_line, children: call_children, .. } => {
			for _ in 0..state.indent_level { output.push('\t') }
			let macro_template = state.macros.get_content().iter().filter(|x| x.get_name() == Some(name)).last();
			let Some(macro_template) = macro_template else { todo!("Create error display for undefined macros") };
			
			let mactemp_scope = match create_scope_from(&macro_template) {
				Ok(x) => x,
				Err(err) => return Err(vec![ErrorState {
					error: err.into(),
					line_position: *pos_in_line,
					line: *line,
					sub_errors: None,
				}])
			};
			let mut maccall_scope = match create_scope_from(&el) {
				Ok(x) => x,
				Err(err) => return Err(vec![ErrorState {
					error: err.into(),
					line_position: *pos_in_line,
					line: *line,
					sub_errors: None,
				}])
			};
			let children = match macro_template {
				BodyElems::MacroDef { children, args, .. } => {
					if args.iter().any(|x| x.name == "kisscontent") {
						let mut content_arg = String::new();
						for child in call_children.iter() {
							let res = ast_to_html(child, state);
							match res {
								Ok((html, mut errs)) => {
									content_arg.push_str(&html);
									errors.append(&mut errs);
								},
								Err(mut errs) => {
									errors.append(&mut errs);
								}
							}
						}
						maccall_scope.insert("kisscontent".to_string(), Some(content_arg));
					}
					children
				},
				_ => todo!("Create error display for non-macro template in macro array"),
			};

			let mut undefined_macro_args: Vec<String> = Vec::new();
			for child in children.iter() {
				output = String::new();
				let mut new_state = state.clone();
				new_state.variable_scopes = vec![mactemp_scope.clone(), maccall_scope.clone()];
				let res = ast_to_html(child, &new_state);
				let result_errors = match res {
					Ok(x) => {
						output.push_str(&x.0);
						x.1
					},
					Err(errs) => errs,
				};
				for err in result_errors {
					match &err.error {
						HtmlGenerationError::KismesisError(KismesisError::UnsetMacroVariable(x)) => undefined_macro_args.push(x.clone()),
						_ => errors.push(err),
					}
				}
			}

			if !undefined_macro_args.is_empty() {
				errors.push(ErrorState { error: KismesisError::UndefinedMacroVariables(undefined_macro_args).into(), line_position: *pos_in_line, line: *line, sub_errors: None });
			}
		},
		BodyElems::ValueTag {name, line, pos_in_line} => {
			for scope in state.variable_scopes.iter().rev() {
				if let Some(value) = scope.get(name) {
					if let Some(value) = value {
						output.push_str(value);
						return Ok((output, errors));
					} else {
						return Err(vec![ErrorState {
							error: KismesisError::UnsetMacroVariable(name.clone()).into(),
							line: *line,
							line_position: *pos_in_line,
							sub_errors: None,
						}])
					}
				}
			}
			return Err(vec![ErrorState {
				error: KismesisError::UseOfUndefinedVariable.into(),
				line_position: *pos_in_line,
				line: *line,
				sub_errors: None
			}])
		},
	}
	Ok((output, errors))
}

fn create_scope_from(elem: &BodyElems) -> Result<HashMap<String, Option<String>>, UnrecoverableError> {
	let mut new_scope: HashMap<String, Option<String>> = HashMap::new();
	match elem {
		BodyElems::MacroCall { args, .. } | BodyElems::MacroDef { args, .. }=> {
			for arg in args.iter() {
				new_scope.insert(arg.name.clone(), arg.value.clone());
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

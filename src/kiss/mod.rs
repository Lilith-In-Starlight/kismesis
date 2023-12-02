pub(crate) mod parser;
pub(crate) mod compiler_options;
pub(crate) mod lexer;

use std::{collections::HashMap, path::Path, fs};

use crate::errors::{ImplementationError, ParsingError, HtmlGenerationError, CompilerErrorReport, ErrorState, SpecialFrom, ErrorReport, report_error, TemplatingError};
use compiler_options::CompilerOptions;

use self::parser::{tag_stack::elements::{Param, ContentTag, Macro, ContentChild, Variable}, MacroArray, ParsedFile, TokenScanner};

type HtmlErrorVec = Vec<ErrorReport<HtmlGenerationError>>;
type ParserResult = Result<String, CompilerErrorReport<HtmlGenerationError>>;
type ParserVecResult = Result<(String, HtmlErrorVec), HtmlErrorVec>;

pub fn parse_template(path: &Path) -> Result<ParsedFile, TemplatingError>{
	let file_string = match fs::read_to_string(path) {
		Ok(x) => x,
		Err(x) => return Err(TemplatingError::IOError(x, path.to_path_buf())),
	};
	match parser::get_ast(&file_string, CompilerOptions::for_templates()) {
		Ok((token_scanner, parsed_file, errors)) => {
			match errors {
				Some(errors) => {
					report_error(path, &token_scanner, None, errors.into_iter().map(|x| x.from()).collect());
					Err(TemplatingError::ParseFailed(path.to_path_buf()))
				}
				None => Ok(parsed_file),
			}
		},
		Err((token_scanner, recovered, unrecoverable)) => {
			report_error(path, &token_scanner, Some(unrecoverable.from()), recovered.into_iter().map(|x| x.from()).collect());
			Err(TemplatingError::ParseFailed(path.to_path_buf()))
		}
	}
}

pub fn parse_file(path: &Path) -> Result<(TokenScanner, ParsedFile), TemplatingError>{
	let file_string = match fs::read_to_string(path) {
		Ok(x) => x,
		Err(x) => return Err(TemplatingError::IOError(x, path.to_path_buf())),
	};
	match parser::get_ast(&file_string, CompilerOptions::default()) {
		Ok((token_scanner, parsed_file, errors)) => {
			match errors {
				Some(errors) => {
					report_error(path, &token_scanner, None, errors.into_iter().map(|x| x.from()).collect());
					Err(TemplatingError::ParseFailed(path.to_path_buf()))
				}
				None => Ok((token_scanner, parsed_file)),
			}
		},
		Err((token_scanner, recovered, unrecoverable)) => {
			report_error(path, &token_scanner, Some(unrecoverable.from()), recovered.into_iter().map(|x| x.from()).collect());
			Err(TemplatingError::ParseFailed(path.to_path_buf()))
		}
	}
}


pub fn kiss_to_html(s: &str) -> ParserResult {
	let (token_scanner, parsed_file, errors) = match parser::get_ast(s, CompilerOptions::for_templates()) {
		Ok(x) => x,
		Err((scanner, recovered, unrecovered)) => return Err(CompilerErrorReport {
			scanner,
			unresolved: Some(unrecovered.from()),
			resolved: recovered.into_iter().map(|x| x.from()).collect(),
		}),
	};
	let mut errors = match errors {
		Some(x) => x.into_iter().map(|x|x.from()).collect(),
		None => Vec::new(),
	};
	match to_html(token_scanner, parsed_file) {
		Ok(x) => Ok(x),
		Err(mut x) => {
			x.resolved.append(&mut errors);
			return Err(x)
		}
	}
}

pub fn to_html(token_scanner: TokenScanner, parsed_file: ParsedFile) -> ParserResult {
	let mut output = String::new();
	let mut errors = Vec::new();
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

pub fn ast_to_html(el: &ContentChild, state: &HtmlCreationState) -> ParserVecResult {
	let mut output = String::new();
	let result = match el {
		ContentChild::ContentTag(tag) => content_tag_to_html(tag, state),
		ContentChild::MacroCall(m) => if !state.compiler_options.is_lambda(&m.name) {
			macro_call_to_html(m, state)
		} else {
			Ok((String::new(), vec![]))
		},
		ContentChild::String(string) => Ok((string.clone(), HtmlErrorVec::new())),
		ContentChild::Variable(variable) => variable_to_html(variable, state)
	};
	let errors = match result {
		Ok(x) => {
			output.push_str(&x.0);
			x.1
		},
		Err(x) => {
			x
		}
	};
	Ok((output, errors))
}

fn variable_to_html(valtag: &Variable, state: &HtmlCreationState) -> ParserVecResult {
	let mut output = String::new();
	for scope in state.variable_scopes.iter().rev() {
		if let Some(value) = scope.get(&valtag.name) {
			if let Some(value) = value {
				output.push_str(value);
				return Ok((output, vec![]));
			} else {
				return Err(vec![ErrorState {
					error: ParsingError::UnsetMacroVariable(valtag.name.clone()).into(),
					line: valtag.line,
					line_position: valtag.pos_in_line,
					sub_errors: None,
				}.into()])
			}
		}
	}
	Err(vec![ErrorState {
		error: ParsingError::UseOfUndefinedVariable.into(),
		line_position: valtag.pos_in_line,
		line: valtag.line,
		sub_errors: None
	}.into()])
}

fn macro_call_to_html(call: &Macro, state: &HtmlCreationState) -> ParserVecResult {
	let mut output = String::new();
	let mut errors: HtmlErrorVec = Vec::new();
	for _ in 0..state.indent_level { output.push('\t') }
	let macro_template = state.macros.get_content().iter().filter(|x| x.name == call.name).last();
	let Some(macro_template) = macro_template else { todo!("Create error display for undefined macros") };
	
	let mactemp_scope = match create_scope_from(macro_template) {
		Ok(x) => x,
		Err(err) => return Err(vec![ErrorState {
			error: err.into(),
			line_position: call.pos_in_line,
			line: call.line,
			sub_errors: None,
		}.into()])
	};
	let mut maccall_scope = match create_scope_from(call) {
		Ok(x) => x,
		Err(err) => return Err(vec![ErrorState {
			error: err.into(),
			line_position: call.pos_in_line,
			line: call.line,
			sub_errors: None,
		}.into()])
	};
	if call.args.iter().any(|x| x.name == "kisscontent") {
		let mut content_arg = String::new();
		for child in call.children.iter() {
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
	} else if !macro_template.children.is_empty() && macro_template.name != "content" {
		errors.push(ErrorState { error: ParsingError::CallBodyNotDeclared.into(), line_position: call.pos_in_line, line: call.line, sub_errors: None }.into())
	}

	let mut undefined_macro_args: Vec<String> = Vec::new();
	output = String::new();
	for child in macro_template.children.iter() {
		let mut new_state = state.clone();
		new_state.variable_scopes = vec![mactemp_scope.clone(), maccall_scope.clone()];
		let res = ast_to_html(child, &new_state);
		let result_errors = match res {
			Ok(x) => {
				output.push_str(&x.0);
				output.push('\n');
				x.1
			},
			Err(errs) => errs,
		};
		for erra in result_errors {
		if let ErrorReport::Stateful(ref err) = erra { match &err.error {
				HtmlGenerationError::KismesisError(ParsingError::UnsetMacroVariable(x)) => undefined_macro_args.push(x.clone()),
				_ => errors.push(erra),
			} }
		}
	}

	if !undefined_macro_args.is_empty() {
		errors.push(ErrorState { error: ParsingError::UndefinedMacroVariables(undefined_macro_args).into(), line_position: call.pos_in_line, line: call.line, sub_errors: None }.into());
	}
	Ok((output, errors))
}

fn content_tag_to_html(tag: &ContentTag, state: &HtmlCreationState) -> ParserVecResult {
	let mut output = String::new();
	let mut errors: Vec<ErrorReport<HtmlGenerationError>> = Vec::new();
	for _ in 0..state.indent_level { output.push('\t') }
	output.push('<');
	if state.compiler_options.is_only_closer(&tag.name) {
		output.push_str("/ ");
	}
	output.push_str(&tag.name);

	if !tag.params.is_empty() {
		output.push(' ');
		output.push_str(&get_param_string(&tag.params));
	}

	output.push('>');

	// In case not having a body and being one sided ever stop being opposites
	if state.compiler_options.is_one_sided(&tag.name) && !state.compiler_options.has_body(&tag.name) { return Ok((output, errors)) }
	
	if state.compiler_options.has_body(&tag.name) {
		for child in tag.children.iter() {
			let mut new_state = state.clone();
			if !state.compiler_options.is_inline(&tag.name) {
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

	if state.compiler_options.is_one_sided(&tag.name) { return Ok((output, errors)) }

	if !tag.children.is_empty() && !state.compiler_options.is_inline(&tag.name) { 
		if !state.compiler_options.is_inline(&tag.name) { output.push('\n') }
		for _ in 0..state.indent_level { output.push('\t') }
	}

	if !state.compiler_options.is_only_opener(&tag.name) {
		output.push_str("</");
		output.push_str(&tag.name);
		output.push('>');
	}
	Ok((output, errors))
}

fn create_scope_from(elem: &Macro) -> Result<HashMap<String, Option<String>>, ImplementationError> {
	let mut new_scope: HashMap<String, Option<String>> = HashMap::new();
	for arg in elem.args.iter() {
		new_scope.insert(arg.name.clone(), arg.value.clone());
	}
	Ok(new_scope)
}

fn get_param_string(params: &[Param]) -> String {
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

use std::collections::HashMap;

use crate::{compiler::parser::types::ParsedFile, kismesis::{KisID, Kismesis}};

use super::{
	errors::{ErrorKind, ErrorState},
	options::Settings,
	parser::{
		state::TokenPos,
		types::{
			Attribute, BinFunc, Expression, HtmlNodes, HtmlTag, Macro, PlugCall, Ranged,
			Scoped, StringParts, TextPos, TopNodes, UniFunc, IfTag, ForTag,
		}, errors::{Hint, Hintable, Hints},
	},
};

type CompileResult<'a, T> = Result<T, Vec<ScopedError<CompilerError>>>;

#[derive(Clone, Debug)]
enum OutputTypes {
	ContentMark(usize),
	Html(String),
}

#[derive(Clone, Debug)]
pub struct HtmlOutput {
	val: Vec<OutputTypes>,
}

impl HtmlOutput {
	pub fn new() -> Self {
		Self { val: Vec::new() }
	}
	pub fn new_content(indents: usize) -> Self {
		Self {
			val: vec![OutputTypes::ContentMark(indents)],
		}
	}
	pub fn is_empty(&self) -> bool {
		self.val.is_empty()
	}
	fn push_string<T>(&mut self, new: T)
	where
		T: Into<String>,
	{
		let new = new.into();
		let Some(top) = self.val.last_mut() else {
			self.val.push(OutputTypes::Html(new));
			return;
		};
		match top {
			OutputTypes::Html(ref mut old_string) => old_string.push_str(&new),
			_ => self.val.push(OutputTypes::Html(new)),
		}
	}

	fn push_output(&mut self, new: &mut HtmlOutput) {
		self.val.append(&mut new.val)
	}

	pub fn to_string_forced(&self) -> String {
		let mut output = String::new();
		for x in self.val.iter() {
			match x {
				OutputTypes::ContentMark(_) => output.push_str("<content!>"),
				OutputTypes::Html(string) => output.push_str(string),
			}
		}
		output
	}

	pub fn to_string(&self) -> Result<String, CompilerError> {
		let mut output = String::new();
		for x in self.val.iter() {
			match x {
				OutputTypes::ContentMark(_) => return Err(CompilerError::ContentTagInOutput),
				OutputTypes::Html(string) => output.push_str(string),
			}
		}
		Ok(output)
	}
}

#[derive(Clone)]
struct GenerationState<'a> {
	options: &'a Settings,
	variable_scopes: VariableScope<'a>,
	macro_templates: HashMap<String, Scoped<'a, &'a Macro>>,
	indent: usize,
	scope: KisID,
}

type ValueRef<'a> = Scoped<'a, Option<&'a Ranged<Expression>>>;

type VariableScope<'a> = HashMap<String, ValueRef<'a>>;

impl<'a> GenerationState<'a> {
	pub(crate) fn from(
		file: &'a ParsedFile,
		sub_scopes: &[&'a ParsedFile],
		options: &'a Settings,
		engine: &'a Kismesis,
	) -> Self {
		Self {
			options,
			variable_scopes: file.get_variable_scope(&sub_scopes, engine),
			macro_templates: file.get_macro_scope(engine),
			indent: 0,
			scope: file.file_id,
		}
	}
}

pub fn generate_html<'a>(
	file: &'a ParsedFile,
	sub_scopes: Vec<&'a ParsedFile>,
	options: &'a Settings,
	engine: &Kismesis,
) -> CompileResult<'a, HtmlOutput> {
	let state = GenerationState::from(file, &sub_scopes, options, engine);
	let mut errors = Vec::new();
	let mut output = HtmlOutput::new();
	for node in file.body.iter() {
		if !output.is_empty() {
			output.push_string('\n');
		}
		match parse_node(node, &state) {
			Ok(mut string) => output.push_output(&mut string),
			Err(mut error) => errors.append(&mut error),
		}
	}

	if !errors.is_empty() {
		return Err(errors);
	}

	if let Some(template) = file.template.as_ref().and_then(|x| engine.get_template(x.clone())) {
		let mut next_scopes = sub_scopes;
		next_scopes.push(file);
		let template_output = generate_html(template, next_scopes, options, engine)?;
		output.val = template_output
			.val
			.into_iter()
			.flat_map(|x| match x {
				OutputTypes::ContentMark(indents) => {
					let mut out = Vec::new();
					let mut is_first_text = true;
					for x in output.val.iter() {
						match x.clone() {
							OutputTypes::ContentMark(x) => {
								out.push(OutputTypes::ContentMark(x + 1))
							}
							OutputTypes::Html(mut output_string) => {
								if is_first_text {
									output_string =
										format!("{}{}", make_indents(indents), output_string);
									is_first_text = false;
								}
								let output_string = output_string
									.replace('\n', &format!("\n{}", make_indents(indents)));
								out.push(OutputTypes::Html(output_string));
							}
						}
					}
					out
				}
				x => vec![x],
			})
			.collect();
	}

	Ok(output)
}

fn parse_node<'a>(
	node: &'a TopNodes,
	state: &GenerationState<'a>,
) -> CompileResult<'a, HtmlOutput> {
	match node {
		TopNodes::HtmlTag(t) => tag(t, state),
		TopNodes::MacroCall(t) => mac_call(t, state),
		TopNodes::PlugCall(t) => plug_call(t, state),
		TopNodes::Content => Ok(HtmlOutput::new_content(state.indent)),
		TopNodes::Section(_) => Ok(HtmlOutput { val: vec![] }),
		TopNodes::If(x) => if_tag(x, state),
		TopNodes::For(x) => for_tag(x, state),
		TopNodes::Doctype(string) => {
			let mut htmlo = HtmlOutput::new();
			htmlo.push_string(format!("<!DOCTYPE {}>", string));
			Ok(htmlo)
		}
	}
}

fn if_tag<'a>(tag: &'a IfTag, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let value = calculate_expression(&tag.condition, (&state.variable_scopes, state.scope))?;
	let mut output = HtmlOutput::new();
	if value.is_truthy() {
		let mut errors = Vec::new();
		for child in tag.body.iter() {
			output.push_string('\n');
			match parse_html_child(child, &state) {
				Ok(mut string) => output.push_output(&mut string),
				Err(mut error) => errors.append(&mut error),
			}
		}

		output.push_string('\n');
	}
	Ok(output)
}

fn for_tag<'a>(tag: &'a ForTag, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	todo!("FOR tags");
	// Ok(output)
}

fn parse_html_child<'a>(
	node: &'a HtmlNodes,
	state: &GenerationState<'a>,
) -> CompileResult<'a, HtmlOutput> {
	match node {
		HtmlNodes::HtmlTag(t) => tag(t, state),
		HtmlNodes::MacroCall(t) => mac_call(t, state),
		HtmlNodes::PlugCall(t) => plug_call(t, state),
		HtmlNodes::Content => Ok(HtmlOutput::new_content(state.indent)),
		HtmlNodes::If(t) => if_tag(t, state),
		HtmlNodes::For(t) => for_tag(t, state),
		HtmlNodes::String(t) => match parse_kis_string(t, (&state.variable_scopes, state.scope)) {
			Ok(mut x) => {
				let mut a = HtmlOutput {
					val: vec![OutputTypes::Html(make_indents(state.indent))],
				};
				a.push_output(&mut x);
				Ok(a)
			}
			Err(x) => Err(x),
		},
		HtmlNodes::Section(_) => Ok(HtmlOutput { val: vec![] }),
	}
}

fn mac_call<'a>(mac: &'a Macro, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut errors = Vec::new();
	let template = state.macro_templates.get(&mac.name.value).ok_or(vec![
		CompilerError::UndefinedMacroCall.state_at(mac.name.range, state.scope),
	])?;
	let mut new_state = state.clone();
	new_state.variable_scopes = {
		let mut base = template.0.get_argument_scope(template.1);

		base.extend(mac.get_argument_scope(state.scope));
		let mut output = VariableScope::new();
		for arg in base.iter() {
			match arg.1 .0 {
				None => {
					errors.push(
						CompilerError::UnsetArgNoDefault(arg.0.clone())
							.state_at(mac.name.range, state.scope)
							.with_hint(Hints::ArgumentDefinedHere.with_state_at(TextPos::Range(template.0.name.range), template.1))
					);
				}
				Some(value) => {
					let _ = output.insert(arg.0.clone(), (Some(value), template.1));
				}
			}
		}
		output
	};

	if !errors.is_empty() {
		return Err(errors);
	}

	let mut output = HtmlOutput::new();
	for child in template.0.body.iter() {
		if !output.is_empty() {
			output.push_string('\n');
			for _ in 0..state.indent {
				output.push_string('\t');
			}
		}
		match parse_html_child(child, &new_state) {
			Ok(mut string) => output.push_output(&mut string),
			Err(mut error) => errors.append(&mut error),
		}
	}

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}
fn plug_call(_plugin: &PlugCall, _state: &GenerationState) -> ! {
	todo!("Plugin calls to html")
}

fn tag<'a>(tag: &'a HtmlTag, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut errors = Vec::new();
	let mut output = HtmlOutput::new();
	for _ in 0..state.indent {
		output.push_string('\t');
	}
	output.push_string('<');
	output.push_string(&tag.name.value);
	match attribute_string(&tag.attributes, state) {
		Ok(string) => output.push_string(&string),
		Err(mut error) => errors.append(&mut error),
	}
	if state.options.is_only_closer(&tag.name.value) {
		output.push_string(" />");
	} else {
		output.push_string('>');
	}

	if state.options.has_body(&tag.name.value) {
		let mut inline = state.options.is_inline(&tag.name.value) || tag.body.is_empty();
		for child in tag.body.iter() {
			match child {
				HtmlNodes::HtmlTag(x) => {
					if !state.options.is_inline(&x.name.value) {
						inline = false;
						break;
					}
				}
				HtmlNodes::MacroCall(_) | HtmlNodes::PlugCall(_) => {
					inline = false;
					break;
				}
				_ => continue,
			}
		}

		let mut new_state = state.clone();

		if !inline {
			new_state.indent += 1;
		} else {
			new_state.indent = 0;
		}

		for child in tag.body.iter() {
			if !inline {
				output.push_string('\n');
			}
			match parse_html_child(child, &new_state) {
				Ok(mut string) => output.push_output(&mut string),
				Err(mut error) => errors.append(&mut error),
			}
		}

		if !inline {
			output.push_string('\n');
			for _ in 0..state.indent {
				output.push_string('\t');
			}
		}
		output.push_string(&format!("</{}>", tag.name.value))
	}

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn attribute_string<'a>(
	attrs: &Vec<Attribute>,
	state: &GenerationState<'a>,
) -> CompileResult<'a, String> {
	let mut output = String::new();
	let mut errors = Vec::new();
	for attr in attrs {
		output.push(' ');
		match calculate_expression(&attr.value, (&state.variable_scopes, state.scope)) {
			Ok(value_string) => {
				let string = match value_string {
					ExpressionValues::String(x) => match parse_kis_string(&x, (&state.variable_scopes, state.scope)) {
						Ok(parsed_kis_string) => parsed_kis_string.to_string_forced(),
						Err(mut kis_string_errors) => {
							errors.append(&mut kis_string_errors);
							continue
						},
					},
					ExpressionValues::None => {
						errors.push(CompilerError::CantWriteNoneValue.state_at(attr.value.range, state.scope));
						continue
					}
					ExpressionValues::Generic => {
						errors.push(CompilerError::CantWriteGenericValue.state_at(attr.value.range, state.scope));
						continue
					}
					ExpressionValues::Array(_) => {
						errors.push(CompilerError::CantWriteArray.state_at(attr.value.range, state.scope));
						continue
					}
				};
				output.push_str(&format!(
				"{}='{}'",
				attr.name.value,
				string
			))},
			Err(mut error) => errors.append(&mut error),
		}
	}

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn parse_kis_string<'a>(
	string: &[StringParts],
	state: (&VariableScope<'a>, KisID),
) -> CompileResult<'a, HtmlOutput> {
	let mut output = HtmlOutput::new();
	let mut errors = Vec::new();
	for parse in string {
		match parse {
			StringParts::String(x) => output.push_string(x),
			StringParts::Expression(expr) => match calculate_expression(expr, state)? {
				ExpressionValues::String(x) => match parse_kis_string(&x, state) {
					Ok(mut parsed_kis_string) => output.push_output(&mut parsed_kis_string),
					Err(mut kis_string_errors) => errors.append(&mut kis_string_errors),
				},
				ExpressionValues::None => {
					errors.push(CompilerError::CantWriteNoneValue.state_at(expr.range, state.1))
				}
				ExpressionValues::Generic => {
					errors.push(CompilerError::CantWriteGenericValue.state_at(expr.range, state.1))
				}
				ExpressionValues::Array(_) => {
					errors.push(CompilerError::CantWriteArray.state_at(expr.range, state.1))
				}
			},
		}
	}

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

#[derive(Clone, Debug)]
enum ExpressionValues {
	String(Vec<StringParts>),
	None,
	Generic,
	Array(Vec<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopedError<T> {
	pub error: ErrorState<T>,
	pub scope: KisID,
}

impl<T> Hintable for ScopedError<T> {
	fn add_hint(&mut self, hint: Hint) {
        self.error.hints.push(hint);
    }
}

impl ExpressionValues {
	fn is_truthy(&self) -> bool {
		matches!(self, Self::Generic | Self::String(_))
	}
}

fn calculate_expression<'a>(
	expr: &Ranged<Expression>,
	state: (&VariableScope<'a>, KisID),
) -> CompileResult<'a, ExpressionValues> {
	match &expr.value {
		Expression::BinFunc(func, exp1, exp2) => {
			let exp1 = calculate_expression(exp1, state)?;
			let exp2 = calculate_expression(exp2, state)?;
			match func {
				BinFunc::And => {
					if exp1.is_truthy() && exp2.is_truthy() {
						Ok(exp2)
					} else {
						Ok(ExpressionValues::None)
					}
				}
				BinFunc::Or => {
					if exp1.is_truthy() {
						Ok(exp1)
					} else if exp2.is_truthy() {
						Ok(exp2)
					} else {
						Ok(ExpressionValues::None)
					}
				}
			}
		}
		Expression::None => Ok(ExpressionValues::None),
		Expression::UniFunc(func, exp) => {
			let exp = calculate_expression(exp, state)?;
			match func {
				UniFunc::Not => {
					if exp.is_truthy() {
						Ok(ExpressionValues::None)
					} else {
						Ok(ExpressionValues::Generic)
					}
				}
			}
		}
		Expression::Variable(x) => {
			if let Some(var) = state.0.get(x) {
				match var.0 {
					Some(value) => Ok(calculate_expression(value, state)?),
					None => Ok(ExpressionValues::None),
				}
			} else {
				Err(vec![
					CompilerError::UndefinedVariable.state_at(expr.range, state.1)
				])
			}
		}
		Expression::Literal(x) => Ok(ExpressionValues::String(x.clone())),
		Expression::Array(x) => Ok(ExpressionValues::Array(x.clone())),
	}
}

#[derive(Clone, Debug)]
pub enum CompilerError {
	CantWriteArray,
	ContentTagInOutput,
	NoDefaultArgDefinedHere,
	UndefinedVariable,
	CantWriteNoneValue,
	CantWriteGenericValue,
	UnsetArgNoDefault(String),
	UndefinedMacroCall,
}

impl CompilerError {
	fn state_at(self, pos: (TokenPos, TokenPos), scope: KisID) -> ScopedError<Self> {
		ScopedError {
			error: ErrorState {
				error: self,
				hints: vec![],
				text_position: TextPos::Range(pos),
			},
			scope,
		}
	}
}

impl ErrorKind for CompilerError {
	fn get_text(&self) -> String {
		match self {
			Self::CantWriteArray => "This computes to an array, which can't be writen into content.".into(),  
			Self::ContentTagInOutput => {
				"Can't write this file to output due to having a <content!> tag".into()
			}
			Self::NoDefaultArgDefinedHere => "Argument defined here".into(),
			Self::UndefinedVariable => "This variable isn't defined".into(),
			Self::CantWriteNoneValue => {
				"This computes to a Nothing value, which cannot be written into content".into()
			}
			Self::CantWriteGenericValue => {
				"This computes to a Anything value, which cannot be written into content".into()
			}
			Self::UnsetArgNoDefault(arg) => format!(
				"The `{}` argument is unset but the macro definition has no default for it",
				arg
			),
			Self::UndefinedMacroCall => "This macro isn't defined".to_string(),
		}
	}
}

fn make_indents(indents: usize) -> String {
	"\t".repeat(indents)
}

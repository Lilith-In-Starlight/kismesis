use htmlize::escape_all_quotes;
use std::collections::HashMap;

use crate::{
	parser::types::{ParsedFile, Paragraph},
	{KisID, Kismesis},
};

use super::{
	errors::{ErrorKind, ErrorState},
	options::Settings,
	parser::{
		errors::{Hint, Hintable, Hints},
		types::{
			Attribute, BinFunc, Expression, ForTag, HtmlNodes, HtmlTag, IfTag, Macro, PlugCall,
			Ranged, Scoped, StringParts, TextPos, TopNodes, UniFunc,
		},
	},
};

type CompileResult<'a, T> = Result<T, Vec<ScopedError<CompilerError>>>;

#[derive(Clone, Debug)]
pub enum MaybeHtml {
	ContentMark(usize),
	Raw(String),
	Html(String),
}

impl MaybeHtml {
	fn merge(self, output: &HtmlOutput) -> Vec<Self> {
		match self {
			Self::ContentMark(indents) => {
				let mut out = Vec::new();
				let mut is_first_text = true;
				for part in &output.parts {
					match part.clone() {
						Self::ContentMark(x) => {
							out.push(Self::ContentMark(x + indents));
						}
						x @ Self::Raw(_) => out.push(x),
						Self::Html(mut output_string) => {
							if is_first_text {
								output_string =
									format!("{}{}", make_indents(indents), output_string);
								is_first_text = false;
							}
							let output_string = output_string
								.replace('\n', &format!("\n{}", make_indents(indents)));
							out.push(Self::Html(output_string));
						}
					}
				}
				out
			}
			x => vec![x],
		}
	}
}

#[derive(Clone, Debug, Default)]
pub struct HtmlOutput {
	parts: Vec<MaybeHtml>,
}

impl HtmlOutput {
	#[must_use]
	pub const fn new() -> Self {
		Self { parts: Vec::new() }
	}

	#[must_use]
	pub fn new_with_content(indents: usize) -> Self {
		Self {
			parts: vec![MaybeHtml::ContentMark(indents)],
		}
	}

	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.parts.is_empty()
	}
	fn push_string<T>(&mut self, new: T)
	where
		T: Into<String>,
	{
		let appended_string = new.into();
		match self.parts.last_mut() {
			Some(MaybeHtml::Html(old_string)) => old_string.push_str(&appended_string),
			_ => self.parts.push(MaybeHtml::Html(appended_string))
		}
	}

	fn push_output(&mut self, new: &mut Self) {
		self.parts.append(&mut new.parts);
	}

	#[must_use]
	pub fn to_string_forced(&self) -> String {
		let mut output = String::new();
		for part in &self.parts {
			match part {
				MaybeHtml::ContentMark(_) => output.push_str("<content!>"),
				MaybeHtml::Raw(string) | MaybeHtml::Html(string) => output.push_str(string),
			}
		}
		output
	}

	/// Converts the output to string.
	/// # Errors
	/// When the string contains `MaybeHtml::ContentMark`.
	pub fn to_string(&self) -> Result<String, CompilerError> {
		let mut output = String::new();
		for part in &self.parts {
			match part {
				MaybeHtml::ContentMark(_) => return Err(CompilerError::ContentTagInOutput),
				MaybeHtml::Raw(string) | MaybeHtml::Html(string) => output.push_str(string),
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
	force_inline: bool,
}

type ValueRef<'a> = Scoped<'a, (Option<&'a Ranged<Expression>>, TextPos)>;

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
			variable_scopes: file.get_variable_scope(sub_scopes, engine),
			macro_templates: file.get_macro_scope(engine),
			indent: 0,
			scope: file.file_id,
			force_inline: false,
		}
	}
}

/// Generates HTML from a file and all the files that are being templated by it
/// # Errors
/// - Whenever an expression cannot be evaluated
/// - Whenever an expression which doesn't evaluate to a pure String is written to content
pub fn generate_html<'a>(
	file: &'a ParsedFile,
	sub_scopes: Vec<&'a ParsedFile>,
	options: &'a Settings,
	engine: &Kismesis,
) -> CompileResult<'a, HtmlOutput> {
	let state = GenerationState::from(file, &sub_scopes, options, engine);
	let mut errors = Vec::new();
	let mut output = HtmlOutput::new();
	for node in &file.body {
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

	if let Some(template) = file
		.template
		.as_ref()
		.and_then(|x| engine.get_template(x.clone()))
	{
		let mut next_scopes = sub_scopes;
		next_scopes.push(file);
		let template_output = generate_html(template, next_scopes, options, engine)?;
		output.parts = template_output
			.parts
			.into_iter()
			.flat_map(|x| x.merge(&output))
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
		TopNodes::Content => Ok(HtmlOutput::new_with_content(state.indent)),
		TopNodes::If(x) => if_tag(x, state),
		TopNodes::For(x) => for_tag(x, state),
		TopNodes::Raw(string) => Ok(HtmlOutput { parts: vec![MaybeHtml::Raw(escape_all_quotes(string).to_string())] }),
		TopNodes::Doctype(string) => {
			let mut htmlo = HtmlOutput::new();
			htmlo.push_string(format!("<!DOCTYPE {string}>"));
			Ok(htmlo)
		}
		TopNodes::Paragraph(paragraph) => top_paragraph(paragraph,state),
	}
}

fn top_paragraph<'a>(paragraph: &'a Paragraph, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut output = HtmlOutput::new();
	let mut errors = vec![];
	output.push_string(make_indents(state.indent));
	output.push_string("<p>");
	let state = &GenerationState {
		indent: 0,
		force_inline: true,
		..state.clone()
	};
	for child in &paragraph.0 {
		match parse_html_child(child, state) {
			Ok(mut string) => output.push_output(&mut string),
			Err(mut error) => errors.append(&mut error),
		}
	}
	output.push_string("</p>");
	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn child_paragraph<'a>(paragraph: &'a Paragraph, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut output = HtmlOutput::new();
	let mut errors = vec![];
	output.push_string(make_indents(state.indent));
	let state = &GenerationState {
		indent: 0,
		force_inline: true,
		..state.clone()
	};
	for child in &paragraph.0 {
		match parse_html_child(child, state) {
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

fn if_tag<'a>(tag: &'a IfTag, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let value = calculate_expression(&tag.condition, state)?;
	let mut output = HtmlOutput::new();
	let mut errors = Vec::new();
	if value.is_truthy(state)? {
		let inline = get_is_inline(state.force_inline, state, &tag.body);

		match render_children(inline, false, &tag.body, state) {
			Ok(mut x) => output.push_output(&mut x),
			Err(mut x) => errors.append(&mut x),
		}
	}
	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn to_iterator<'a>(
	expr: &'a Ranged<Expression>,
	state: &GenerationState<'a>,
) -> CompileResult<'a, Vec<Ranged<Expression>>> {
	match expr.value {
		Expression::Array(ref x) => Ok(x.clone()),
		Expression::Variable(_) => match calculate_expression(expr, state)? {
			ExpressionValues::Array(x) => Ok(x),
			ExpressionValues::Reference(x, _, _) => to_iterator(&x, state),
			_ => Ok(vec![expr.clone()]),
		},
		_ => Ok(vec![expr.clone()]),
	}
}

fn for_tag<'a>(tag: &'a ForTag, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut output = HtmlOutput::new();
	let mut errors = Vec::new();
	let condition = to_iterator(&tag.iterator, state)?;
	let inline = get_is_inline(state.force_inline, state, &tag.body);

	for expr in &condition {
		let mut variable_scopes = state.variable_scopes.clone();
		variable_scopes.insert(
			tag.variable.value.clone(),
			((Some(expr), tag.variable.range.clone()), state.scope),
		);
		let state = GenerationState {
			variable_scopes,
			..state.clone()
		};

		match render_children(inline, false, &tag.body, &state) {
			Ok(mut x) => output.push_output(&mut x),
			Err(mut x) => errors.append(&mut x),
		}
	}

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn parse_html_child<'a>(
	node: &'a HtmlNodes,
	state: &GenerationState<'a>,
) -> CompileResult<'a, HtmlOutput> {
	match node {
		HtmlNodes::HtmlTag(t) => tag(t, state),
		HtmlNodes::MacroCall(t) => mac_call(t, state),
		HtmlNodes::PlugCall(t) => plug_call(t, state),
		HtmlNodes::Content => Ok(HtmlOutput::new_with_content(state.indent)),
		HtmlNodes::If(t) => if_tag(t, state),
		HtmlNodes::For(t) => for_tag(t, state),
		HtmlNodes::String(t) => match parse_kis_string(t, state) {
			Ok(x) => {
				let mut a = HtmlOutput {
					parts: vec![MaybeHtml::Html(make_indents(state.indent))],
				};
				let x = escape_all_quotes(x).to_string();
				a.push_string(x);
				
				Ok(a)
			}
			Err(x) => Err(x),
		},
		HtmlNodes::Paragraph(paragraph) => child_paragraph(paragraph, state),
		HtmlNodes::Raw(string) => Ok(HtmlOutput { parts: vec![MaybeHtml::Raw(escape_all_quotes(string).to_string())] }),
	}
}

fn mac_call<'a>(mac: &'a Macro, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut errors = Vec::new();
	let template =
		state
			.macro_templates
			.get(&mac.name.value)
			.ok_or_else(|| vec![CompilerError::UndefinedMacroCall
				.with_scope_at(state.scope, mac.name.range.clone())])?;
	let mut new_state = state.clone();
	new_state.variable_scopes = {
		let mut base = template.0.get_argument_scope(template.1);

		base.extend(mac.get_argument_scope(state.scope));
		let mut output = VariableScope::new();
		for arg in &base {
			arg.1 .0 .0.map_or_else(|| {
					errors.push(
						CompilerError::UnsetArgNoDefault(arg.0.clone())
							.with_scope_at(state.scope, mac.name.range.clone())
							.with_hint(
								Hints::ArgumentDefinedHere
									.with_state_at(template.0.name.range.clone(), template.1),
							),
					);
				}, |value| {
					let _ = output.insert(
						arg.0.clone(),
						((Some(value), arg.1 .0 .1.clone()), template.1),
					);
				});
		}
		output
	};

	if !errors.is_empty() {
		return Err(errors);
	}

	let mut inner_body = HtmlOutput::new();
	let mut inner_state = new_state.clone();
	let inline = get_is_inline(new_state.force_inline, &inner_state, &mac.body);
	inner_state.indent = 0;
	match render_children(inline, false, &mac.body, &inner_state) {
		Ok(mut x) => inner_body.push_output(&mut x),
		Err(mut x) => errors.append(&mut x),
	}

	let mut output = HtmlOutput::new();
	let inline = get_is_inline(new_state.force_inline, &new_state, &template.0.body);

	match render_children(inline, false, &template.0.body, &new_state) {
		Ok(mut x) => output.push_output(&mut x),
		Err(mut x) => errors.append(&mut x),
	}

	output.parts = output
		.parts
		.into_iter()
		.flat_map(|x| x.merge(&inner_body))
		.collect();

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}
fn plug_call<'a>(plugin: &'a PlugCall, state: &GenerationState) -> CompileResult<'a, HtmlOutput> {
	let mut output = HtmlOutput::new();
	let mut errors = Vec::new();
	let inline = get_is_inline(state.force_inline, state, &plugin.body);

	match render_children(inline, false, &plugin.body, state) {
		Ok(mut x) => output.push_output(&mut x),
		Err(mut x) => errors.append(&mut x),
	}

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn get_is_inline(mut inline: bool, state: &GenerationState, body: &[HtmlNodes]) -> bool {
	if inline && !state.force_inline {
		for child in body {
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
	}

	inline
}

fn render_children<'a>(inline: bool, first_inline: bool, body: &[HtmlNodes], state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut errors = vec![];
	let mut output = HtmlOutput::new();
	for (idx, child) in body.iter().enumerate() {
		if !inline && (idx != 0 || first_inline) {
			output.push_string('\n');
		}
		match parse_html_child(child, state) {
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

fn tag<'a>(tag: &'a HtmlTag, state: &GenerationState<'a>) -> CompileResult<'a, HtmlOutput> {
	let mut errors = Vec::new();
	let mut output = HtmlOutput::new();
	if !state.force_inline {
		for _ in 0..state.indent {
			output.push_string('\t');
		}
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
		let inline = state.options.is_inline(&tag.name.value) || tag.body.is_empty() || state.force_inline;
		let inline = get_is_inline(inline, state, &tag.body);
		let mut new_state = state.clone();

		if inline {
			new_state.indent = 0;
		} else {
			new_state.indent += 1;
		}

		match render_children(inline, true, &tag.body, &new_state) {
			Ok(mut x) => output.push_output(&mut x),
			Err(mut x) => errors.append(&mut x),
		}

		if !inline {
			output.push_string('\n');
			for _ in 0..state.indent {
				output.push_string('\t');
			}
		}
		output.push_string(&format!("</{}>", tag.name.value));
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
		match calculate_expression(&attr.value, state) {
			Ok(value_string) => {
				let string =
					value_string.to_string(&attr.value.range, state.scope, state)?;
				let string = escape_all_quotes(string).to_string();
				output.push_str(&format!("{}='{}'", attr.name.value, string));
			}
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
	string: &'a [StringParts],
	state: &GenerationState<'a>,
) -> CompileResult<'a, String> {
	let mut output = String::new();
	let mut errors = Vec::new(); // TODO actually use this vector, remove elvis operators below
	for parse in string {
		match parse {
			StringParts::String(x) => output.push_str(x),
			StringParts::Expression(expr) => match calculate_expression(expr, state) {
				Ok(calculated_expression) => {
					match calculated_expression.to_string(&expr.range, state.scope, state) {
						Ok(string) => output.push_str(&string),
						Err(mut x) => errors.append(&mut x),
					}
				}
				Err(mut x) => errors.append(&mut x),
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
	Array(Vec<Ranged<Expression>>),
	Reference(Ranged<Expression>, KisID, TextPos),
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
	fn is_truthy<'a>(&self, state: &GenerationState<'a>) -> CompileResult<'a, bool> {
		match self {
			Self::Generic | Self::String(_) | Self::Array(_) => Ok(true),
			Self::Reference(x, _, _) => calculate_expression(x, state)?.is_truthy(state),
			Self::None => Ok(false),
		}
	}

	fn to_string<'a>(
		&'a self,
		range: &TextPos,
		scope: KisID,
		state: &GenerationState<'a>,
	) -> CompileResult<'a, String> {
		match self {
			Self::String(x) => match parse_kis_string(x, state) {
				Ok(parsed_kis_string) => Ok(parsed_kis_string),
				Err(kis_string_errors) => Err(kis_string_errors),
			},
			Self::None => Err(vec![
				CompilerError::CantWriteNoneValue.with_scope_at(scope, range.clone())
			]),
			Self::Generic => Err(vec![
				CompilerError::CantWriteGenericValue.with_scope_at(scope, range.clone())
			]),
			Self::Array(_) => Err(vec![
				CompilerError::CantWriteArray.with_scope_at(scope, range.clone())
			]),
			Self::Reference(x, id, pos) => {
				match calculate_expression(x, state)?.to_string(range, scope, state) {
					Ok(x) => Ok(x),
					Err(mut x) => {
						x[0].add_hint(Hints::ReferenceToThis.with_state_at(pos.clone(), *id));
						Err(x)
					}
				}
			}
		}
	}
}

fn calculate_expression<'a>(
	expr: &'a Ranged<Expression>,
	state: &GenerationState<'a>,
) -> CompileResult<'a, ExpressionValues> {
	match &expr.value {
		Expression::BinFunc(func, left, right) => {
			let left = calculate_expression(left, state)?;
			let right = calculate_expression(right, state)?;
			match func {
				BinFunc::And => {
					if left.is_truthy(state)? && right.is_truthy(state)? {
						Ok(right)
					} else {
						Ok(ExpressionValues::None)
					}
				}
				BinFunc::Or => {
					if left.is_truthy(state)? {
						Ok(left)
					} else if right.is_truthy(state)? {
						Ok(right)
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
					if exp.is_truthy(state)? {
						Ok(ExpressionValues::None)
					} else {
						Ok(ExpressionValues::Generic)
					}
				}
			}
		}
		Expression::Variable(x) => {
			state.variable_scopes.get(x).map_or_else(|| Err(vec![
					CompilerError::UndefinedVariable.with_scope_at(state.scope, expr.range.clone())
				]), |var| var.0 .0.map_or_else(|| Ok(ExpressionValues::Reference(
						Ranged {
							value: Expression::None,
							range: var.0 .1.clone(),
						},
						var.1,
						var.0 .1.clone(),
					)), |value| Ok(ExpressionValues::Reference(
						value.clone(),
						var.1,
						value.range.clone(),
					))))
		}
		Expression::Literal(x) => Ok(ExpressionValues::String(x.clone())),
		Expression::Array(x) => Ok(ExpressionValues::Array(x.clone())),
	}
}

#[derive(Clone, Debug)]
pub enum CompilerError {
	CantWriteArray,
	ContentTagInOutput,
	UndefinedVariable,
	CantWriteNoneValue,
	CantWriteGenericValue,
	UnsetArgNoDefault(String),
	UndefinedMacroCall,
}

impl ErrorKind for CompilerError {
	fn get_text(&self) -> String {
		match self {
			Self::CantWriteArray => {
				"This computes to an array, which can't be writen into content.".into()
			}
			Self::ContentTagInOutput => {
				"Can't write this file to output due to having a <content!> tag".into()
			}
			Self::UndefinedVariable => "This variable isn't defined".into(),
			Self::CantWriteNoneValue => {
				"This computes to a Nothing value, which cannot be written into content".into()
			}
			Self::CantWriteGenericValue => {
				"This computes to a Anything value, which cannot be written into content".into()
			}
			Self::UnsetArgNoDefault(arg) => format!(
				"The `{arg}` argument is unset but the macro definition has no default for it"
			),
			Self::UndefinedMacroCall => "This macro isn't defined".to_string(),
		}
	}
}

fn make_indents(indents: usize) -> String {
	"\t".repeat(indents)
}

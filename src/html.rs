//! Module containing everything that turns kismesis into html.

use htmlize::escape_all_quotes;
use std::{collections::HashMap, fmt::Display};

use crate::{
	errors::{MaybeStateless, StatelessError},
	parser::types::{Content, FillContent, Paragraph, ParsedFile},
	plugins::PostProcPluginInput,
	KisTemplateId, KisTokenId, Kismesis,
};

use super::{
	errors::ErrorKind,
	options::Settings,
	parser::{
		errors::{Hint, Hintable, Hints},
		types::{
			Attribute, BinFunc, Expression, ForTag, HtmlNodes, HtmlTag, IfTag, Macro, PlugCall,
			Ranged, Scoped, StringParts, TextPos, TopNodes, UniFunc,
		},
	},
};

pub type CompileResult<T> = Result<T, Vec<MaybeUnscoped<CompilerError>>>;

#[derive(Clone, Debug)]
pub enum MaybeRaw {
	Raw(String),
	Html(String),
}

impl MaybeRaw {
	fn merge(self) -> Vec<Self> {
		vec![self]
	}
}

#[derive(Clone, Debug, Default)]
pub struct Output {
	parts: Vec<MaybeRaw>,
}

impl Output {
	#[must_use]
	pub const fn new() -> Self {
		Self { parts: Vec::new() }
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
			Some(MaybeRaw::Html(old_string)) => old_string.push_str(&appended_string),
			_ => self.parts.push(MaybeRaw::Html(appended_string)),
		}
	}

	fn push_output(&mut self, new: Self) {
		for new_part in new.parts {
			match new_part {
				MaybeRaw::Html(x) => {
					self.push_string(x);
				}
				MaybeRaw::Raw(_) => self.parts.push(new_part),
			}
		}
	}
}

impl Display for Output {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for part in &self.parts {
			match part {
				MaybeRaw::Raw(string) | MaybeRaw::Html(string) => write!(f, "{string}")?,
			}
		}
		Ok(())
	}
}

#[derive(Clone)]
struct GenerationState<'a> {
	options: &'a Settings,
	variable_scopes: VariableScope<'a>,
	macro_templates: HashMap<String, Scoped<'a, &'a Macro>>,
	indent: usize,
	scope: KisTokenId,
	bottom_scopes: &'a [ParsedFile],
	force_inline: bool,
	stack_paths: Box<[KisTemplateId]>,
}

type ValueRef<'a> = Scoped<'a, (Option<&'a Ranged<Expression>>, TextPos)>;

type VariableScope<'a> = HashMap<String, ValueRef<'a>>;

impl<'a> GenerationState<'a> {
	pub(crate) fn nth_scope(&self, nth: usize) -> Self {
		let next_scope = &self.bottom_scopes[nth];
		let mut variable_scopes = self.variable_scopes.clone();
		variable_scopes.extend(next_scope.get_local_variable_scope());
		let mut macro_templates = self.macro_templates.clone();
		macro_templates.extend(next_scope.get_local_macro_scope());
		Self {
			variable_scopes,
			macro_templates,
			bottom_scopes: &self.bottom_scopes[0..nth],
			scope: next_scope.file_id,
			..self.clone()
		}
	}
}

#[must_use]
pub fn expand(
	file: &ParsedFile,
	children: Vec<ParsedFile>,
	engine: &Kismesis,
) -> (ParsedFile, Vec<ParsedFile>) {
	let mut file = file.clone();
	if children.last().is_some() {
		file.fill_content(children.len() - 1);
	}

	if let Some(template) = file.template.as_ref().and_then(|x| engine.get_template(x)) {
		let mut children = children;
		children.push(file);
		expand(template, children, engine)
	} else {
		(file, children)
	}
}

/// # Errors
/// When the file can't be compiled into html
pub fn compile(file: &ParsedFile, engine: &Kismesis) -> CompileResult<Output> {
	let first_expansion = expand(file, vec![], engine);
	let current_file = engine
		.get_file(first_expansion.0.file_id)
		.ok_or_else(|| {
			vec![CompilerError::TriedCompileUnregistered(first_expansion.0.file_id).unscoped()]
		})?
		.path
		.clone();

	let (file, contexts) = engine.call_post_processing_plugins(PostProcPluginInput {
		body: first_expansion,
		current_file,
	})?;
	let state = GenerationState {
		options: &engine.settings,
		variable_scopes: file.get_variable_scope(&contexts, engine),
		macro_templates: file.get_macro_scope(engine),
		indent: 0,
		scope: file.file_id,
		bottom_scopes: &contexts,
		force_inline: false,
		stack_paths: contexts.iter().filter_map(|x| x.template.clone()).collect(),
	};

	generate_html(&file, &state)
}

/// Generates HTML from a file and all the files that are being templated by it
/// # Errors
/// - Whenever an expression cannot be evaluated
/// - Whenever an expression which doesn't evaluate to a pure String is written to content
fn generate_html<'a>(file: &'a ParsedFile, state: &GenerationState<'a>) -> CompileResult<Output> {
	let mut errors = Vec::new();
	let mut output = Output::new();
	for node in &file.body {
		if !output.is_empty() {
			output.push_string('\n');
		}
		match parse_node(node, state) {
			Ok(string) => output.push_output(string),
			Err(mut error) => errors.append(&mut error),
		}
	}

	if !errors.is_empty() {
		return Err(errors);
	}

	Ok(output)
}

fn parse_node<'a>(node: &'a TopNodes, state: &GenerationState<'a>) -> CompileResult<Output> {
	match node {
		TopNodes::HtmlTag(t) => tag(t, state),
		TopNodes::MacroCall(t) => mac_call(t, state),
		TopNodes::PlugCall(t) => plug_call(t, state),
		TopNodes::Content(Content {
			position: _,
			content: Some(bottom),
		}) => generate_html(&state.bottom_scopes[*bottom], &state.nth_scope(*bottom)),
		TopNodes::Content(Content {
			position,
			content: None,
		}) => Err(vec![
			CompilerError::ContentTagInOutput.with_scope_at(state.scope, position.clone())
		]),
		TopNodes::If(x) => if_tag(x, state),
		TopNodes::For(x) => for_tag(x, state),
		TopNodes::Raw(string) => Ok(Output {
			parts: vec![MaybeRaw::Raw(escape_all_quotes(string).to_string())],
		}),
		TopNodes::Doctype(string) => {
			let mut htmlo = Output::new();
			htmlo.push_string(format!("<!DOCTYPE {string}>"));
			Ok(htmlo)
		}
		TopNodes::Paragraph(paragraph) => top_paragraph(paragraph, state),
	}
}

fn top_paragraph<'a>(
	paragraph: &'a Paragraph,
	state: &GenerationState<'a>,
) -> CompileResult<Output> {
	let mut output = Output::new();
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
			Ok(string) => output.push_output(string),
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

fn child_paragraph<'a>(
	paragraph: &'a Paragraph,
	state: &GenerationState<'a>,
) -> CompileResult<Output> {
	let mut output = Output::new();
	let mut errors = vec![];
	output.push_string(make_indents(state.indent));
	let state = &GenerationState {
		indent: 0,
		force_inline: true,
		..state.clone()
	};
	for child in &paragraph.0 {
		match parse_html_child(child, state) {
			Ok(string) => output.push_output(string),
			Err(mut error) => errors.append(&mut error),
		}
	}
	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn if_tag<'a>(tag: &'a IfTag, state: &GenerationState<'a>) -> CompileResult<Output> {
	let value = calculate_expression(&tag.condition, state)?;
	let mut output = Output::new();
	let mut errors = Vec::new();
	if value.is_truthy(state)? {
		let inline = get_is_inline(state.force_inline, state, &tag.body);

		match render_children(inline, false, &tag.body, state) {
			Ok(x) => output.push_output(x),
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
) -> CompileResult<Vec<Ranged<Expression>>> {
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

fn for_tag<'a>(tag: &'a ForTag, state: &GenerationState<'a>) -> CompileResult<Output> {
	let mut output = Output::new();
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
			Ok(x) => output.push_output(x),
			Err(mut x) => errors.append(&mut x),
		}
	}

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn parse_html_child<'a>(node: &'a HtmlNodes, state: &GenerationState<'a>) -> CompileResult<Output> {
	match node {
		HtmlNodes::HtmlTag(t) => tag(t, state),
		HtmlNodes::MacroCall(t) => mac_call(t, state),
		HtmlNodes::PlugCall(t) => plug_call(t, state),
		HtmlNodes::Content(Content {
			position: _,
			content: Some(bottom),
		}) => generate_html(&state.bottom_scopes[*bottom], &state.nth_scope(*bottom)),
		HtmlNodes::Content(Content {
			position,
			content: None,
		}) => Err(vec![
			CompilerError::ContentTagInOutput.with_scope_at(state.scope, position.clone())
		]),
		HtmlNodes::If(t) => if_tag(t, state),
		HtmlNodes::For(t) => for_tag(t, state),
		HtmlNodes::String(t) => match parse_kis_string(t, state) {
			Ok(x) => {
				let mut a = Output {
					parts: vec![MaybeRaw::Html(make_indents(state.indent))],
				};
				let x = escape_all_quotes(x).to_string();
				a.push_string(x);

				Ok(a)
			}
			Err(x) => Err(x),
		},
		HtmlNodes::Paragraph(paragraph) => child_paragraph(paragraph, state),
		HtmlNodes::Raw(string) => Ok(Output {
			parts: vec![MaybeRaw::Raw(escape_all_quotes(string).to_string())],
		}),
	}
}

fn mac_call<'a>(mac: &'a Macro, state: &GenerationState<'a>) -> CompileResult<Output> {
	let mut errors = Vec::new();
	let template = state.macro_templates.get(&mac.name.value).ok_or_else(|| {
		vec![CompilerError::UndefinedMacroCall.with_scope_at(state.scope, mac.name.range.clone())]
	})?;
	let mut new_state = state.clone();
	new_state.variable_scopes = {
		let mut base = template.0.get_argument_scope(template.1);

		base.extend(mac.get_argument_scope(state.scope));
		let mut output = VariableScope::new();
		for arg in &base {
			arg.1 .0 .0.map_or_else(
				|| {
					errors.push(
						CompilerError::UnsetArgNoDefault(arg.0.clone())
							.with_scope_at(state.scope, mac.name.range.clone())
							.with_hint(
								Hints::ArgumentDefinedHere
									.with_state_at(template.0.name.range.clone(), template.1),
							),
					);
				},
				|value| {
					let _ = output.insert(
						arg.0.clone(),
						((Some(value), arg.1 .0 .1.clone()), template.1),
					);
				},
			);
		}
		output
	};

	if !errors.is_empty() {
		return Err(errors);
	}

	let mut inner_body = Output::new();
	let mut inner_state = new_state.clone();
	let inline = get_is_inline(new_state.force_inline, &inner_state, &mac.body);
	inner_state.indent = 0;
	match render_children(inline, false, &mac.body, &inner_state) {
		Ok(x) => inner_body.push_output(x),
		Err(mut x) => errors.append(&mut x),
	}

	let mut output = Output::new();
	let inline = get_is_inline(new_state.force_inline, &new_state, &template.0.body);

	match render_children(inline, false, &template.0.body, &new_state) {
		Ok(x) => output.push_output(x),
		Err(mut x) => errors.append(&mut x),
	}

	output.parts = output.parts.into_iter().flat_map(MaybeRaw::merge).collect();

	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}
fn plug_call(plugin: &PlugCall, state: &GenerationState) -> CompileResult<Output> {
	let mut output = Output::new();
	let mut errors = Vec::new();
	let inline = get_is_inline(state.force_inline, state, &plugin.body);

	match render_children(inline, false, &plugin.body, state) {
		Ok(x) => output.push_output(x),
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

fn render_children(
	inline: bool,
	first_inline: bool,
	body: &[HtmlNodes],
	state: &GenerationState<'_>,
) -> CompileResult<Output> {
	let mut errors = vec![];
	let mut output = Output::new();
	for (idx, child) in body.iter().enumerate() {
		if !inline && (idx != 0 || first_inline) {
			output.push_string('\n');
		}
		match parse_html_child(child, state) {
			Ok(string) => output.push_output(string),
			Err(mut error) => errors.append(&mut error),
		}
	}
	if errors.is_empty() {
		Ok(output)
	} else {
		Err(errors)
	}
}

fn tag<'a>(tag: &'a HtmlTag, state: &GenerationState<'a>) -> CompileResult<Output> {
	let mut errors = Vec::new();
	let mut output = Output::new();
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
		let inline =
			state.options.is_inline(&tag.name.value) || tag.body.is_empty() || state.force_inline;
		let inline = get_is_inline(inline, state, &tag.body);
		let mut new_state = state.clone();

		if inline {
			new_state.indent = 0;
		} else {
			new_state.indent += 1;
		}

		match render_children(inline, true, &tag.body, &new_state) {
			Ok(x) => output.push_output(x),
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

fn attribute_string(attrs: &Vec<Attribute>, state: &GenerationState<'_>) -> CompileResult<String> {
	let mut output = String::new();
	let mut errors = Vec::new();
	for attr in attrs {
		output.push(' ');
		match calculate_expression(&attr.value, state) {
			Ok(value_string) => {
				let string = value_string.to_string(&attr.value.range, state.scope, state)?;
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
) -> CompileResult<String> {
	let mut output = String::new();
	let mut errors = Vec::new();
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
	Reference(Ranged<Expression>, KisTokenId, TextPos),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopedError<T> {
	pub error: MaybeStateless<T>,
	pub scope: KisTokenId,
}

impl<T> From<ScopedError<T>> for MaybeUnscoped<T> {
	fn from(value: ScopedError<T>) -> Self {
		Self::Scoped(value)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum MaybeUnscoped<T> {
	Scoped(ScopedError<T>),
	Unscoped(StatelessError<T>),
}

impl<T> From<StatelessError<T>> for MaybeUnscoped<T> {
	fn from(value: StatelessError<T>) -> Self {
		Self::Unscoped(value)
	}
}

impl<T> Hintable for MaybeUnscoped<T> {
	fn get_hints(&self) -> &[Hint] {
		match self {
			Self::Scoped(x) => x.get_hints(),
			Self::Unscoped(x) => x.get_hints(),
		}
	}

	fn get_hints_mut(&mut self) -> &mut [Hint] {
		match self {
			Self::Scoped(x) => x.get_hints_mut(),
			Self::Unscoped(x) => x.get_hints_mut(),
		}
	}

	fn add_hint(&mut self, hint: Hint) {
		match self {
			Self::Scoped(x) => x.add_hint(hint),
			Self::Unscoped(x) => x.add_hint(hint),
		}
	}
}

impl<T> Hintable for ScopedError<T> {
	fn add_hint(&mut self, hint: Hint) {
		self.error.add_hint(hint);
	}

	fn get_hints(&self) -> &[Hint] {
		self.error.get_hints()
	}

	fn get_hints_mut(&mut self) -> &mut [Hint] {
		self.error.get_hints_mut()
	}
}

impl ExpressionValues {
	fn is_truthy(&self, state: &GenerationState<'_>) -> CompileResult<bool> {
		match self {
			Self::Generic | Self::String(_) | Self::Array(_) => Ok(true),
			Self::Reference(x, _, _) => calculate_expression(x, state)?.is_truthy(state),
			Self::None => Ok(false),
		}
	}

	fn to_string<'a>(
		&'a self,
		range: &TextPos,
		scope: KisTokenId,
		state: &GenerationState<'a>,
	) -> CompileResult<String> {
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
						let err = &mut x[0];
						err.add_hint(Hints::ReferenceToThis.with_state_at(pos.clone(), *id));
						err.add_hint(Hints::StackIsThis(state.stack_paths.clone()).stateless());
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
) -> CompileResult<ExpressionValues> {
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
		Expression::Variable(x) => state.variable_scopes.get(x).map_or_else(
			|| {
				Err(vec![
					CompilerError::UndefinedVariable.with_scope_at(state.scope, expr.range.clone())
				])
			},
			|var| {
				var.0 .0.map_or_else(
					|| {
						Ok(ExpressionValues::Reference(
							Ranged {
								value: Expression::None,
								range: var.0 .1.clone(),
							},
							var.1,
							var.0 .1.clone(),
						))
					},
					|value| {
						Ok(ExpressionValues::Reference(
							value.clone(),
							var.1,
							value.range.clone(),
						))
					},
				)
			},
		),
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
	TriedCompileUnregistered(KisTokenId),
	PluginError(String),
	PluginDoesntExist(String),
	ExtismError(String),
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
			Self::TriedCompileUnregistered(id) => {
				format!("Tried to compile a file with an unregistered ID: {id}")
			}
			Self::PluginError(message)
			| Self::PluginDoesntExist(message)
			| Self::ExtismError(message) => message.to_owned(),
		}
	}
}

fn make_indents(indents: usize) -> String {
	"\t".repeat(indents)
}

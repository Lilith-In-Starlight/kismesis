use std::{collections::HashMap, path::Path};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{KisID, KisTemplateID, Kismesis};

use super::state::TokenPos;

pub type Scoped<'a, T> = (T, KisID);
pub type ScopedExpression<'a> = Scoped<'a, (Option<&'a Ranged<Expression>>, TextPos)>;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// A Kismesis string, containing both string literals and expressions
pub enum StringParts {
	String(String),
	Expression(Ranged<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// An HTML attribute, whose value can never be null
pub struct Attribute {
	pub name: Ranged<String>,
	pub value: Ranged<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// A Kismesis argument, which is simular to an attribute but its value is nullable
pub struct Argument {
	pub name: Ranged<String>,
	pub value: Option<Ranged<Expression>>,
}
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// An HTML tag
pub struct HtmlTag {
	/// The Kismesis name of the tag. It might get compiled to something else, like how <container> is compiled as <div>
	pub name: Ranged<String>,
	pub attributes: Vec<Attribute>,
	/// Subtags get compiled as children of the tag
	pub subtags: Vec<HtmlTag>,
	/// Children of the tag
	pub body: Vec<HtmlNodes>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// A Kismesis macro
pub struct Macro {
	pub name: Ranged<String>,
	pub arguments: Vec<Argument>,
	pub body: Vec<HtmlNodes>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// A part of the document denotated by a heading and some content
pub struct Section {
	pub depth: Ranged<usize>,
	pub name: Ranged<Paragraph>,
	pub subtitle: Option<Ranged<Paragraph>>,
	pub content: Vec<Paragraph>,
}

impl Section {
	pub fn into_tag(self) -> HtmlTag {
		let mut tags = Vec::new();
		let hstr = format!("h{}", self.depth.value);
		let title = HtmlTag {
			name: Ranged {
				value: hstr,
				range: self.depth.range,
			},
			attributes: vec![],
			body: vec![HtmlNodes::Paragraph(self.name.value)],
			subtags: vec![],
		};
		let header = match self.subtitle {
			Some(subtitle) => {
				let subtitle = HtmlTag {
					name: Ranged {
						value: String::from("p"),
						range: subtitle.range,
					},
					attributes: vec![],
					body: vec![HtmlNodes::Paragraph(subtitle.value)],
					subtags: vec![],
				};
				HtmlTag {
					name: Ranged {
						value: String::from("hgroup"),
						range: TextPos::Single(TokenPos::new()),
					},
					attributes: vec![],
					body: vec![HtmlNodes::HtmlTag(title), HtmlNodes::HtmlTag(subtitle)],
					subtags: vec![],
				}
			}
			None => title,
		};

		let mut content = Vec::new();

		for x in self.content {
			match (x.0.first(), x.0.len()) {
				(_, 2..) | (Some(HtmlNodes::String(_)), _) => {
					let x = HtmlTag {
						name: Ranged {
							value: "p".to_string(),
							range: TextPos::Single(TokenPos::default()),
						},
						attributes: vec![],
						subtags: vec![],
						body: vec![HtmlNodes::Paragraph(Paragraph(x.0))],
					};
					content.push(HtmlNodes::HtmlTag(x));
				}
				(Some(x), 1) => content.push(x.clone()),
				_ => content.push(HtmlNodes::Paragraph(x)),
			}
		}

		tags.push(HtmlNodes::HtmlTag(header));
		tags.append(&mut content);

		HtmlTag {
			name: Ranged {
				value: String::from("section"),
				range: TextPos::Single(TokenPos::new()),
			},
			attributes: vec![],
			body: tags,
			subtags: vec![],
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The result of a plugin call
pub struct PlugCall {
	pub name: Ranged<String>,
	pub body: Vec<HtmlNodes>,
}
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// All AST nodes that end up in the final HTML in one way or another
pub enum HtmlNodes {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	String(Vec<StringParts>),
	PlugCall(Box<PlugCall>),
	If(IfTag),
	For(ForTag),
	Paragraph(Paragraph),
	Content,
	Raw(String),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Paragraph(pub Vec<HtmlNodes>);

#[derive(Debug, Clone, PartialEq)]
/// All AST nodes that can be at the topmost level of a file
pub enum TopNodes {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	Content,
	Doctype(String),
	If(IfTag),
	For(ForTag),
	Paragraph(Paragraph),
	Raw(String),
}

#[derive(Debug, Clone, PartialEq)]
/// All AST nodes that can be in any level of a file except for the topmost
pub enum BodyTags {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	If(IfTag),
	For(ForTag),
	Content,
	Raw(String),
}

#[derive(Debug, Clone, PartialEq)]
/// All Kismesis tags
pub enum Tag {
	Html(HtmlTag),
	MacroDef(Macro),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	Content,
	Doctype(String),
	If(IfTag),
	For(ForTag),
}

#[derive(Debug, Clone, PartialEq)]
/// All AST nodes
pub enum BodyNodes {
	HtmlTag(HtmlTag),
	MacroDef(Macro),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	LambdaDef(Lambda),
	VarDef(Variable),
	Content,
	SetStmt(String, String),
	Doctype(String),
	If(IfTag),
	For(ForTag),
	Paragraph(Paragraph),
	Raw(String),
}

#[derive(Debug, Clone, PartialEq)]
/// The completely parsed file, including macros, variables, lambdas and a template
pub struct ParsedFile {
	pub file_id: KisID,
	pub body: Vec<TopNodes>,
	pub defined_macros: Vec<Macro>,
	pub defined_variables: Vec<Variable>,
	pub defined_lambdas: Vec<Lambda>,
	pub template: Option<KisTemplateID>,
}

/// An Option with the possibility of something not being set
pub enum VariableOption<T> {
	/// The variable was found and has a value
	Some(T),
	/// The variable was not found
	None,
	/// The variable was found, but it's not set
	Unset,
}

impl ParsedFile {
	pub fn new(file_id: KisID) -> Self {
		Self {
			file_id,
			body: vec![],
			defined_macros: vec![],
			defined_variables: vec![],
			defined_lambdas: vec![],
			template: None,
		}
	}

	pub fn get_macro_template<'a>(
		&'a self,
		engine: &'a Kismesis,
		predicate: impl Fn(&&Macro) -> bool,
	) -> Option<&Macro> {
		self.defined_macros.iter().rfind(&predicate).or_else(|| self
			.template
			.as_ref()
			.and_then(|x| {
				engine
					.get_template(x.clone())
					.unwrap()
					.get_macro_template(engine, &predicate)
			}))
	}

	pub fn get_path_slice<'a>(&'a self, engine: &'a Kismesis) -> Option<&Path> {
		engine.get_file(self.file_id)?.path.as_deref()
	}

	pub fn get_variable_value<'a>(
		&'a self,
		engine: &'a Kismesis,
		predicate: &str,
	) -> VariableOption<&Ranged<Expression>> {
		for var in &self.defined_variables {
			if var.name.value == predicate {
				return VariableOption::Some(&var.value);
			}
		}

		for var in &self.defined_lambdas {
			if var.name.value == predicate {
				match var.value {
					Some(ref value) => return VariableOption::Some(value),
					None => return VariableOption::Unset,
				}
			}
		}

		self.template.as_ref().map_or(VariableOption::None, |template| engine
				.get_template(template.clone())
				.unwrap()
				.get_variable_value(engine, predicate))
	}

	pub fn get_macro_scope<'a>(&'a self, engine: &'a Kismesis) -> HashMap<String, Scoped<&Macro>> {
		let mut output = HashMap::new();
		if let Some(ref template) = self.template {
			output.extend(
				engine
					.get_template(template.clone())
					.unwrap()
					.get_macro_scope(engine),
			);
		}

		output.extend(
			self.defined_macros
				.iter()
				.map(|x| (x.name.value.clone(), (x, self.file_id))),
		);

		output
	}

	pub fn get_variable_scope<'a>(
		&'a self,
		sub_scope: &[&'a Self],
		engine: &'a Kismesis,
	) -> HashMap<String, ScopedExpression> {
		let mut out = HashMap::new();

		if let Some(ref template) = self.template {
			out.extend(
				engine
					.get_template(template.clone())
					.unwrap()
					.get_variable_scope(&[], engine),
			);
		}

		out.extend(self.defined_lambdas.iter().map(|x| {
			if !sub_scope.is_empty() {
				for scope in sub_scope {
					let find = scope
						.defined_variables
						.iter()
						.rfind(|y| y.name.value == x.name.value);
					if let Some(find) = find {
						return (
							find.name.value.clone(),
							((Some(&find.value), find.name.range.clone()), self.file_id),
						);
					}
				}
			}
			(
				x.name.value.clone(),
				((x.value.as_ref(), x.name.range.clone()), self.file_id),
			)
		}));

		out.extend(self.defined_variables.iter().map(|x| {
			(
				x.name.value.clone(),
				((Some(&x.value), x.name.range.clone()), self.file_id),
			)
		}));

		out.into_iter().collect()
	}
}

impl From<Tag> for BodyNodes {
	fn from(value: Tag) -> Self {
		match value {
			Tag::Html(x) => Self::HtmlTag(x),
			Tag::MacroCall(x) => Self::MacroCall(x),
			Tag::MacroDef(x) => Self::MacroDef(x),
			Tag::PlugCall(x) => Self::PlugCall(x),
			Tag::Content => Self::Content,
			Tag::Doctype(x) => Self::Doctype(x),
			Tag::If(x) => Self::If(x),
			Tag::For(x) => Self::For(x),
		}
	}
}

impl From<BodyTags> for BodyNodes {
	fn from(value: BodyTags) -> Self {
		match value {
			BodyTags::HtmlTag(x) => Self::HtmlTag(x),
			BodyTags::MacroCall(x) => Self::MacroCall(x),
			BodyTags::PlugCall(x) => Self::PlugCall(x),
			BodyTags::Content => Self::Content,
			BodyTags::If(x) => Self::If(x),
			BodyTags::For(x) => Self::For(x),
			BodyTags::Raw(x) => Self::Raw(x),
		}
	}
}

impl From<BodyTags> for HtmlNodes {
	fn from(value: BodyTags) -> Self {
		match value {
			BodyTags::HtmlTag(x) => Self::HtmlTag(x),
			BodyTags::MacroCall(x) => Self::MacroCall(x),
			BodyTags::PlugCall(x) => Self::PlugCall(x),
			BodyTags::Content => Self::Content,
			BodyTags::If(x) => Self::If(x),
			BodyTags::For(x) => Self::For(x),
			BodyTags::Raw(x) => Self::Raw(x),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum BinFunc {
	And,
	Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum UniFunc {
	Not,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expression {
	None,
	Variable(String),
	Literal(Vec<StringParts>),
	BinFunc(BinFunc, Box<Ranged<Expression>>, Box<Ranged<Expression>>),
	UniFunc(UniFunc, Box<Ranged<Expression>>),
	Array(Vec<Ranged<Expression>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
	pub name: Ranged<String>,
	pub value: Ranged<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
	pub name: Ranged<String>,
	pub value: Option<Ranged<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Ranged<T> {
	pub value: T,
	pub range: TextPos,
}

impl Ranged<&str> {
	pub fn to_own(&self) -> Ranged<String> {
		Ranged {
			value: self.value.to_owned(),
			range: self.range.clone(),
		}
	}
}

/*pub trait AstNode {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>>;
}

impl AstNode for Macro {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		self.body
			.iter()
			.flat_map(|x| x.find_undefined_vars(defined))
			.collect()
	}
}

impl AstNode for Vec<StringParts> {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		self.iter()
			.flat_map(|x| x.find_undefined_vars(defined))
			.collect()
	}
}

impl AstNode for StringParts {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		match self {
			StringParts::String(_) => Vec::new(),
			StringParts::Expression(x) => x.find_undefined_vars(defined),
		}
	}
}

impl AstNode for Ranged<Expression> {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		match &self.value {
			Expression::None => Vec::new(),
			Expression::Variable(x) => {
				if defined.iter().any(|y| y == x) {
					vec![Ranged {
						value: x.clone(),
						range: self.range,
					}]
				} else {
					vec![]
				}
			}
			Expression::BinFunc(_, x, y) => x
				.find_undefined_vars(defined)
				.into_iter()
				.chain(y.find_undefined_vars(defined))
				.collect(),
			Expression::UniFunc(_, x) => x.find_undefined_vars(defined),
			Expression::Literal(x) => x.find_undefined_vars(defined),
		}
	}
}
*/
/*impl AstNode for HtmlTag {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		self.body
			.iter()
			.flat_map(|x| x.find_undefined_vars(defined))
			.collect()
	}
}
*/
/*impl AstNode for HtmlNodes {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		match self {
			HtmlNodes::HtmlTag(x) => x.find_undefined_vars(defined),
			HtmlNodes::MacroCall(x) => x.find_undefined_vars(defined),
			HtmlNodes::String(x) => x.find_undefined_vars(defined),
			HtmlNodes::Section(_) => todo!("undefined vars in body"),
			HtmlNodes::PlugCall(_) => Vec::new(),
			HtmlNodes::If(x) => x.body.find_unde
			Self::Content => Vec::new(),
		}
	}
}
*/

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TextPos {
	Single(TokenPos),
	Range((TokenPos, TokenPos)),
	Multi(Vec<TextPos>),
}

impl TextPos {
	pub fn get_start_line(&self) -> usize {
		match self {
			Self::Single(x) => x.get_line(),
			Self::Range(x) => x.0.get_line(),
			Self::Multi(x) => x[0].get_start_line(),
		}
	}

	pub fn get_end_line(&self) -> usize {
		match self {
			Self::Single(x) => x.get_line(),
			Self::Range(x) => x.1.get_line(),
			Self::Multi(x) => unsafe { x.get_unchecked(x.len() - 1).get_end_line() },
		}
	}

	pub fn is_one_line(&self) -> bool {
		self.get_end_line() == self.get_start_line()
	}
}

impl Macro {
	pub fn get_argument_scope(&self, scope: KisID) -> HashMap<String, ScopedExpression> {
		let mut output = HashMap::new();

		output.extend(self.arguments.iter().map(|x| x.value.as_ref().map_or_else(|| (x.name.value.clone(), ((None, x.name.range.clone()), scope)), |value| (
				x.name.value.clone(),
				((Some(value), x.name.range.clone()), scope),
			))));

		output
	}
}

impl HtmlTag {
	/// Turn all subtags into regular HTML tags
	pub fn merge_subtags(mut self) -> Self {
		let mut subtag_stack = self.subtags;
		let Some(top) = subtag_stack.last_mut() else {
			self.subtags = subtag_stack;
			return self;
		};
		top.body = self.body;
		loop {
			let Some(top) = subtag_stack.pop() else { break };
			let Some(top_2) = subtag_stack.last_mut() else {
				subtag_stack.push(top);
				break;
			};
			top_2.body.push(HtmlNodes::HtmlTag(top));
		}

		self.body = subtag_stack.into_iter().map(HtmlNodes::HtmlTag).collect();

		self.subtags = Vec::new();

		self
	}
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct IfTag {
	pub condition: Ranged<Expression>,
	pub body: Vec<HtmlNodes>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ForTag {
	pub variable: Ranged<String>,
	pub iterator: Ranged<Expression>,
	pub body: Vec<HtmlNodes>,
}

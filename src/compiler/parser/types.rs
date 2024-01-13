use std::{collections::HashMap, path::Path};

#[cfg(feature="serde")]
use serde::{Deserialize, Serialize};


use crate::{KisID, KisTemplateID, Kismesis};

use super::state::TokenPos;

pub type Scoped<'a, T> = (T, KisID);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub enum StringParts {
	String(String),
	Expression(Ranged<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct Attribute {
	pub name: Ranged<String>,
	pub value: Ranged<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct Argument {
	pub name: Ranged<String>,
	pub value: Option<Ranged<Expression>>,
}
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct HtmlTag {
	pub name: Ranged<String>,
	pub attributes: Vec<Attribute>,
	pub body: Vec<HtmlNodes>,
	pub subtags: Vec<HtmlTag>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct Macro {
	pub name: Ranged<String>,
	pub arguments: Vec<Argument>,
	pub body: Vec<HtmlNodes>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct Section {
	pub depth: usize,
	pub name: Vec<StringParts>,
	pub subtitle: Option<Vec<StringParts>>,
	pub content: Vec<Vec<HtmlNodes>>,
}

impl Section {
	pub fn to_tag(self) -> HtmlTag {
		let mut tags = Vec::new();
		let hstr = format!("h{}", self.depth);
		let title = HtmlTag {
			name: Ranged {
				value: hstr,
				range: TextPos::Single(TokenPos::new()),
			},
			attributes: vec![],
			body: vec![HtmlNodes::String(self.name)],
			subtags: vec![],
		};
		let header = match self.subtitle {
			Some(subtitle) => {
				let subtitle = HtmlTag {
					name: Ranged {
						value: String::from("p"),
						range: TextPos::Single(TokenPos::new()),
					},
					attributes: vec![],
					body: vec![HtmlNodes::String(subtitle)],
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
			match (x.first().to_owned(), x.len()) {
				(Some(HtmlNodes::String(_)), 1) | (Some(_), 2..) => {
					let r = HtmlTag {
						name: Ranged {
							value: String::from("p"),
							range: TextPos::Single(TokenPos::new()),
						},
						attributes: vec![],
						body: x,
						subtags: vec![],
					};
					content.push(HtmlNodes::HtmlTag(r));
				}
				(Some(x), 1) => content.push(x.clone()),
				_ => continue,
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

pub fn paragraph_str_to_p(vec: Vec<HtmlNodes>) -> HtmlTag {
	HtmlTag {
		name: Ranged {
			value: String::from("p"),
			range: TextPos::Single(TokenPos::new()),
		},
		attributes: vec![],
		body: vec,
		subtags: vec![],
	}
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct PlugCall {
	pub name: Ranged<String>,
	pub body: Vec<HtmlNodes>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub enum HtmlNodes {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	String(Vec<StringParts>),
	PlugCall(Box<PlugCall>),
	Section(Section),
	If(IfTag),
	For(ForTag),
	Content,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopNodes {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	Section(Section),
	Content,
	Doctype(String),
	If(IfTag),
	For(ForTag),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyTags {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	Section(Section),
	If(IfTag),
	For(ForTag),
	Content,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tag {
	HtmlTag(HtmlTag),
	MacroDef(Macro),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	Section(Section),
	Content,
	Doctype(String),
	If(IfTag),
	For(ForTag),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyNodes {
	HtmlTag(HtmlTag),
	MacroDef(Macro),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	String(Vec<StringParts>),
	LambdaDef(Lambda),
	VarDef(Variable),
	Content,
	SetStmt(String, String),
	Section(Section),
	Doctype(String),
	If(IfTag),
	For(ForTag),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFile {
	pub file_id: KisID,
	pub body: Vec<TopNodes>,
	pub defined_macros: Vec<Macro>,
	pub defined_variables: Vec<Variable>,
	pub defined_lambdas: Vec<Lambda>,
	pub template: Option<KisTemplateID>,
}

pub enum VariableOption<T> {
	Some(T),
	None,  // The variable was not found
	Unset, // The variable was found, but it's not set
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
		self.defined_macros.iter().rfind(&predicate).or(self
			.template
			.as_ref()
			.map(|x| {
				engine
					.get_template(x.clone())
					.unwrap()
					.get_macro_template(engine, &predicate)
			})
			.unwrap_or(None))
	}

	pub fn get_path_slice<'a>(&'a self, engine: &'a Kismesis) -> Option<&Path> {
		engine.get_file(self.file_id)?.path.as_deref()
	}

	pub fn get_variable_value<'a>(
		&'a self,
		engine: &'a Kismesis,
		predicate: &str,
	) -> VariableOption<&Ranged<Expression>> {
		for var in self.defined_variables.iter() {
			if var.name.value == predicate {
				return VariableOption::Some(&var.value);
			}
		}

		for var in self.defined_lambdas.iter() {
			if var.name.value == predicate {
				match var.value {
					Some(ref value) => return VariableOption::Some(value),
					None => return VariableOption::Unset,
				}
			}
		}

		match self.template {
			Some(ref template) => engine
				.get_template(template.clone())
				.unwrap()
				.get_variable_value(engine, predicate),
			None => VariableOption::None,
		}
	}

	pub fn get_macro_scope<'a>(&'a self, engine: &'a Kismesis) -> HashMap<String, Scoped<&Macro>> {
		let mut output = HashMap::new();
		if let Some(ref template) = self.template {
			output.extend(
				engine
					.get_template(template.clone())
					.unwrap()
					.get_macro_scope(engine),
			)
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
		sub_scope: &[&'a ParsedFile],
		engine: &'a Kismesis,
	) -> HashMap<String, Scoped<(Option<&Ranged<Expression>>, TextPos)>> {
		let mut out = HashMap::new();

		if let Some(ref template) = self.template {
			out.extend(
				engine
					.get_template(template.clone())
					.unwrap()
					.get_variable_scope(&[], engine),
			)
		}

		out.extend(self.defined_lambdas.iter().map(|x| {
			if !sub_scope.is_empty() {
				for scope in sub_scope.iter() {
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
				(
					x.name.value.clone(),
					((x.value.as_ref(), x.name.range.clone()), self.file_id),
				)
			} else {
				(
					x.name.value.clone(),
					((x.value.as_ref(), x.name.range.clone()), self.file_id),
				)
			}
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
			Tag::HtmlTag(x) => Self::HtmlTag(x),
			Tag::MacroCall(x) => Self::MacroCall(x),
			Tag::MacroDef(x) => Self::MacroDef(x),
			Tag::PlugCall(x) => Self::PlugCall(x),
			Tag::Section(x) => Self::Section(x),
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
			BodyTags::Section(x) => Self::Section(x),
			BodyTags::Content => Self::Content,
			BodyTags::If(x) => Self::If(x),
			BodyTags::For(x) => Self::For(x),
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
			BodyTags::Section(x) => Self::Section(x),
			BodyTags::If(x) => Self::If(x),
			BodyTags::For(x) => Self::For(x),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub enum BinFunc {
	And,
	Or,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub enum UniFunc {
	Not,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
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
#[cfg_attr(feature="serde", derive(Deserialize, Serialize))]
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
#[cfg_attr(feature="serde", derive(Deserialize, Serialize))]
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
	pub fn get_argument_scope(
		&self,
		scope: KisID,
	) -> HashMap<String, Scoped<(Option<&Ranged<Expression>>, TextPos)>> {
		let mut output = HashMap::new();

		output.extend(self.arguments.iter().map(|x| match x.value {
			Some(ref value) => (
				x.name.value.clone(),
				((Some(value), x.name.range.clone()), scope),
			),
			None => (x.name.value.clone(), ((None, x.name.range.clone()), scope)),
		}));

		output
	}
}

impl HtmlTag {
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
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct IfTag {
	pub condition: Ranged<Expression>,
	pub body: Vec<HtmlNodes>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct ForTag {
	pub variable: Ranged<String>,
	pub iterator: Ranged<Expression>,
	pub body: Vec<HtmlNodes>,
}

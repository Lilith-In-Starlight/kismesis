use std::{
	collections::HashMap,
	path::{Path, PathBuf},
};

use crate::compiler::lexer::Token;

use super::state::TokenPos;

pub type Scoped<'a, T> = (T, Scope<'a>);
pub type Scope<'a> = (&'a [Token], Option<&'a Path>);

#[derive(Debug, Clone, PartialEq)]
pub enum StringParts {
	String(String),
	Expression(Ranged<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
	pub(crate) name: Ranged<String>,
	pub(crate) value: Vec<StringParts>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub(crate) name: Ranged<String>,
	pub(crate) value: Option<Vec<StringParts>>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct HtmlTag {
	pub(crate) name: Ranged<String>,
	pub(crate) attributes: Vec<Attribute>,
	pub(crate) body: Vec<HtmlNodes>,
	pub(crate) subtags: Vec<HtmlTag>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Macro {
	pub(crate) name: Ranged<String>,
	pub(crate) arguments: Vec<Argument>,
	pub(crate) body: Vec<HtmlNodes>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Section {
	pub(crate) depth: usize,
	pub(crate) name: Vec<StringParts>,
	pub(crate) subtitle: Option<Vec<StringParts>>,
	pub(crate) content: Vec<Vec<HtmlNodes>>,
}

impl Section {
	pub fn to_tag(self) -> HtmlTag {
		let mut tags = Vec::new();
		let hstr = format!("h{}", self.depth);
		let title = HtmlTag {
			name: Ranged {
				value: hstr,
				range: (TokenPos::new(), TokenPos::new()),
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
						range: (TokenPos::new(), TokenPos::new()),
					},
					attributes: vec![],
					body: vec![HtmlNodes::String(subtitle)],
					subtags: vec![],
				};
				HtmlTag {
					name: Ranged {
						value: String::from("hgroup"),
						range: (TokenPos::new(), TokenPos::new()),
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
							range: (TokenPos::new(), TokenPos::new()),
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
				range: (TokenPos::new(), TokenPos::new()),
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
			range: (TokenPos::new(), TokenPos::new()),
		},
		attributes: vec![],
		body: vec,
		subtags: vec![],
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct PlugCall {
	pub(crate) name: Ranged<String>,
	pub(crate) arguments: Ranged<Vec<Token>>,
	pub(crate) body: Option<Ranged<Vec<Token>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HtmlNodes {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	String(Vec<StringParts>),
	PlugCall(Box<PlugCall>),
	Section(Section),
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyTags {
	HtmlTag(HtmlTag),
	MacroCall(Macro),
	PlugCall(Box<PlugCall>),
	Section(Section),
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFile<'a> {
	pub local_tokens: Vec<Token>,
	pub body: Vec<TopNodes>,
	pub defined_macros: Vec<Macro>,
	pub defined_variables: Vec<Variable>,
	pub defined_lambdas: Vec<Lambda>,
	pub template: Option<&'a ParsedFile<'a>>,
	pub path: Option<PathBuf>,
}

pub enum VariableOption<T> {
	Some(T),
	None,  // The variable was not found
	Unset, // The variable was found, but it's not set
}

impl<'a> ParsedFile<'a> {
	pub fn new(local_tokens: Vec<Token>, path: Option<PathBuf>) -> Self {
		Self {
			local_tokens,
			body: vec![],
			defined_macros: vec![],
			defined_variables: vec![],
			defined_lambdas: vec![],
			template: None,
			path,
		}
	}

	pub fn get_macro_template(&self, predicate: impl Fn(&&Macro) -> bool) -> Option<&Macro> {
		self.defined_macros.iter().rfind(&predicate).or(self
			.template
			.map(|x| x.get_macro_template(&predicate))
			.unwrap_or(None))
	}

	pub fn get_path_slice(&self) -> Option<&Path> {
		self.path.as_ref().map(|x| x.as_path())
	}

	pub fn get_variable_value(&self, predicate: &str) -> VariableOption<&Vec<StringParts>> {
		for var in self.defined_variables.iter() {
			if var.name == predicate {
				return VariableOption::Some(&var.value);
			}
		}

		for var in self.defined_lambdas.iter() {
			if var.name == predicate {
				match var.value {
					Some(ref value) => return VariableOption::Some(value),
					None => return VariableOption::Unset,
				}
			}
		}

		match self.template {
			Some(template) => template.get_variable_value(predicate),
			None => VariableOption::None,
		}
	}

	pub fn get_macro_scope(&self) -> HashMap<String, Scoped<&Macro>> {
		let mut output = HashMap::new();
		if let Some(template) = self.template {
			output.extend(template.get_macro_scope())
		}

		output.extend(self.defined_macros.iter().map(|x| {
			(
				x.name.value.clone(),
				(x, (self.local_tokens.as_slice(), self.get_path_slice())),
			)
		}));

		output
	}

	pub fn get_variable_scope(
		&'a self,
		sub_scope: &[&'a ParsedFile],
	) -> HashMap<String, Scoped<Option<&'a Vec<StringParts>>>> {
		let mut out = HashMap::new();

		if let Some(template) = self.template {
			out.extend(template.get_variable_scope(&[]))
		}

		out.extend(self.defined_lambdas.iter().map(|x| {
			if !sub_scope.is_empty() {
				for scope in sub_scope.iter() {
					let find = scope.defined_variables.iter().rfind(|y| y.name == x.name);
					if let Some(find) = find {
						return (
							find.name.clone(),
							(
								Some(&find.value),
								(scope.local_tokens.as_slice(), scope.get_path_slice()),
							),
						);
					}
				}
				(
					x.name.clone(),
					(
						x.value.as_ref(),
						(self.local_tokens.as_slice(), self.get_path_slice()),
					),
				)
			} else {
				(
					x.name.clone(),
					(
						x.value.as_ref(),
						(self.local_tokens.as_slice(), self.get_path_slice()),
					),
				)
			}
		}));

		out.extend(self.defined_variables.iter().map(|x| {
			(
				x.name.clone(),
				(
					Some(&x.value),
					(self.local_tokens.as_slice(), self.get_path_slice()),
				),
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
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinFunc {
	And,
	Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UniFunc {
	Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
	None,
	Variable(String),
	Literal(Vec<StringParts>),
	BinFunc(BinFunc, Box<Ranged<Expression>>, Box<Ranged<Expression>>),
	UniFunc(UniFunc, Box<Ranged<Expression>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
	pub name: String,
	pub value: Vec<StringParts>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
	pub name: String,
	pub value: Option<Vec<StringParts>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ranged<T> {
	pub(crate) value: T,
	pub(crate) range: (TokenPos, TokenPos),
}

impl Ranged<&str> {
	pub fn to_own(&self) -> Ranged<String> {
		Ranged {
			value: self.value.to_owned(),
			range: self.range,
		}
	}
}

pub trait AstNode {
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

impl AstNode for HtmlTag {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		self.body
			.iter()
			.flat_map(|x| x.find_undefined_vars(defined))
			.collect()
	}
}

impl AstNode for HtmlNodes {
	fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
		match self {
			HtmlNodes::HtmlTag(x) => x.find_undefined_vars(defined),
			HtmlNodes::MacroCall(x) => x.find_undefined_vars(defined),
			HtmlNodes::String(x) => x.find_undefined_vars(defined),
			HtmlNodes::Section(_) => todo!("undefined vars in body"),
			HtmlNodes::PlugCall(_) => Vec::new(),
			Self::Content => Vec::new(),
		}
	}
}

#[derive(Clone, Debug)]
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
	pub fn get_argument_scope<'a>(
		&'a self,
		scope: Scope<'a>,
	) -> HashMap<String, Scoped<Option<&Vec<StringParts>>>> {
		let mut output = HashMap::new();

		output.extend(self.arguments.iter().map(|x| match x.value {
			Some(ref value) => (x.name.value.clone(), (Some(value), scope)),
			None => (x.name.value.clone(), (None, scope)),
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

		self.body = subtag_stack
			.into_iter()
			.map(|x| HtmlNodes::HtmlTag(x))
			.collect();

		self.subtags = Vec::new();

		self
	}
}

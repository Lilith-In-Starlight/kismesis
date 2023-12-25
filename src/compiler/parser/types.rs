use std::collections::HashMap;

use crate::compiler::lexer::Token;

use super::state::TokenPos;

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
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopNodes {
    HtmlTag(HtmlTag),
    MacroCall(Macro),
    PlugCall(Box<PlugCall>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyTags {
    HtmlTag(HtmlTag),
    MacroCall(Macro),
    PlugCall(Box<PlugCall>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tag {
    HtmlTag(HtmlTag),
    MacroDef(Macro),
    MacroCall(Macro),
    PlugCall(Box<PlugCall>),
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFile {
    pub local_tokens: Vec<Token>,
    pub body: Vec<TopNodes>,
    pub defined_macros: Vec<Macro>,
    pub defined_variables: Vec<Variable>,
    pub defined_lambdas: Vec<Lambda>,
}

impl ParsedFile {
    pub fn new(local_tokens: Vec<Token>) -> Self {
        Self {
            local_tokens,
            body: vec![],
            defined_macros: vec![],
            defined_variables: vec![],
            defined_lambdas: vec![],
        }
    }

    pub fn get_macro_scope(&self) -> HashMap<String, (&Macro, &[Token])> {
        self.defined_macros
            .iter()
            .map(|x| (x.name.value.clone(), (x, self.local_tokens.as_slice())))
            .collect()
    }
    pub fn get_variable_scope(&self) -> HashMap<String, (Variable, &[Token])> {
        self.defined_variables
            .clone()
            .into_iter()
            .map(|x| (x.name.clone(), (x, self.local_tokens.as_slice())))
            .chain(
                self.defined_lambdas
                    .clone()
                    .into_iter()
                    .filter_map(|x| match x.value {
                        Some(val) => Some((
                            x.name.clone(),
                            (
                                Variable {
                                    name: x.name.clone(),
                                    value: val.clone(),
                                },
                                self.local_tokens.as_slice(),
                            ),
                        )),
                        None => None,
                    }),
            )
            .collect()
    }

    pub fn get_undefined_lambdas(&self) -> Vec<(String, &[Token])> {
        self.defined_lambdas
            .iter()
            .filter_map(|x| {
                x.value
                    .as_ref()
                    .map(|_| (x.name.clone(), self.local_tokens.as_slice()))
            })
            .collect()
    }
}

impl From<Tag> for BodyNodes {
    fn from(value: Tag) -> Self {
        match value {
            Tag::HtmlTag(x) => Self::HtmlTag(x),
            Tag::MacroCall(x) => Self::MacroCall(x),
            Tag::MacroDef(x) => Self::MacroDef(x),
            Tag::PlugCall(x) => Self::PlugCall(x),
        }
    }
}

impl From<BodyTags> for BodyNodes {
    fn from(value: BodyTags) -> Self {
        match value {
            BodyTags::HtmlTag(x) => Self::HtmlTag(x),
            BodyTags::MacroCall(x) => Self::MacroCall(x),
            BodyTags::PlugCall(x) => Self::PlugCall(x),
        }
    }
}

impl From<BodyTags> for HtmlNodes {
    fn from(value: BodyTags) -> Self {
        match value {
            BodyTags::HtmlTag(x) => Self::HtmlTag(x),
            BodyTags::MacroCall(x) => Self::MacroCall(x),
            BodyTags::PlugCall(x) => Self::PlugCall(x),
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
            HtmlNodes::PlugCall(_) => Vec::new(),
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

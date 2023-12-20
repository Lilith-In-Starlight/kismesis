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
pub struct HtmlTag<'a> {
    pub(crate) name: Ranged<String>,
    pub(crate) attributes: Vec<Attribute>,
    pub(crate) body: Vec<HtmlNodes<'a>>,
    pub(crate) subtags: Vec<HtmlTag<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Macro<'a> {
    pub(crate) name: Ranged<String>,
    pub(crate) arguments: Vec<Argument>,
    pub(crate) body: Vec<HtmlNodes<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PlugCall {
    pub(crate) name: Ranged<String>,
    pub(crate) arguments: Ranged<Vec<Token>>,
    pub(crate) body: Option<Ranged<Vec<Token>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HtmlNodes<'a> {
    HtmlTag(HtmlTag<'a>),
    MacroCall(Macro<'a>),
    String(Vec<StringParts>),
    PlugCall(Box<PlugCall>),
    Subtree(ParsedFile<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopNodes<'a> {
    HtmlTag(HtmlTag<'a>),
    MacroCall(Macro<'a>),
    PlugCall(Box<PlugCall>),
    Subtree(ParsedFile<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyTags<'a> {
    HtmlTag(HtmlTag<'a>),
    MacroCall(Macro<'a>),
    PlugCall(Box<PlugCall>),
    Subtree(ParsedFile<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tag<'a> {
    HtmlTag(HtmlTag<'a>),
    MacroDef(Macro<'a>),
    MacroCall(Macro<'a>),
    PlugCall(Box<PlugCall>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyNodes<'a> {
    HtmlTag(HtmlTag<'a>),
    MacroDef(Macro<'a>),
    MacroCall(Macro<'a>),
    PlugCall(Box<PlugCall>),
    String(Vec<StringParts>),
    LambdaDef(Lambda),
    VarDef(Variable),
    Subtree(ParsedFile<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFile<'a> {
    pub local_tokens: &'a [Token],
    pub body: Vec<TopNodes<'a>>,
    pub defined_macros: Vec<Macro<'a>>,
    pub defined_variables: Vec<Variable>,
    pub defined_lambdas: Vec<Lambda>,
}

impl<'a> ParsedFile<'a> {
    pub fn new(local_tokens: &'a [Token]) -> Self {
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
            .map(|x| (x.name.value.clone(), (x, self.local_tokens)))
            .collect()
    }
    pub fn get_variable_scope(&self) -> HashMap<String, (Variable, &[Token])> {
        self.defined_variables
            .clone()
            .into_iter()
            .map(|x| (x.name.clone(), (x, self.local_tokens)))
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
                                self.local_tokens,
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
            .filter_map(|x| x.value.as_ref().map(|_| (x.name.clone(), self.local_tokens)))
            .collect()
    }
}

impl<'a> From<Tag<'a>> for BodyNodes<'a> {
    fn from(value: Tag<'a>) -> Self {
        match value {
            Tag::HtmlTag(x) => Self::HtmlTag(x),
            Tag::MacroCall(x) => Self::MacroCall(x),
            Tag::MacroDef(x) => Self::MacroDef(x),
            Tag::PlugCall(x) => Self::PlugCall(x),
        }
    }
}

impl<'a> From<BodyTags<'a>> for BodyNodes<'a> {
    fn from(value: BodyTags<'a>) -> Self {
        match value {
            BodyTags::HtmlTag(x) => Self::HtmlTag(x),
            BodyTags::MacroCall(x) => Self::MacroCall(x),
            BodyTags::PlugCall(x) => Self::PlugCall(x),
            BodyTags::Subtree(x) => Self::Subtree(x),
        }
    }
}

impl<'a> From<BodyTags<'a>> for HtmlNodes<'a> {
    fn from(value: BodyTags<'a>) -> Self {
        match value {
            BodyTags::HtmlTag(x) => Self::HtmlTag(x),
            BodyTags::MacroCall(x) => Self::MacroCall(x),
            BodyTags::PlugCall(x) => Self::PlugCall(x),
            BodyTags::Subtree(x) => Self::Subtree(x),
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

impl<'a> AstNode for Macro<'a> {
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

impl<'a> AstNode for HtmlTag<'a> {
    fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
        self.body
            .iter()
            .flat_map(|x| x.find_undefined_vars(defined))
            .collect()
    }
}

impl<'a> AstNode for HtmlNodes<'a> {
    fn find_undefined_vars(&self, defined: &[String]) -> Vec<Ranged<String>> {
        match self {
            HtmlNodes::HtmlTag(x) => x.find_undefined_vars(defined),
            HtmlNodes::MacroCall(x) => x.find_undefined_vars(defined),
            HtmlNodes::String(x) => x.find_undefined_vars(defined),
            HtmlNodes::PlugCall(_) => Vec::new(),
            HtmlNodes::Subtree(_) => todo!("Subtree calls"),
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

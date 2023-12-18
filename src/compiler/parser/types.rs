use crate::compiler::lexer::Token;

use super::state::TokenPos;

#[derive(Debug, Clone, PartialEq)]
pub enum StringParts {
    String(String),
    Expression(Expression),
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
    pub(crate) subtags: Vec<HtmlTag>,
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
    PlugCall(PlugCall),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyTags {
    HtmlTag(HtmlTag),
    MacroCall(Macro),
    PlugCall(PlugCall),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tag {
    HtmlTag(HtmlTag),
    MacroDef(Macro),
    MacroCall(Macro),
    PlugCall(PlugCall),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyNodes {
    HtmlTag(HtmlTag),
    MacroDef(Macro),
    MacroCall(Macro),
    PlugCall(PlugCall),
    String(Vec<StringParts>),
    LambdaDef(Lambda),
    VarDef(Variable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParsedFile {
    pub body: Vec<HtmlNodes>,
    pub defined_macros: Vec<Macro>,
    pub defined_variables: Vec<Variable>,
    pub defined_lambdas: Vec<Lambda>,
}

impl ParsedFile {
    pub fn new() -> Self {
        Self {
            body: vec![],
            defined_macros: vec![],
            defined_variables: vec![],
            defined_lambdas: vec![],
        }
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
    BinFunc(BinFunc, Box<Expression>, Box<Expression>),
    UniFunc(UniFunc, Box<Expression>),
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

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
pub struct ParsedFile<'a> {
    pub local_tokens: Vec<Token>,
    pub body: Vec<TopNodes>,
    pub defined_macros: Vec<Macro>,
    pub defined_variables: Vec<Variable>,
    pub defined_lambdas: Vec<Lambda>,
    pub template: Option<&'a ParsedFile<'a>>,
}

pub enum VariableOption<T> {
    Some(T),
    None, // The variable was not found
    Unset, // The variable was found, but it's not set
}

impl<'a> ParsedFile<'a> {
    pub fn new(local_tokens: Vec<Token>) -> Self {
        Self {
            local_tokens,
            body: vec![],
            defined_macros: vec![],
            defined_variables: vec![],
            defined_lambdas: vec![],
            template: None,
        }
    }

    pub fn get_macro_template(&self, predicate: impl Fn(&&Macro) -> bool) -> Option<&Macro> {
        self.defined_macros.iter().rfind(&predicate)
            .or(self.template.map(|x| x.get_macro_template(&predicate)).unwrap_or(None))
    }

    pub fn get_variable_value(&self, predicate: &str) -> VariableOption<&Vec<StringParts>> {
        for var in self.defined_variables.iter() {
            if var.name == predicate {
                return VariableOption::Some(&var.value)
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

    pub fn get_macro_scope(&self) -> HashMap<String, (&Macro, &[Token])> {
        let mut output = HashMap::new();
        if let Some(template) = self.template {
            output.extend(template.get_macro_scope())
        }

        output.extend(self.defined_macros.iter().map(|x| (x.name.value.clone(), (x, self.local_tokens.as_slice()))));

        output
    }

    pub fn get_variable_scope(&self) -> HashMap<String, (Option<&Vec<StringParts>>, &[Token])> {
        let mut out = HashMap::new();

        if let Some(template) = self.template {
            out.extend(template.get_variable_scope())
        }

        out.extend(self.defined_lambdas.iter().map(|x| match x.value {
            Some(ref value) => (x.name.clone(), (Some(value), self.local_tokens.as_slice())),
            None => (x.name.clone(), (None, self.local_tokens.as_slice()))
        }));

        out.extend(self.defined_variables.iter().map(|x| (x.name.clone(), (Some(&x.value), self.local_tokens.as_slice()))));

        out.into_iter().collect()
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

impl Macro {
    pub fn get_argument_scope<'a>(&'a self, tokens: &'a [Token]) -> HashMap<String, (Option<&Vec<StringParts>>, &[Token])> {
        let mut output = HashMap::new();

        output.extend(self.arguments.iter().map(|x| match x.value {
            Some(ref value) => (x.name.value.clone(), (Some(value), tokens)),
            None => (x.name.value.clone(), (None, tokens)),
        }));

        output
    }
}

impl HtmlTag {
    pub fn merge_subtags(mut self) -> Self {
        let mut subtag_stack = self.subtags;
        let Some(top) = subtag_stack.last_mut() else { self.subtags = subtag_stack; return self };
        top.body = self.body;
        loop {
            let Some(top) = subtag_stack.pop() else { break };
            let Some(top_2) = subtag_stack.last_mut() else { subtag_stack.push(top); break };
            top_2.body.push(HtmlNodes::HtmlTag(top));
        }

        self.body = subtag_stack.into_iter().map(|x| HtmlNodes::HtmlTag(x)).collect();

        self.subtags = Vec::new();

        self
    }
}

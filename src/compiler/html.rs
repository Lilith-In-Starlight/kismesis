use std::collections::HashMap;

use crate::compiler::parser::types::ParsedFile;

use super::{parser::{types::{TopNodes, HtmlTag, Macro, Attribute, StringParts, HtmlNodes, PlugCall, Expression, BinFunc, UniFunc}, errors::ErrorState, state::TokenPos}, options::Settings};

type CompileResult<T> = Result<T, Inside>;

#[derive(Clone)]
struct GenerationState<'a> {
    options: &'a Settings,
    variable_scopes: VariableScope,
    undefined_lambdas: Vec<String>,
    macro_templates: Vec<Macro>,
    indent: usize,
    inline: bool,
}

type VariableScope = HashMap<String, Vec<StringParts>>;

impl<'a> GenerationState<'a> {
    pub(crate) fn from(file: &ParsedFile, options: &'a Settings) -> Self {
        Self {
            options,
            variable_scopes: VariableScope::new(),
            macro_templates: file.defined_macros.clone(),
            undefined_lambdas: file.defined_lambdas.iter().filter(|x| x.value.is_none()).map(|x| x.name.clone()).collect(),
            indent: 0,
            inline: false,
        }
    }
}

fn get_var_scope(file: &ParsedFile) -> VariableScope {
    let mut scope = VariableScope::new();
    for var in file.defined_lambdas.iter() {
        if let Some(value) = var.value.clone() {
            scope.insert(var.name.clone(), value);
        }
    }
    for var in file.defined_variables.iter() {
        scope.insert(var.name.clone(), var.value.clone());
    }
    scope
}

pub fn generate_html(file: &ParsedFile, options: Settings) -> Result<String, Vec<ErrorState<CompilerError>>> {
    let state = GenerationState::from(file, &options);
    let mut errors = Vec::new();
    let mut output = String::new();
    for node in file.body.iter() {
        if !output.is_empty() {
            output.push('\n');
        }
        match parse_node(node, &state) {
            Ok(string) => output.push_str(&string),
            Err(error) => error.push_into(&mut errors),
        }
    }
    if !errors.is_empty() {
        return Err(errors)
    }
    Ok(output)
}

fn parse_node(node: &TopNodes, state: &GenerationState) -> CompileResult<String> {
    match node {
        TopNodes::HtmlTag(t) => tag(t, state),
        TopNodes::MacroCall(t) => mac_call(t, state),
        TopNodes::PlugCall(t) => plug_call(t, state),
        TopNodes::Subtree(t) => subtree(t, state),
    }
}

fn parse_html_child(node: &HtmlNodes, state: &GenerationState) -> CompileResult<String> {
    match node {
        HtmlNodes::HtmlTag(t) => tag(t, state),
        HtmlNodes::MacroCall(t) => mac_call(t, state),
        HtmlNodes::PlugCall(t) => plug_call(t, state),
        HtmlNodes::Subtree(t) => subtree(t, state),
        HtmlNodes::String(t) => parse_kis_string(&t, state),
    }
}

fn mac_call(mac: &Macro, state: &GenerationState) -> CompileResult<String> {
    let mut errors = Vec::new();
    let template = state.macro_templates.iter().rfind(|x| x.name.value == mac.name.value).ok_or(Inside::Err(CompilerError::UndefinedMacroCall.state_at(mac.name.range)))?;
    let mut new_state = state.clone();
    new_state.variable_scopes = {
        let mut base = template.arguments.clone();
        for arg in mac.arguments.iter() {
            if let Some(base_arg) = base.iter_mut().find(|x| x.name.value == arg.name.value) {
                base_arg.value = arg.value.clone();
            }
        }
        let mut output = VariableScope::new();
        for arg in base {
            match arg.value {
                None => errors.push(CompilerError::UnsetArgNoDefault(arg.name.value).state_at(template.name.range)),
                Some(value) => {output.insert(arg.name.value, value);},
            }
        }
        output
    };

    if !errors.is_empty() {
        return Err(Inside::In(errors))
    }
    
    let mut output = String::new();
    for child in template.body.iter() {
        if !output.is_empty() {
            output.push('\n');
            for _ in 0..state.indent {
                output.push('\t');
            }
        }
        match parse_html_child(child, &new_state) {
            Ok(string) => output.push_str(&string),
            Err(error) => error.push_into(&mut errors),
        }
    }

    if errors.is_empty() {
        Ok(output)
    } else {
        Err(Inside::In(errors))
    }

}
fn plug_call(_plugin: &PlugCall, _state: &GenerationState) -> ! {
    todo!("Plugin calls to html")
}

fn subtree(_tree: &ParsedFile, _state: &GenerationState) -> ! {
    todo!("Subtrees")
}

fn tag(tag: &HtmlTag, state: &GenerationState) -> CompileResult<String> {
    let mut errors = Vec::new();
    let tag = {
        let clone = tag.clone();
        let mut new_body = Vec::<HtmlTag>::new();
        for subtag in clone.subtags {
            match new_body.last_mut() {
                Some(tag) => tag.body.push(HtmlNodes::HtmlTag(subtag)),
                None => new_body.push(subtag),
            }
        }
        let mut clone_two = tag.clone();
        match new_body.last_mut() {
            Some(last) => {
                last.body = clone.body;
                clone_two.body = new_body.into_iter().map(HtmlNodes::HtmlTag).collect();
            },
            None => {
                clone_two.body = clone.body;
            }
        }
        clone_two
    };
    let mut output = String::new();
    for _ in 0..state.indent {
        output.push('\t');
    }
    output.push('<');
    output.push_str(&tag.name.value);
    match attribute_string(&tag.attributes, state) {
        Ok(string) => output.push_str(&string),
        Err(error) => error.push_into(&mut errors),
    }
    if state.options.is_only_closer(&tag.name.value) {
        output.push_str(" />");
    }
    else {
        output.push_str(">");
    }

    if state.options.has_body(&tag.name.value) {
        let mut inline = state.options.is_inline(&tag.name.value) || tag.body.is_empty();
        for child in tag.body.iter() {
            match child {
                HtmlNodes::HtmlTag(x) => {
                    if !state.options.is_inline(&x.name.value) {
                        inline = false;
                        break
                    }
                },
                HtmlNodes::MacroCall(_) | HtmlNodes::PlugCall(_) => {
                    inline = false;
                    break
                },
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
                output.push('\n');
            }
            match parse_html_child(child, &new_state) {
                Ok(string) => output.push_str(&string),
                Err(error) => error.push_into(&mut errors),
            }
        }

        if !inline {
            output.push('\n');
            for _ in 0..state.indent {
                output.push('\t');
            }
        }
        output.push_str(&format!("</{}>", tag.name.value))
    }

    if errors.is_empty() {
        Ok(output)
    } else {
        Err(Inside::In(errors))
    }
}

fn attribute_string(attrs: &Vec<Attribute>, state: &GenerationState) -> CompileResult<String> {
    let mut output = String::new();
    let mut errors = Vec::new();
    for attr in attrs {
        output.push(' ');
        match parse_kis_string(&attr.value, state) {
            Ok(value_string) =>  output.push_str(&format!("{}='{}'", attr.name.value, value_string)),
            Err(error) => error.push_into(&mut errors),
            
        }
    }

    if errors.is_empty() {
        Ok(output)
    } else {
        Err(Inside::In(errors))
    }
}

fn parse_kis_string(string: &[StringParts], state: &GenerationState) -> CompileResult<String> {
    let mut output = String::new();
    for parse in string {
        match parse {
            StringParts::String(x) => output.push_str(&x),
            StringParts::Expression(x) => match calculate_expression(x, state)? {
                ExpressionValues::String(x) => output.push_str(&parse_kis_string(&x, state)?),
                ExpressionValues::None => panic!("Can't write a None value"),
                ExpressionValues::Generic => panic!("Generic string can't be written"),
            },
        }
    }

    output = output.trim().to_string();
    Ok(output)
}

enum ExpressionValues {
    String(Vec<StringParts>),
    None,
    Generic
}

impl ExpressionValues {
    fn is_truthy(&self) -> bool {
        match self {
            Self::String(_) | Self::Generic => true,
            _ => false,
        }
    }
}

fn calculate_expression(expr: &Expression, state: &GenerationState) -> CompileResult<ExpressionValues> {
    match expr {
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
                    if exp1.is_truthy() { Ok(exp1) }
                    else if exp2.is_truthy() { Ok(exp1) }
                    else { Ok(ExpressionValues::None) }
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
            if let Some(var) = state.variable_scopes.get(x) {
                Ok(ExpressionValues::String(var.clone()))
            } else {
                panic!("Undefined variable {}", x)
            }
        },
    }
}


#[derive(Clone, Debug)]
pub enum CompilerError {
    UnsetArgNoDefault(String),
    UndefinedMacroCall,
}

#[derive(Clone, Debug)]
pub enum Inside {
    In(Vec<ErrorState<CompilerError>>),
    Err(ErrorState<CompilerError>),
}

impl Inside {
    fn push_into(self, vec: &mut Vec<ErrorState<CompilerError>>) {
        match self {
            Self::In(mut content) => vec.append(&mut content),
            Self::Err(error) => vec.push(error),
        }
    }
}

impl CompilerError {
    fn state_at(self, pos: (TokenPos,TokenPos)) -> ErrorState<Self> {
        ErrorState { error: self, previous_errors: vec![], start_position: pos.0, end_position: pos.1 }
    }
}

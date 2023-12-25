use std::collections::HashMap;

use crate::compiler::parser::types::ParsedFile;

use super::{
    errors::{ErrorKind, ErrorState},
    lexer::Token,
    options::Settings,
    parser::{
        state::TokenPos,
        types::{
            Attribute, BinFunc, Expression, HtmlNodes, HtmlTag, Macro, PlugCall, Ranged,
            StringParts, TextPos, TopNodes, UniFunc,
        },
    },
};

type CompileResult<'a, T> = Result<T, Inside<'a>>;

#[derive(Clone)]
struct GenerationState<'a> {
    options: &'a Settings,
    variable_scopes: VariableScope<'a>,
    undefined_lambdas: Vec<(String, &'a [Token])>,
    macro_templates: HashMap<String, (&'a Macro, &'a [Token])>,
    indent: usize,
    inline: bool,
    tokens: &'a [Token],
}

type VariableScope<'a> = HashMap<String, String>;

impl<'a> GenerationState<'a> {
    pub(crate) fn from(file: &'a ParsedFile, options: &'a Settings) -> Self {
        let undefined_lambdas = file.get_undefined_lambdas();
        let mut variable_scopes = VariableScope::new();
        let mut defined_variables = file.defined_variables.clone();
        for var in file.defined_lambdas.iter() {
            if let Some(value) = var.value.clone() {
                defined_variables.push(super::parser::types::Variable {
                    name: var.name.clone(),
                    value,
                });
            }
        }

        let mut uncalculated = defined_variables;

        while !uncalculated.is_empty() {
            let mut remv = 0;
            let mut calculated_any = false;
            for var in uncalculated.clone().iter() {
                if let Ok(x) = parse_kis_string(
                    &var.value,
                    (&variable_scopes, &undefined_lambdas, &file.local_tokens),
                ) {
                    calculated_any = true;
                    variable_scopes.insert(var.name.clone(), x);
                    uncalculated.remove(remv);
                    remv = 0;
                }
                remv += 1;

                if !calculated_any {
                    panic!("Cant calculate variable values");
                }
            }
        }
        Self {
            options,
            variable_scopes,
            macro_templates: file.get_macro_scope(),
            undefined_lambdas: file.get_undefined_lambdas(),
            indent: 0,
            inline: false,
            tokens: &file.local_tokens,
        }
    }
}

pub fn generate_html<'a>(file: &'a ParsedFile, options: &'a Settings) -> CompileResult<'a, String> {
    let state = GenerationState::from(file, options);
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
        return Err(Inside::In(errors));
    }
    Ok(output)
}

fn parse_node<'a>(node: &TopNodes, state: &GenerationState<'a>) -> CompileResult<'a, String> {
    match node {
        TopNodes::HtmlTag(t) => tag(t, state),
        TopNodes::MacroCall(t) => mac_call(t, state),
        TopNodes::PlugCall(t) => plug_call(t, state),
    }
}

fn parse_html_child<'a>(
    node: &HtmlNodes,
    state: &GenerationState<'a>,
) -> CompileResult<'a, String> {
    match node {
        HtmlNodes::HtmlTag(t) => tag(t, state),
        HtmlNodes::MacroCall(t) => mac_call(t, state),
        HtmlNodes::PlugCall(t) => plug_call(t, state),
        HtmlNodes::String(t) => parse_kis_string(
            t,
            (
                &state.variable_scopes,
                &state.undefined_lambdas,
                state.tokens,
            ),
        ),
    }
}

fn mac_call<'a>(mac: &Macro, state: &GenerationState<'a>) -> CompileResult<'a, String> {
    let mut errors = Vec::new();
    let template = state
        .macro_templates
        .get(&mac.name.value)
        .ok_or(Inside::Err(
            CompilerError::UndefinedMacroCall.state_at(mac.name.range, state.tokens),
        ))?;
    let mut new_state = state.clone();
    new_state.variable_scopes = {
        let mut base = template.0.arguments.clone();
        for arg in mac.arguments.iter() {
            if let Some(base_arg) = base.iter_mut().find(|x| x.name.value == arg.name.value) {
                base_arg.value = arg.value.clone();
            }
        }
        let mut output = VariableScope::new();
        for arg in base {
            match arg.value {
                None => errors.push(
                    CompilerError::UnsetArgNoDefault(arg.name.value)
                        .state_at(template.0.name.range, template.1),
                ),
                Some(value) => match parse_kis_string(
                    &value,
                    (
                        &state.variable_scopes,
                        &state.undefined_lambdas,
                        state.tokens,
                    ),
                ) {
                    Ok(string) => {
                        output.insert(arg.name.value, string);
                    }
                    Err(error) => error.push_into(&mut errors),
                },
            }
        }
        output
    };

    if !errors.is_empty() {
        return Err(Inside::In(errors));
    }

    let mut output = String::new();
    for child in template.0.body.iter() {
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


fn tag<'a>(tag: &HtmlTag, state: &GenerationState<'a>) -> CompileResult<'a, String> {
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
            }
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
    } else {
        output.push('>');
    }

    if state.options.has_body(&tag.name.value) {
        let mut inline = state.options.is_inline(&tag.name.value) || tag.body.is_empty();
        for child in tag.body.iter() {
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

fn attribute_string<'a>(
    attrs: &Vec<Attribute>,
    state: &GenerationState<'a>,
) -> CompileResult<'a, String> {
    let mut output = String::new();
    let mut errors = Vec::new();
    for attr in attrs {
        output.push(' ');
        match parse_kis_string(
            &attr.value,
            (
                &state.variable_scopes,
                &state.undefined_lambdas,
                state.tokens,
            ),
        ) {
            Ok(value_string) => output.push_str(&format!("{}='{}'", attr.name.value, value_string)),
            Err(error) => error.push_into(&mut errors),
        }
    }

    if errors.is_empty() {
        Ok(output)
    } else {
        Err(Inside::In(errors))
    }
}

fn parse_kis_string<'a>(
    string: &[StringParts],
    state: (&VariableScope<'a>, &Vec<(String, &'a [Token])>, &'a [Token]),
) -> CompileResult<'a, String> {
    let mut output = String::new();
    let mut errors = Vec::new();
    for parse in string {
        match parse {
            StringParts::String(x) => output.push_str(x),
            StringParts::Expression(expr) => match calculate_expression(expr, state)? {
                ExpressionValues::String(x) => output.push_str(&x),
                ExpressionValues::None => {
                    errors.push(CompilerError::CantWriteNoneValue.state_at(expr.range, state.2))
                }
                ExpressionValues::Generic => {
                    errors.push(CompilerError::CantWriteGenericValue.state_at(expr.range, state.2))
                }
            },
        }
    }

    output = output.trim().to_string();

    if errors.is_empty() {
        Ok(output)
    } else {
        Err(Inside::In(errors))
    }
}

#[derive(Clone)]
enum ExpressionValues {
    String(String),
    None,
    Generic,
}

#[derive(Clone, Debug)]
pub struct ScopedError<'a, T> {
    pub error: ErrorState<T>,
    pub scope: &'a [Token],
}

impl ExpressionValues {
    fn is_truthy(&self) -> bool {
        matches!(self, Self::Generic | Self::String(_))
    }
}

fn calculate_expression<'a>(
    expr: &Ranged<Expression>,
    state: (&VariableScope<'a>, &Vec<(String, &'a [Token])>, &'a [Token]),
) -> CompileResult<'a, ExpressionValues> {
    match &expr.value {
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
                    if exp1.is_truthy() {
                        Ok(exp1)
                    } else if exp2.is_truthy() {
                        Ok(exp2)
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
                    if exp.is_truthy() {
                        Ok(ExpressionValues::None)
                    } else {
                        Ok(ExpressionValues::Generic)
                    }
                }
            }
        }
        Expression::Variable(x) => {
            if let Some(var) = state.0.get(x) {
                Ok(ExpressionValues::String(var.clone()))
            } else {
                Err(Inside::Err(
                    CompilerError::UndefinedVariable.state_at(expr.range, state.2),
                ))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompilerError {
    UndefinedVariable,
    CantWriteNoneValue,
    CantWriteGenericValue,
    UnsetArgNoDefault(String),
    UndefinedMacroCall,
}

#[derive(Clone, Debug)]
pub enum Inside<'a> {
    In(Vec<ScopedError<'a, CompilerError>>),
    Err(ScopedError<'a, CompilerError>),
}

impl<'a> Inside<'a> {
    fn push_into(self, vec: &mut Vec<ScopedError<'a, CompilerError>>) {
        match self {
            Self::In(mut content) => vec.append(&mut content),
            Self::Err(error) => vec.push(error),
        }
    }
}

impl CompilerError {
    fn state_at(self, pos: (TokenPos, TokenPos), scope: &[Token]) -> ScopedError<Self> {
        ScopedError {
            error: ErrorState {
                error: self,
                previous_errors: vec![],
                text_position: TextPos::Range(pos),
            },
            scope,
        }
    }
}

impl ErrorKind for CompilerError {
    fn get_text(&self) -> String {
        match self {
            Self::UndefinedVariable => "This variable isn't defined".into(),
            Self::CantWriteNoneValue => {
                "This computes to a Nothing value, which cannot be written into content".into()
            }
            Self::CantWriteGenericValue => {
                "This computes to a Anything value, which cannot be written into content".into()
            }
            Self::UnsetArgNoDefault(arg) => format!(
                "The `{}` argument is unset but the macro definition has no default for it",
                arg
            ),
            Self::UndefinedMacroCall => "This macro isn't defined".to_string(),
        }
    }
}

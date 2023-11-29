use std::cmp::Ordering;

use crate::kiss::{parser::{States, TokenScanner}, lexer};
use colored::*;

#[derive(Debug)]
pub enum KismesisError {
	ClosedTooManyTags,
	ExpectedTagName,
	ExpectedCharacter { character: char },
	ExpectedParameterValue,
	ExpectedAnyOpener,
	UnrecoverableError(UnrecoverableError),
	ParametersNotAllowed,
	NewlineInParameter,
	ExpectedTagBodyOrParam,
	TriedMacroDefMerge,
	ExpectedMacroName,
	ExpectedMacroBodyOrArg,
	ExpectedArgValue,
	TriedMacroDefInTag,
	ExpectedVariableName,
	UseOfUndefinedVariable,
	UndefinedMacroVariables(Vec<String>),
	UnsetMacroVariable(String),
	UseOfDeprecatedTag,
	NoContentArg,
	CallBodyNotDeclared,
}

#[derive(Debug)]
pub enum HtmlGenerationError {
	UnrecoverableError(UnrecoverableError),
	KismesisError(KismesisError),
}

impl Error for HtmlGenerationError {
	fn get_error_text(&self) -> String {
        match self {
			Self::UnrecoverableError(x) => x.get_error_text(),
			Self::KismesisError(x) => x.get_error_text(),
		}
    }
}

impl From<KismesisError> for HtmlGenerationError {
	fn from(val: KismesisError) -> Self{
		Self::KismesisError(val)
	}
}

impl From<UnrecoverableError> for HtmlGenerationError {
	fn from(val: UnrecoverableError) -> Self {
		Self::UnrecoverableError(val)
	}
}

#[derive(Debug)]
pub struct ErrorState<T: Error> {
	pub error: T,
	pub line_position: usize,
	pub line: usize,
	pub sub_errors: Option<Vec<ErrorState<T>>>,
}

pub trait Error {
	fn get_error_text(&self) -> String;
}

impl<T: Error> ErrorState<T> {
	fn report(&self, scanner: &TokenScanner) -> String {
		self.report_error_line(scanner, self.error.get_error_text())
	}
	pub fn report_error_line(&self, token_scanner: &TokenScanner, message: String) -> String {
		let lines = token_scanner.get_lines_slices();
		let mut output = String::new();
		let minline = match self.line {
			x if x >= 2 => x - 2,
			_ => 0,
		};
		let maxline = match self.line {
			x if x + 2 <= lines.len() - 1 => x + 2,
			_ => lines.len() - 1
		};
		for n in minline..=maxline {
			let line_slices = token_scanner.get_lines_slices();
			let Some(line) = line_slices.get(n) else { return format!("Failed to report an error that happened at line {} because the error reporter tried to access line {}", self.line, n)};
			if n == self.line {
				output.push_str(&report_line(line, n, maxline, Some((self.line_position, &message))));
			} else {
				output.push_str(&report_line(line, n, maxline, None));
			}
			output.push('\n');
		}
		output
	}
}

impl Error for KismesisError {
	fn get_error_text(&self) -> String {
		match self {
			Self::ClosedTooManyTags => "This > is unnecessary as there are no more tags to close".into(),
			Self::ExpectedTagName => "Expected a tag name here".into(),
			Self::ExpectedCharacter { character } => format!("Expected a {} here", character),
			Self::ExpectedParameterValue => "Expected a parameter value here".into(),
			Self::ExpectedAnyOpener => "Expected the start of a tag, a macro definition, or a variable declaration".into(),
			Self::UnrecoverableError(err) => err.get_error_text(),
			Self::NewlineInParameter => "Newlines in parameters are not currently accepted".into(),
			Self::ExpectedTagBodyOrParam => "Expected a | or the beginning of a new parameter".into(),
			Self::ParametersNotAllowed => "This tag is not allowed to have parameters".into(),
			Self::TriedMacroDefMerge => "Tried to insert a macro definition as a child of another tag".into(),
			Self::ExpectedMacroName => "Expected a macro name here".into(),
			Self::ExpectedMacroBodyOrArg => "Expected a macro body or argument name".into(),
			Self::ExpectedArgValue => "Expected a value for this argument".into(),
			Self::TriedMacroDefInTag => "Body tags cannot have macro definitions".into(),
			Self::ExpectedVariableName => "Expected a variable name here".into(),
			Self::UseOfUndefinedVariable => "Tried to use an undefined variable".into(),
			Self::UndefinedMacroVariables(val) => match val.len() {
				1 => format!("The argument {} has no default value", display_string_list(val)),
				_ => format!("The arguments {} have no default value", display_string_list(val)),
			},
			Self::UnsetMacroVariable(val) => format!("{} has no default value and is unset", val),
			Self::UseOfDeprecatedTag => "This tag is deprecated".into(),
			Self::NoContentArg => "This macro has content, but its definition has no content argument".into(),
			Self::CallBodyNotDeclared => "For a macro call to have a body it must have a kisscontent argument".into()
		}
	}
}

impl KismesisError {
	pub fn state(self, scanner: &TokenScanner) -> ErrorState<KismesisError> {
		ErrorState { error: self, line_position: scanner.get_position_in_line(), line: scanner.get_current_line_number(), sub_errors: None }
	}
}

impl Error for UnrecoverableError {
	fn get_error_text(&self) -> String {
		match self {
			Self::ImpossibleEmpty => "Tag stack was unexpectedly empty".into(),
			Self::UndefinedStateTransition {from, to} => format!("Parser transitioned from state {:?} to {:?}, which has not been manually allowed", from, to),
			Self::TagStackHadNonTag => "Tag stack had a non-tag, which should be impossible".into(),
			Self::ImpossibleNotTag => "Tried to access a non-tag that should have been a tag".into(),
			Self::AddedParamToMacro => "Tried to add a Parameter to a macro".into(),
			Self::AddedArgToTag => "Tried to add a macro argument to a tag".into(),
			Self::UnmetExpectancy => "Tag stack had insufficient tags.".into(),
			Self::NonMacroIntoMacroArray => "Tried to push a non-macro into a macro array".into(),
			Self::ImpossibleNoMacroDefs => "Defining a macro argument, but the parser recognizes no macro being currently defined".into(),
			Self::VariableInWrongPlace => "It should be impossible to have variables here".into(),
			Self::CannotMakeIntoScope => "Tried to create a scope out of something that cannot create one".into(),
		}
	}
}

pub fn display_string_list(l: &Vec<String>) -> String{
	let mut out = String::new();
	for (idx, i) in l.iter().enumerate() {
		if idx == l.len() - 1 && l.len() != 1 {
			out.push_str("and ");
		}
		out.push_str(i);
		if idx < l.len() - 1 {
			out.push_str(", ");
		}
	}
	out
}


#[derive(Debug)]
pub enum UnrecoverableError {
	TagStackHadNonTag,
	UndefinedStateTransition {from: States, to: States},
	ImpossibleEmpty,
	ImpossibleNotTag,
	AddedParamToMacro,
	AddedArgToTag,
	UnmetExpectancy,
	NonMacroIntoMacroArray,
	ImpossibleNoMacroDefs,
	VariableInWrongPlace,
	CannotMakeIntoScope,
}

impl From<UnrecoverableError> for KismesisError {
	fn from(val: UnrecoverableError) -> KismesisError {
		KismesisError::UnrecoverableError(val)
	}
}

pub fn report_error(token_scanner: TokenScanner, unrecoverable_error: Option<ErrorState<HtmlGenerationError>>, recovered_errors: Vec<ErrorState<HtmlGenerationError>>) -> String {
	let mut output = String::from("Couldn't compile due to the following errors:\n");
	for error in recovered_errors {
		output.push_str(&format!(" At line {} ", error.line).on_red().black().to_string());
 		output.push('\n');
		output.push_str(&error.report(&token_scanner));
		match error.error {
			HtmlGenerationError::UnrecoverableError(_) => {
				output.push_str("The previous error was caused due to an internal issue with the Kismesis Compiler");
				output.push_str("Please report this issue");
			}
			HtmlGenerationError::KismesisError(_) => (),
		}
		output.push('\n');
		output.push('\n');
	}
	let Some(unrecoverable_error) = unrecoverable_error else {
		output.push_str("All errors were caught");
		return output
	};
	output.push_str("The following error was unrecoverable, so compilation stopped here and some errors may not have been caught:\n");
	output.push_str(&format!(" At line {} ", unrecoverable_error.line).on_red().black().to_string());
	output.push('\n');
	output.push_str(&unrecoverable_error.report(&token_scanner));
	output
}


pub fn turn_to_chars(s: &str, c: char) -> String {
	let mut out = String::new();
	for k in s.chars() {
		match k {
			'\t' => {
				for _ in 0..4 { out.push(c) }
			},
			_ => out.push(c),
		}
	}
	out
}

pub fn report_line(line: &[lexer::Token], line_number: usize, max_line_number: usize, error_char: Option<(usize, &String)>) -> String {
	let mut output = get_line_number(line_number, max_line_number);
	let mut error_decoration = turn_to_chars(&output, ' ');
	match error_char {
		None => output = output.bright_black().to_string(),
		Some(_) => output = output.yellow().to_string(),
	}
	for (cidx, token) in line.iter().enumerate() {
		if let lexer::Token::Newline(_) = token {
			match error_char {
				Some((n, _)) if n == cidx => output.push_str(&"~".red().to_string()),
				_ => output.push(' '),
			}
		} else { 
			match error_char {
				Some((n, _)) if n == cidx => output.push_str(&token.get_as_string().red().to_string()),
				_ => output.push_str(&token.get_as_string().replace('\t', "    ")),
			}
		}
		if let Some((idx, message)) = error_char {
			match cidx.cmp(&idx) {
				Ordering::Less => error_decoration.push_str(&turn_to_chars(&token.get_as_string(), ' ')),
				Ordering::Equal => {
					error_decoration.push_str(&turn_to_chars(&token.get_as_string(), '^').yellow().to_string());
					error_decoration.push(' ');
					error_decoration.push_str(&message.yellow().to_string());
				},
				_ => ()
			}
		}
	}
	match error_char {
		None => (),
		Some(_) => {
			output.push('\n');
			output.push_str(&error_decoration);
		},
	}
	output
}

pub fn get_line_number(n: usize, mexn: usize) -> String {
	let mexns = mexn.to_string();
	let ns = n.to_string();
	let mut out = String::from("  ");
	for _ in ns.len()..mexns.len() {
		out.push(' ');
	}
	out.push_str(&ns);
	out.push_str(" | ");
	out
}

pub struct CompilerErrorReport<T: Error> {
	pub scanner: TokenScanner,
	pub unresolved: Option<ErrorState<T>>,
	pub resolved: Vec<ErrorState<T>>
}

impl From<CompilerErrorReport<KismesisError>> for CompilerErrorReport<HtmlGenerationError> {
	fn from(val: CompilerErrorReport<KismesisError>) -> CompilerErrorReport<HtmlGenerationError> {
		CompilerErrorReport {
			scanner: val.scanner,
			unresolved: val.unresolved.map(|x| x.into()),
			resolved: val.resolved.into_iter().map(|x| x.into()).collect(),
		}
	}
}

impl From<ErrorState<KismesisError>> for ErrorState<HtmlGenerationError> {
	fn from (val:ErrorState<KismesisError>) -> ErrorState<HtmlGenerationError> {
		ErrorState {
			error: val.error.into(),
			line_position: val.line_position,
			line: val.line,
			sub_errors: val.sub_errors.map(|x| x.into_iter().map(|x| x.into()).collect()),
		}
	}
}

pub trait SpecialFrom<F: Error, T: Error> {
	fn from(self) -> ErrorState<T>;
}

impl<F: Error, T: Error + From<F>> SpecialFrom<F, T> for ErrorState<F> {
	fn from(self) -> ErrorState<T> {
		ErrorState {
			error: self.error.into(),
			line_position: self.line_position,
			line: self.line,
			sub_errors: self.sub_errors.map(|x| x.into_iter().map(|x| x.from()).collect()),
		}
	}
}

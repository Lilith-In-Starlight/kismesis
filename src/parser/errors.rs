//! Everything related to parsing errors.

use std::fmt::{Display, Write};
use std::ops::Bound;

use crate::errors::MaybeStateless;
use crate::KisTemplateId;
use crate::{
	errors::{ErrorKind, ErrorState, StatelessError},
	html::ScopedError,
	KisTokenId,
};

use super::{state::State, types::MultilineRange};

#[derive(Clone, Debug)]
pub enum Expected {
	Mut,
	Set,
	Const,
	Equals,
	EOF,
	ExpressionStart,
	ExpressionEnd,
	MacroDefinition,
	MacroMark,
	PluginMark,
	UnaryFunction,
	BinaryFunction,
	VariableName,
	TagName,
	BodyOpener,
	TagCloser,
	TagOpener,
	TagComposer,
	Parameter,
}

impl Display for Expected {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Mut => write!(f, "`$mut`"),
			Self::Set => write!(f, "`$set`"),
			Self::Const => write!(f, "$const"),
			Self::Equals => write!(f, "equals sign (`=`)"),
			Self::EOF => write!(f, "end of file"),
			Self::ExpressionStart => write!(f, "expression starter (`{{`)"),
			Self::ExpressionEnd => write!(f, "expression ender (`}}`)"),
			Self::MacroDefinition => write!(f, "macro definition (the word `macro`)"),
			Self::MacroMark => write!(f, "macro specifier (`!`)"),
			Self::PluginMark => write!(f, "plugin specifier (`?`)"),
			Self::UnaryFunction => write!(f, "unary function (`not`)"),
			Self::BinaryFunction => write!(f, "binary function (`or` or `and`)"),
			Self::VariableName => write!(f, "variable name"),
			Self::TagName => write!(f, "tag name"),
			Self::BodyOpener => write!(f, "body opener (`:`)"),
			Self::TagCloser => write!(f, "tag closer (`>`)"),
			Self::TagOpener => write!(f, "tag opener (`<`)"),
			Self::TagComposer => write!(f, "tag composition (`+`)"),
			Self::Parameter => write!(f, "parameter"),
		}
	}
}

#[derive(Clone, Debug)]
pub enum ParseError {
	Expected(Vec<Expected>),
	InvalidAttrName,
	SuspiciousStmtString,
	UnregisteredFileID(KisTokenId),
	HeaderNotAllowedHere,
	SkippedHeadingLevel(usize),
	IncorrectHeaderNumber,
	IncorrectChild(String),
	ThisTagCannotBeEmpty(String),
	UsedDiv,
	PluginError(String),
	ExtismError(String),
	PluginDoesntExist,
	PluginIsUndeclared,
	PluginsDisabled,
	TriedToParseInvalidID(KisTokenId),
	WronglyNestedSection,
	ConditionUnmet,
	NotInRange(Bound<usize>, Bound<usize>),
	ExpressionInSetStmt,
	TagOpenerMismatch,
	TagCloserMismatch,
	LiteralNotMatch {
		expected: String,
		got: Option<String>,
	},
	NotANewline,
	NotLiteral,
	UnexpectedMacroDef,
	EmptyString,
	NotSymbol,
	NotMacroStart,
	CharacterNotMatch {
		expected: char,
		got: Option<char>,
	},
	NotQuoteMark,
	NotASpace,
	NotAnIndent,
	ReachedEOF,
	EndlessString,
}

#[derive(Clone, Debug)]
pub enum Err {
	Error(ScopedError<ParseError>),
	Failure(ScopedError<ParseError>),
}

impl Hintable for Err {
	fn add_hint(&mut self, hint: Hint) {
		match self {
			Self::Error(x) | Self::Failure(x) => x.add_hint(hint),
		}
	}

	fn get_hints(&self) -> &[Hint] {
		self.unpack_ref().get_hints()
	}

	fn get_hints_mut(&mut self) -> &mut [Hint] {
		self.unpack_mut().get_hints_mut()
	}
}

pub trait Unpack<T> {
	fn unpack(self) -> T;
	fn unpack_ref(&self) -> &T;
	fn unpack_mut(&mut self) -> &mut T;
}

impl Unpack<ScopedError<ParseError>> for Err {
	fn unpack(self) -> ScopedError<ParseError> {
		match self {
			Self::Error(x) | Self::Failure(x) => x,
		}
	}

	fn unpack_ref(&self) -> &ScopedError<ParseError> {
		match self {
			Self::Error(x) | Self::Failure(x) => x,
		}
	}

	fn unpack_mut(&mut self) -> &mut ScopedError<ParseError> {
		match self {
			Self::Error(x) | Self::Failure(x) => x,
		}
	}
}

impl<T> Unpack<T> for MaybeStateless<T> {
	fn unpack(self) -> T {
		match self {
			Self::Stateful(x) => x.error,
			Self::Statelss(x) => x.error,
		}
	}

	fn unpack_ref(&self) -> &T {
		match self {
			Self::Stateful(x) => &x.error,
			Self::Statelss(x) => &x.error,
		}
	}

	fn unpack_mut(&mut self) -> &mut T {
		match self {
			Self::Stateful(x) => &mut x.error,
			Self::Statelss(x) => &mut x.error,
		}
	}
}

impl Err {
	#[must_use]
	pub fn cut(self) -> Self {
		match self {
			Self::Error(x) => Self::Failure(x),
			x @ Self::Failure(_) => x,
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Hints {
	StackIsThis(Box<[KisTemplateId]>),
	HeaderSectionDynamics,
	HeaderForSize,
	ArgumentDefinedHere,
	ReferenceToThis,
	CustomMessage(String),
	DontUseDiv,
	SectionTagContents,
	HeaderForLargeText,
	HgroupContents,
}

impl ErrorKind for Hints {
	fn get_text(&self) -> String {
		match self {
			Self::StackIsThis(stack) => {
				let formatted = stack.iter().fold(String::new(), |mut output, current| {
					let current = match current {
						KisTemplateId::Input(x) => format!("Command Line Input #{x}"),
						KisTemplateId::File(x) => x.display().to_string(),
					};
					if !output.is_empty() {
						let _ = writeln!(&mut output);
					}
					let _ = write!(&mut output, " - {current}");
					output
				});
				format!("This error could be caused by a mistake made in any of these templates:\n{formatted}")
			},
			Self::HeaderSectionDynamics => "Headers must always be the first child of a `<section>`, and their number must correspond to the amount of nested sections".into(),
			Self::HeaderForSize => "If you're trying to control the size of text for aesthetic purposes, use CSS instead".into(),
			Self::HgroupContents => "An `<hgroup>` must have a heading (e.g. `<h1>` `<h2>`, etc) as its first child".into(),
			Self::HeaderForLargeText => "If you're trying to create smaller text, use CSS instead".into(),
			Self::SectionTagContents => "A `<section>` tag must contain a heading (e.g. `<h1>` `<h2>`, etc) or an `<hgroup>` as its first child.".into(),
			Self::DontUseDiv => "Consider using a more semantic alternative like `section`, `header`, `main`, `footer`, or `button`. If you really need `<div>`, use `<container>`.".into(),
			Self::ArgumentDefinedHere => "Argument defined here".into(),
			Self::ReferenceToThis => "Value comes from here".into(),
			Self::CustomMessage(string) => string.clone(),
		}
	}
}

impl Hints {
	#[must_use]
	pub fn with_state_at(self, state: MultilineRange, scope: KisTokenId) -> Hint {
		Hint::Stateful(ScopedError {
			error: ErrorState {
				error: self,
				text_position: state,
				hints: vec![],
			}
			.into(),
			scope,
		})
	}
	#[must_use]
	pub fn stateless(self) -> Hint {
		Hint::Stateless(StatelessError {
			error: self,
			hints: vec![],
		})
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Hint {
	Stateful(ScopedError<Hints>),
	Stateless(StatelessError<Hints>),
}

pub trait Hintable
where
	Self: Sized,
{
	fn get_hints(&self) -> &[Hint];
	fn get_hints_mut(&mut self) -> &mut [Hint];
	fn add_hint(&mut self, hint: Hint);
	#[must_use]
	fn with_hint(mut self, hint: Hint) -> Self {
		self.add_hint(hint);
		self
	}
}

impl ParseError {
	/// Unlike the implementation in `ErrorKind`, this method outputs an [`Err`],
	/// containing information about when the parser should stop considering
	/// options and simply should crash
	pub(crate) fn error_at(self, state: &State) -> Err {
		let pos = state.get_end_position();
		let a = ErrorState {
			error: self,
			text_position: MultilineRange::Single(pos),
			hints: vec![],
		};
		Err::Error(ScopedError {
			error: MaybeStateless::Stateful(a),
			scope: state.current_file,
		})
	}
	pub(crate) fn error_at_pos(self, text_position: MultilineRange, scope: KisTokenId) -> Err {
		let a = ErrorState {
			error: self,
			text_position,
			hints: vec![],
		};
		Err::Error(ScopedError {
			error: MaybeStateless::Stateful(a),
			scope,
		})
	}
}

impl ErrorKind for ParseError {
	fn get_text(&self) -> String {
		match self {
			Self::InvalidAttrName => "Invalid attribute name".to_string(),
			Self::SuspiciousStmtString => r"This looks too much like a statement. If it isn't meant to be one, add a `\` before the first word".to_string(),
			Self::UnregisteredFileID(id) => format!("No file with the ID {} has been registered", id.0),
			Self::Expected(expected) => {
				if expected.len() == 1 {
					let expected = expected.first().expect("It is impossible for this to fail");
					format!("Expected {expected}")
				} else {
					let mut x = String::from("Expected one of the following:\n");
					for expected in expected {
						writeln!(x, " - {expected}").unwrap();
					}
					x
				}
			}
			Self::HeaderNotAllowedHere => "Headers are not allowed outside sections".into(),
			Self::SkippedHeadingLevel(expected) => format!("Skipped heading level - expected {expected}"),
			Self::IncorrectHeaderNumber => "Headers can only go from 1 up to 6".to_string(),
			Self::IncorrectChild(parent) => {
				format!("This tag is incorrect as a child of a `<{parent}>` tag")
			}
			Self::ThisTagCannotBeEmpty(name) => format!("The `<{name}>` tag cannot be empty"),
			Self::UsedDiv => "<div> tags are discouraged by Kismesis".to_string(),
			Self::PluginError(message) => message.to_owned(),
			Self::ExtismError(message) => format!("Plugin failed: {message}"),
			Self::PluginDoesntExist => "This plugin does not exist".to_string(),
			Self::PluginsDisabled => {
				"This version of kismesis was not made with the `plugins` feature".to_string()
			}
			Self::TriedToParseInvalidID(id) => {
				format!("Tried to parse a file with invalid ID: {id:?}")
			}
			Self::WronglyNestedSection => "Wrongly nested section".to_string(),
			Self::ConditionUnmet => "Unmet condition".to_string(),
			Self::NotInRange(start, end) => format!(
				"Expected this to repeat from {start:?} to {end:?} times"
			),
			Self::ExpressionInSetStmt => "Expressions are not allowed in `set` statements".into(),
			Self::TagOpenerMismatch => "This `<` is never closed".into(),
			Self::TagCloserMismatch => {
				"This `>` is mismatched. Check if all tags before it are closed correctly".into()
			}
			Self::LiteralNotMatch { expected, .. } => format!("Expected the word `{expected}`"),
			Self::NotANewline => "Expected a newline".into(),
			Self::NotLiteral => "Expected a word".into(),
			Self::UnexpectedMacroDef => "Expected a tag name, not a macro definition".into(),
			Self::EmptyString => "Empty string".into(),
			Self::NotSymbol => "Expected a special character".into(),
			Self::NotMacroStart => "Expected the start of a macro".into(),
			Self::CharacterNotMatch { expected, .. } => format!("Expected `{expected}`"),
			Self::NotQuoteMark => "Expected a quotation mark".into(),
			Self::NotASpace => "Expected a space".into(),
			Self::NotAnIndent => "Expected an indent (tab key)".into(),
			Self::ReachedEOF => "Reached end of file".into(),
			Self::EndlessString => "String reaches end of file".into(),
			Self::PluginIsUndeclared => "This plugin hasn't been declared in the kismet file".into()
		}
	}
}

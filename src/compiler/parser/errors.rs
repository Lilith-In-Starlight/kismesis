use std::ops::Bound;

use crate::{
	compiler::{
		errors::{ErrorKind, ErrorState, StatelessError},
		html::ScopedError,
	},
	kismesis::KisID,
};

use super::{state::ParserState, types::TextPos};

#[derive(Clone, Debug)]
pub enum ParseError {
	TriedToParseInvalidID(KisID),
	WronglyNestedSection,
	ExpectedLambdaStart,
	ConditionUnmet,
	NotInRange(Bound<usize>, Bound<usize>),
	ExpressionInSetStmt,
	ExpectedSetStarter,
	TagOpenerMismatch,
	TagCloserMismatch,
	ExpectedEOF,
	ExpectedEquals,
	LiteralNotMatch {
		expected: String,
		got: Option<String>,
	},
	ExpectedExprStart,
	ExpectedExprEnd,
	ExpectedMacroMark,
	ExpectedPluginMark,
	ExpectedUniFunc,
	ExpectedBinFunc,
	ExpectedVarName,
	ExpectedTagNameOrMacroDef,
	ExpectedBodyOpener,
	ExpectedTagName,
	ExpectedTagCloser,
	ExpectedTagOpener,
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
	Error(ErrorState<ParseError>),
	Failure(ErrorState<ParseError>),
}

impl Err {
	pub fn unpack(self) -> ErrorState<ParseError> {
		match self {
			Self::Error(x) => x,
			Self::Failure(x) => x,
		}
	}

	pub fn cut(self) -> Err {
		match self {
			Self::Error(x) => Err::Failure(x),
			x => x,
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Hints {
	ArgumentDefinedHere,
	ReferenceToThis,
}

impl ErrorKind for Hints {
	fn get_text(&self) -> String {
		match self {
			Self::ArgumentDefinedHere => "Argument defined here".into(),
			Self::ReferenceToThis => "Value comes from here".into(),
		}
	}
}

impl Hints {
	pub fn with_state_at(self, state: TextPos, scope: KisID) -> Hint {
		Hint::Stateful(ScopedError {
			error: ErrorState {
				error: self,
				text_position: state,
				hints: vec![],
			},
			scope,
		})
	}
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
	fn add_hint(&mut self, hint: Hint);
	fn with_hint(mut self, hint: Hint) -> Self {
		self.add_hint(hint);
		self
	}
}

impl ParseError {
	/// Unlike the implementation in ErrorKind, this method outputs an [`Err`],
	/// containing information about when the parser should stop considering
	/// options and simply should crash
	pub(crate) fn error_at(self, state: &ParserState) -> Err {
		let pos = state.position;
		Err::Error(ErrorState {
			error: self,
			text_position: TextPos::Single(pos),
			hints: vec![],
		})
	}
}

impl ErrorKind for ParseError {
	fn get_text(&self) -> String {
		match self {
			Self::TriedToParseInvalidID(id) => {
				format!("Tried to parse a file with invalid ID: {:?}", id)
			}
			Self::WronglyNestedSection => "Wrongly nested section".to_string(),
			Self::ExpectedLambdaStart => "Expected `lambda`".to_string(),
			Self::ConditionUnmet => "Unmet condition".to_string(),
			Self::NotInRange(start, end) => format!(
				"Expected this to repeat from {:?} to {:?} times",
				start, end
			),
			Self::ExpressionInSetStmt => "Expressions are not allowed in `set` statements".into(),
			Self::ExpectedSetStarter => "Expected `set`".into(),
			Self::TagOpenerMismatch => "This `<` is never closed".into(),
			Self::TagCloserMismatch => {
				"This `>` is mismatched. Check if all tags before it are closed correctly".into()
			}
			Self::ExpectedEOF => {
				"Expected the file to end, but it didn't. You might have too many `>`".into()
			}
			Self::LiteralNotMatch { expected, .. } => format!("Expected the word `{}`", expected),
			Self::ExpectedExprStart => "Expected `[` to denote the start of an expression".into(),
			Self::ExpectedExprEnd => "Expected `]`to denote the end of an expression".into(),
			Self::ExpectedMacroMark => "Expected `!` to denote a macro call".into(),
			Self::ExpectedPluginMark => "Expected `?` to denote a plugin call".into(),
			Self::ExpectedUniFunc => "Expected `not` or some other unary function".into(),
			Self::ExpectedBinFunc => "Expected `and`, `or` or some other binary function".into(),
			Self::ExpectedVarName => "Expected a valid variable name".into(),
			Self::ExpectedTagNameOrMacroDef => "Expected a tag name or the word `macro`".into(),
			Self::ExpectedBodyOpener => {
				"Expected a `|` or a newline to denote the start of the tag's body".into()
			}
			Self::ExpectedTagName => "Expected a valid tag name".into(),
			Self::ExpectedTagCloser => "Expected a `>` to denote the end of a tag".into(),
			Self::ExpectedTagOpener => "Expected a `<` to denote the start of a tag".into(),
			Self::NotANewline => "Expected a newline".into(),
			Self::NotLiteral => "Expected a word".into(),
			Self::UnexpectedMacroDef => "Expected a tag name, not a macro definition".into(),
			Self::EmptyString => "Empty string".into(),
			Self::NotSymbol => "Expected a special character".into(),
			Self::NotMacroStart => "Expected the start of a macro".into(),
			Self::CharacterNotMatch { expected, .. } => format!("Expected `{}`", expected),
			Self::NotQuoteMark => "Expected a quotation mark".into(),
			Self::NotASpace => "Expected a space".into(),
			Self::NotAnIndent => "Expected an indent (tab key)".into(),
			Self::ReachedEOF => "Reached end of file".into(),
			Self::EndlessString => "String reaches end of file".into(),
			Self::ExpectedEquals => "Expected an equals sign `=`".into(),
		}
	}
}

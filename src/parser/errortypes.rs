use crate::lexer::Token;

use super::state::ParserState;

#[derive(Clone, Debug)]
pub enum Error {
	ExpectedUniFunc,
	ExpectedBinFunc,
	ExpectedVarName,
	ExpectedTagNameOrMacroDef,
	ExpectedBodyOpener,
	ExpectedTagName,
	ExpectedTagCloser,
	ExpectedVarCaller,
	ExpectedTagOpener,
	NewlineInQuote,
	NotANewline,
	NotLiteral,
	UnexpectedMacroDef,
	UnendingZero,
	EmptyString,
    NotSymbol,
	NotMacroStart,
    CharacterNotMatch { expected: char, got: Option<char> },
    NotQuoteMark,
    ExpectedQuoteStart,
    NotASpace,
    NotAnIndent,
	EndlessName,
    UnclosedQuote,
	InvalidSymbolsInParamName,
	InvalidSymbolsInTagName,
	EmptyName,
	ExpectedValue,
	ReachedEOF,
	EndlessString,
}

#[derive(Clone, Debug)]
pub enum Err<'a> {
	Error(ErrorState<'a>),
	Failure(ErrorState<'a>),
}

impl<'a> Err<'a> {
	pub fn unpack(self) -> ErrorState<'a> {
		match self {
			Self::Error(x) => x,
			Self::Failure(x) => x,
		}
	}
}

impl Error {
	pub(crate) fn state_at<'a>(self, state: &ParserState<'a>) -> Err<'a> {
        let pos = (state.line, state.column);
        Err::Error(ErrorState { error: self, start_position: pos, previous_errors: state.clone().errors, end_position: pos, tokens: state.tokens})
	}
}

#[derive(Clone, Debug)]
pub struct ErrorState<'a> {
	pub error: Error,
	pub previous_errors: Vec<ErrorState<'a>>,
    pub start_position: (usize, usize),
    pub end_position: (usize, usize),
	pub tokens: &'a [Token]
}

pub(crate) trait Recoverable {
    fn empty() -> Self;
}

impl<'a> Recoverable for &'a char {
    fn empty() -> Self {
        &' '
    }
}

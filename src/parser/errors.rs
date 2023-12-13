use super::state::ParserState;

#[derive(Clone, Debug)]
pub(crate) enum Error {
    NotSymbol,
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
}

impl Error {
	pub(crate) fn state_at<'a>(self, state: &ParserState<'a>) -> ErrorState {
        let pos = (state.line, state.column);
        ErrorState { error: self, start_position: pos, end_position: pos }
	}
}

#[derive(Debug, Clone)]
pub(crate) enum ParserResult<'a, T> {
    Ok(T, ParserState<'a>),
    Err(ErrorState, ParserState<'a>),
}

impl<'a, T> ParserResult<'a, T> {
    pub(crate) fn err(error: Error, state: ParserState<'a>) -> Self {
        let pos = (state.line, state.column);
        Self::Err(ErrorState { error, start_position: pos, end_position: pos }, state)
    }

	pub(crate) fn state(self) -> ParserState<'a> {
		match self {
			Self::Err(_, state) => state,
			Self::Ok(_, state) => state
		}
	}
}

#[derive(Clone, Debug)]
pub(crate) struct ErrorState {
	pub(crate) error: Error,
    pub(crate) start_position: (usize, usize),
    pub(crate) end_position: (usize, usize),
}

pub(crate) trait Recoverable {
    fn empty() -> Self;
}

impl<'a> Recoverable for &'a char {
    fn empty() -> Self {
        &' '
    }
}

use super::{state::ParserState, types::TextPos};

#[derive(Clone, Debug)]
pub enum Error {
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
pub enum Err {
    Error(ErrorState<Error>),
    Failure(ErrorState<Error>),
}

impl Err {
    pub fn unpack(self) -> ErrorState<Error> {
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

impl Error {
    pub(crate) fn state_at<'a>(self, state: &ParserState<'a>) -> Err {
        let pos = state.position;
        Err::Error(ErrorState {
            error: self,
            text_position: TextPos::Single(pos),
            previous_errors: state.clone().errors,
        })
    }
}

#[derive(Clone, Debug)]
pub struct ErrorState<T> {
    pub error: T,
    pub previous_errors: Vec<ErrorState<T>>,
    pub text_position: TextPos,
}

pub(crate) trait Recoverable {
    fn empty() -> Self;
}

impl<'a> Recoverable for &'a char {
    fn empty() -> Self {
        &' '
    }
}

use crate::compiler::lexer::Token;

use super::errors::{ErrorState, Error};

#[derive(Clone, Debug)]
pub struct ParserState<'a> {
    pub(crate) tokens: &'a [Token],
    pub(crate) position: TokenPos,
    pub(crate) errors: Vec<ErrorState<Error>>,
}

impl<'a> ParserState<'a> {
    pub(crate) fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            position: TokenPos::new(),
            errors: vec![],
        }
    }
    pub(crate) fn next_state(self) -> Self {
        let next_token = self.tokens.get(1);
        let position = match next_token {
            Some(Token::Newline(_)) => self.position.next_line(),
            _ => self.position.next_character(),
        };
        Self {
            tokens: self.tokens.get(1..).unwrap_or(&[]),
            position,
            ..self
        }
    }

    pub(crate) fn first_token(&self) -> Option<&Token> {
        self.tokens.first()
    }

    pub(crate) fn advanced(&self) -> (Option<&'a Token>, ParserState<'a>) {
        (self.tokens.first(), self.clone().next_state())
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub struct TokenPos {
    idx: usize,
    line: usize,
    column: usize,
}

impl TokenPos {
    pub fn new() -> Self {
        Self {
            idx: 0,
            line: 0,
            column: 0,
        }
    }

    fn next_character(self) -> Self {
        Self {
            idx: self.idx + 1,
            column: self.idx + 1,
            ..self
        }
    }

    fn next_line(self) -> Self {
        Self {
            idx: self.idx + 1,
            column: 0,
            line: self.line + 1,
        }
    }
}

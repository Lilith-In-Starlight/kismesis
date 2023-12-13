use crate::lexer::Token;

use super::errors::ErrorState;

#[derive(Clone, Debug)]
pub(crate) struct ParserState<'a> {
    pub(crate) tokens: &'a [Token],
    pub(crate) line: usize,
    pub(crate) column: usize,
    pub(crate) errors: Vec<ErrorState>
}

impl<'a> ParserState<'a>{
	pub(crate) fn new(tokens: &'a [Token]) -> Self {
		Self {
			tokens,
			line: 0,
			column: 0,
			errors: vec![],
		}
	}
    pub(crate) fn next_state(self) -> Self {
        let next_token = self.tokens.get(1);
        let (line, column) = match next_token {
            Some(Token::Newline(_)) => (self.line + 1, 0),
            _ => (self.line, self.column + 1),
        };
        Self {
            tokens: self.tokens.get(1..).unwrap_or(&[]),
            line,
            column,
            errors: Vec::new(),
        }
    }

    pub(crate) fn first_token(&self) -> Option<&Token> {
        self.tokens.first()
    }

    pub(crate) fn advanced(&self) -> (Option<&'a Token>, ParserState<'a>) {
        (self.tokens.first(), self.clone().next_state())
    }
}

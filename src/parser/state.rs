use std::path::PathBuf;

use crate::{errors::ErrorState, lexer::Token, Kismesis};

use super::{errors::ParseError, types::TextPos};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
pub struct ParserState<'a> {
	pub(crate) tokens: &'a [Token],
	pub(crate) position: TokenPos,
	pub(crate) errors: Vec<ErrorState<ParseError>>,
	pub(crate) tag_openers: Vec<TokenPos>,
	pub(crate) section_depth: usize,
	pub(crate) file_path: Option<PathBuf>,
	pub(crate) engine: &'a Kismesis,
}

impl<'a> ParserState<'a> {
	pub(crate) fn new(
		tokens: &'a [Token],
		file_path: Option<PathBuf>,
		engine: &'a Kismesis,
	) -> Self {
		Self {
			tokens,
			position: TokenPos::new(),
			errors: vec![],
			tag_openers: Vec::new(),
			section_depth: 0,
			engine,
			file_path,
		}
	}
	pub(crate) fn next_state(self) -> Self {
		let next_token = self.tokens.first();
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

	pub(crate) fn close_tag(&self) -> Result<Self, ParseError> {
		if !self.tag_openers.is_empty() {
			let mut clone = self.clone();
			clone.tag_openers.pop();
			Ok(Self {
				tag_openers: clone.tag_openers,
				..clone
			})
		} else {
			Err(ParseError::TagCloserMismatch)
		}
	}

	pub(crate) fn open_tag(&self) -> Self {
		let clone = self.clone();
		Self {
			tag_openers: clone
				.tag_openers
				.into_iter()
				.chain(vec![self.position])
				.collect(),
			..clone
		}
	}

	pub(crate) fn below_scope(&self) -> Self {
		let clone = self.clone();
		Self {
			section_depth: clone.section_depth + 1,
			..clone
		}
	}

	pub(crate) fn above_scope(&self) -> Self {
		let clone = self.clone();
		Self {
			section_depth: clone.section_depth - 1,
			..clone
		}
	}
}

#[derive(Clone, Debug, Copy, PartialEq, Default)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
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

	pub fn new_at(idx: usize, line: usize, column: usize) -> Self {
		Self { idx, line, column }
	}
	pub fn get_idx(&self) -> usize {
		self.idx
	}

	pub fn get_line(&self) -> usize {
		self.line
	}

	pub fn get_column(&self) -> usize {
		self.column
	}

	pub fn is_in(&self, o: &TextPos) -> bool {
		match o {
			TextPos::Single(x) => x == self,
			TextPos::Range((st, nd)) => self.idx >= st.idx && self.idx < nd.idx,
			TextPos::Multi(x) => x.iter().any(|x| self.is_in(x)),
		}
	}
	pub fn is_at_a_start(&self, o: &TextPos) -> bool {
		match o {
			TextPos::Single(x) => x == self,
			TextPos::Range((st, _)) => self.idx == st.idx,
			TextPos::Multi(x) => x.iter().any(|x| self.is_at_a_start(x)),
		}
	}
	pub fn is_at_an_end(&self, o: &TextPos) -> bool {
		match o {
			TextPos::Single(x) => x.idx == self.idx,
			TextPos::Range((_, nd)) => self.idx == nd.idx - 1,
			TextPos::Multi(x) => x.iter().any(|x| self.is_at_an_end(x)),
		}
	}

	fn next_character(self) -> Self {
		Self {
			idx: self.idx + 1,
			column: self.column + 1,
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

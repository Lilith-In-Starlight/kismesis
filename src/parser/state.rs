use std::path::PathBuf;

use crate::{lexer::Token, Kismesis};

use super::{
	errors::{Err, ParseError},
	types::TextPos,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
pub struct State<'a> {
	pub(crate) tokens: &'a [Token],
	pub(crate) errors: Vec<Err>,
	pub(crate) tag_openers: Vec<TokenPos>,
	pub(crate) section_depth: usize,
	pub(crate) file_path: Option<PathBuf>,
	pub(crate) engine: &'a Kismesis,
}

impl<'a> State<'a> {
	pub(crate) fn new(
		tokens: &'a [Token],
		file_path: Option<PathBuf>,
		engine: &'a Kismesis,
	) -> Self {
		Self {
			tokens,
			errors: vec![],
			tag_openers: Vec::new(),
			section_depth: 0,
			engine,
			file_path,
		}
	}

	pub(crate) fn get_end_position(&self) -> TokenPos {
		self.first_token()
			.map_or_else(TokenPos::default, |first_token| {
				first_token.get_end_position()
			})
	}

	pub(crate) fn get_start_position(&self) -> TokenPos {
		self.first_token()
			.map_or_else(TokenPos::default, |first_token| {
				first_token.get_start_position()
			})
	}
	pub(crate) fn next_state(self) -> Self {
		Self {
			tokens: self.tokens.get(1..).unwrap_or(&[]),
			..self
		}
	}

	pub(crate) fn with_error(self, error: Err) -> Self {
		let mut errors = self.errors;
		errors.push(error);
		Self { errors, ..self }
	}

	pub(crate) const fn first_token(&self) -> Option<&Token> {
		self.tokens.first()
	}

	pub(crate) fn advanced(&self) -> (Option<&'a Token>, Self) {
		(self.tokens.first(), self.clone().next_state())
	}

	pub(crate) fn close_tag(&self) -> Result<Self, ParseError> {
		if self.tag_openers.is_empty() {
			Err(ParseError::TagCloserMismatch)
		} else {
			let mut clone = self.clone();
			clone.tag_openers.pop();
			Ok(Self {
				tag_openers: clone.tag_openers,
				..clone
			})
		}
	}

	pub(crate) fn open_tag(&self) -> Self {
		let clone = self.clone();
		Self {
			tag_openers: clone
				.tag_openers
				.into_iter()
				.chain(vec![self.get_end_position()])
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

#[derive(Clone, Debug, Copy, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TokenPos {
	pub(crate) idx: usize,
	pub(crate) line: usize,
	pub(crate) column: usize,
}

impl TokenPos {
	pub const fn new() -> Self {
		Self {
			idx: 0,
			line: 0,
			column: 0,
		}
	}

	pub const fn new_at(idx: usize, line: usize, column: usize) -> Self {
		Self { idx, line, column }
	}
	pub const fn get_idx(&self) -> usize {
		self.idx
	}

	pub const fn get_line(&self) -> usize {
		self.line
	}

	pub const fn get_column(&self) -> usize {
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
			TextPos::Range((_, nd)) => self.idx == nd.idx,
			TextPos::Multi(x) => x.iter().any(|x| self.is_at_an_end(x)),
		}
	}

	pub fn next_character(&mut self) {
		self.idx += 1;
		self.column += 1;
	}

	pub fn next_line(&mut self) {
		self.idx += 1;
		self.column = 0;
		self.line += 1;
	}

	pub const fn get_previous_character(&self) -> Self {
		Self {
			idx: self.idx - 1,
			column: self.column - 1,
			..*self
		}
	}
}

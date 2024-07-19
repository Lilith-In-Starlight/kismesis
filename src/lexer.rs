//! Module regarding [`Token`]s and tokenization.
use std::fmt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::parser::state::TokenPos;

/// The different tokens that can be in an input string
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Token {
	Word {
		content: String,
		start: TokenPos,
		end: TokenPos,
	},
	Space {
		content: char,
		position: TokenPos,
	},
	Newline {
		content: char,
		position: TokenPos,
	},
	Indent {
		content: char,
		position: TokenPos,
	},
	Symbol {
		content: char,
		position: TokenPos,
	},
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum TokenRange {
	Single(TokenPos),
	Range(TokenPos, TokenPos),
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Word { content: word, .. } => write!(f, "{word}"),
			Self::Space { content: c, .. }
			| Self::Newline { content: c, .. }
			| Self::Indent { content: c, .. }
			| Self::Symbol { content: c, .. } => write!(f, "{c}"),
		}
	}
}

impl Token {
	#[must_use]
	pub const fn get_range(&self) -> TokenRange {
		match self {
			Self::Word { start, end, .. } => TokenRange::Range(*start, *end),
			Self::Space { position, .. }
			| Self::Newline { position, .. }
			| Self::Indent { position, .. }
			| Self::Symbol { position, .. } => TokenRange::Single(*position),
		}
	}
	/// Pushes the content of the token into a string
	pub fn push_to_string(&self, to: &mut String) {
		match self {
			Self::Word {
				content: ref word, ..
			} => to.push_str(word),
			Self::Space { content: ref c, .. }
			| Self::Newline { content: ref c, .. }
			| Self::Indent { content: ref c, .. }
			| Self::Symbol { content: ref c, .. } => to.push(*c),
		}
	}

	#[must_use]
	pub const fn get_end_position(&self) -> TokenPos {
		match self {
			Self::Word { end, .. } => end.get_previous_character(),
			Self::Space { position, .. }
			| Self::Newline { position, .. }
			| Self::Indent { position, .. }
			| Self::Symbol { position, .. } => *position,
		}
	}

	#[must_use]
	pub const fn get_start_position(&self) -> TokenPos {
		match self {
			Self::Word { start, .. } => *start,
			Self::Space { position, .. }
			| Self::Newline { position, .. }
			| Self::Indent { position, .. }
			| Self::Symbol { position, .. } => *position,
		}
	}
}

/// Converts a string into a `Vec<Token>`, ignoring `\r` characters.
#[must_use]
#[allow(clippy::too_many_lines)]
pub fn tokenize(s: &str) -> Vec<Token> {
	let s: String = {
		let mut buffer = Vec::new();
		let mut buffer2 = Vec::new();
		let mut comments = false;
		for x in s.chars() {
			if comments {
				buffer2.push(x);
				if let [.., '-', '>'] = buffer2.as_slice() {
					buffer2.clear();
					comments = false;
				}
			} else {
				buffer.push(x);
				if let [.., '<', '!', '-'] = buffer.as_slice() {
					buffer.pop();
					buffer.pop();
					buffer.pop();
					comments = true;
				}
			}
		}
		buffer.into_iter().collect()
	};

	let mut output: Vec<Token> = vec![];
	let mut current_position = TokenPos::default();
	let mut current_word_start = TokenPos::default();

	for character in s.chars() {
		match character {
			' ' => {
				push_token(
					Token::Space {
						content: character,
						position: current_position,
					},
					&mut output,
					&mut current_word_start,
					&s,
				);
				current_position.next_character();
			}
			'\n' => {
				push_token(
					Token::Newline {
						content: character,
						position: current_position,
					},
					&mut output,
					&mut current_word_start,
					&s,
				);
				current_position.next_line();
				current_word_start = current_position;
			}
			'\r' => {
				let word = &s
					.get(current_word_start.idx..current_position.idx)
					.map_or(String::new(), ToString::to_string);
				if !word.is_empty() {
					output.push(Token::Word {
						content: word.clone(),
						start: current_word_start,
						end: current_position,
					});
				}
				current_position.next_character();
				current_word_start = current_position;
			}
			'\t' => {
				push_token(
					Token::Indent {
						content: character,
						position: current_position,
					},
					&mut output,
					&mut current_word_start,
					&s,
				);
				current_position.next_character();
			}
			x if !x.is_alphanumeric() => {
				push_token(
					Token::Symbol {
						content: character,
						position: current_position,
					},
					&mut output,
					&mut current_word_start,
					&s,
				);
				current_position.next_character();
			}
			_ => current_position.next_character(),
		}
	}
	let word = &s[current_word_start.idx..current_position.idx];
	if !word.is_empty() {
		output.push(Token::Word {
			content: word.to_string(),
			start: current_word_start,
			end: TokenPos {
				idx: s.len() - 1,
				line: current_position.line,
				column: current_position.column,
			},
		});
	}
	output
}

fn push_token(
	token: Token,
	list: &mut Vec<Token>,
	current_word_start: &mut TokenPos,
	string: &str,
) {
	let current_word_end = token.get_end_position();
	let word = &string[current_word_start.idx..current_word_end.idx];
	if !word.is_empty() {
		list.push(Token::Word {
			content: word.to_string(),
			start: *current_word_start,
			end: current_word_end,
		});
	}
	list.push(token);
	let diff = current_word_end.idx + 1 - current_word_start.idx;
	current_word_start.idx += diff;
	current_word_start.column += diff;
}

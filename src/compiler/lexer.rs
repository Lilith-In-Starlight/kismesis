use std::fmt;

#[derive(Debug, Clone, PartialEq)]

/// The different tokens that can be in an input string
pub enum Token {
	Word(String),
	Space(char),
	Newline(char),
	Indent(char),
	Symbol(char),
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.get_as_string())
	}
}

impl Token {
	/// Pushes the content of the token into a string
	pub fn push_to_string(&self, to: &mut String) {
		match self {
			Self::Word(word) => to.push_str(word),
			Self::Space(c) | Self::Newline(c) | Self::Indent(c) | Self::Symbol(c) => to.push(*c),
		}
	}

	/// Returns the content of the token as a string
	pub fn get_as_string(&self) -> String {
		match self {
			Self::Word(word) => word.clone(),
			Self::Space(c) | Self::Newline(c) | Self::Indent(c) | Self::Symbol(c) => c.to_string(),
		}
	}
}

/// Converts a string into a `Vec<Token>`, ignoring `\r` characters.
pub fn tokenize(s: &str) -> Vec<Token> {
	let s: String = {
		let mut buffer = Vec::new();
		let mut buffer2 = Vec::new();
		let mut comments = false;
		for x in s.chars() {
			if !comments {
				buffer.push(x);
				if let [.., '<', '!', '-'] = buffer.as_slice() {
					buffer.pop();
					buffer.pop();
					buffer.pop();
					comments = true;
				}
			} else {
				buffer2.push(x);
				if let [.., '-', '>'] = buffer2.as_slice() {
					buffer2.clear();
					comments = false;
				}
			}
		}
		buffer.into_iter().collect()
	};

	let mut output: Vec<Token> = vec![];

	let mut current_word: usize = 0;
	for (idx, character) in s.char_indices() {
		match character {
			'\r' => {
				let word = &s
					.get(current_word..idx)
					.map(|x| x.to_string())
					.unwrap_or(String::new());
				if !word.is_empty() {
					output.push(Token::Word(word.clone()));
				}
				current_word = idx + 1;
				continue;
			}
			' ' => {
				push_token(
					Token::Space(character),
					&mut output,
					&mut current_word,
					idx,
					&s,
				);
			}
			'\n' => {
				push_token(
					Token::Newline(character),
					&mut output,
					&mut current_word,
					idx,
					&s,
				);
			}
			'\t' => {
				push_token(
					Token::Indent(character),
					&mut output,
					&mut current_word,
					idx,
					&s,
				);
			}
			x if !x.is_alphanumeric() => {
				push_token(
					Token::Symbol(character),
					&mut output,
					&mut current_word,
					idx,
					&s,
				);
			}
			_ => (),
		}
	}
	let word = &s[current_word..s.len()];
	if !word.is_empty() {
		output.push(Token::Word(word.to_string()))
	}
	output
}

fn push_token(
	token: Token,
	list: &mut Vec<Token>,
	current_word_start: &mut usize,
	current_word_end: usize,
	string: &str,
) {
	let word = &string[*current_word_start..current_word_end];
	if !word.is_empty() {
		list.push(Token::Word(word.to_string()))
	}
	list.push(token);
	*current_word_start = current_word_end + 1
}

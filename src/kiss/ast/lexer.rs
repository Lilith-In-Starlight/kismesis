#[derive(Debug)]
pub enum Token {
	OpenTag(char),
	CloseTag(char),
	Word(String),
	MacroName(String),
	Equals(char),
	Quote(char),
	OpenBracket(char),
	CloseBracket(char),
	Hashtag(char),
	Space(char),
	Newline(char),
	Indent(char),
	Bar(char),
}

impl Token {
	pub fn push_to_string(&self, to: &mut String) {
		match self {
			Self::Word(word) | Self::MacroName(word) => to.push_str(word),
			Self::OpenTag(c) | Self::CloseTag(c) | Self::Equals(c) | Self::Quote(c) | Self::OpenBracket(c) | Self::CloseBracket(c) | Self::Hashtag(c) | Self::Space(c) | Self::Newline(c) | Self::Indent(c) | Self::Bar(c) => to.push(*c),
		}
	}
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, &'static str> {
	let mut output: Vec<Token> = vec![];
	
	let mut current_word: usize = 0;
	for (idx, character) in s.char_indices() {
		match character {
			' ' => {
				push_token(Token::Space(character), &mut output, &mut current_word, idx, s);
			},
			'<' => {
				push_token(Token::OpenTag(character), &mut output, &mut current_word, idx, s);
			},
			'>' => {
				push_token(Token::CloseTag(character), &mut output, &mut current_word, idx, s);
			},
			'=' => {
				push_token(Token::Equals(character), &mut output, &mut current_word, idx, s);
			},
			'\'' | '"' => {
				push_token(Token::Quote(character), &mut output, &mut current_word, idx, s);
			},
			'[' => {
				push_token(Token::OpenBracket(character), &mut output, &mut current_word, idx, s);
			},
			']' => {
				push_token(Token::CloseBracket(character), &mut output, &mut current_word, idx, s);
			},
			'#' => {
				push_token(Token::Hashtag(character), &mut output, &mut current_word, idx, s);
			},
			'\n' => {
				push_token(Token::Newline(character), &mut output, &mut current_word, idx, s);
			},
			'\t' => {
				push_token(Token::Indent(character), &mut output, &mut current_word, idx, s);
			},
			'|' => {
				push_token(Token::Bar(character), &mut output, &mut current_word, idx, s);
			},
			_ => (),
		}
	}
	let word = &s[current_word..s.len()];
	if !word.is_empty() { output.push(Token::Word(word.to_string())) }
	Ok(output)
}

fn push_token(token: Token, list: &mut Vec<Token>, current_word_start: &mut usize, current_word_end: usize, string: &str) {
	let word = &string[*current_word_start..current_word_end];
	if !word.is_empty() { list.push(Token::Word(word.to_string())) }
	list.push(token);
	*current_word_start = current_word_end + 1
}
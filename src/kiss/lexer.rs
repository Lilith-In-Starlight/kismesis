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
	Escape(char),
}

impl Token {
	pub fn push_to_string(&self, to: &mut String) {
		match self {
			Self::Word(word) | Self::MacroName(word) => to.push_str(word),
			Self::OpenTag(c) | Self::CloseTag(c) | Self::Equals(c) | Self::Quote(c) | Self::OpenBracket(c) | Self::CloseBracket(c) | Self::Hashtag(c) | Self::Space(c) | Self::Newline(c) | Self::Indent(c) | Self::Bar(c) | Self::Escape(c) => to.push(*c),
		}
	}
	pub fn get_as_string(&self) -> String {
		match self {
			Self::Word(word) | Self::MacroName(word) => word.clone(),
			Self::OpenTag(c) | Self::CloseTag(c) | Self::Equals(c) | Self::Quote(c) | Self::OpenBracket(c) | Self::CloseBracket(c) | Self::Hashtag(c) | Self::Space(c) | Self::Newline(c) | Self::Indent(c) | Self::Bar(c) | Self::Escape(c) => (*c).into(),
		}
	}
}

pub fn tokenize(s: &str) -> Vec<Token> {
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
			'\\' => {
				push_token(Token::Escape(character), &mut output, &mut current_word, idx, s);
			},
			_ => (),
		}
	}
	let word = &s[current_word..s.len()];
	if !word.is_empty() { output.push(Token::Word(word.to_string())) }
	output
}

fn push_token(token: Token, list: &mut Vec<Token>, current_word_start: &mut usize, current_word_end: usize, string: &str) {
	let word = &string[*current_word_start..current_word_end];
	if !word.is_empty() { 
		if !word.ends_with('!') { list.push(Token::Word(word.to_string())) }
		else { list.push(Token::MacroName(word.to_string())) }
	}
	list.push(token);
	*current_word_start = current_word_end + 1
}

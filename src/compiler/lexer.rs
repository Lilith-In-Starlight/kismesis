#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Word(String),
    Space(char),
    Newline(char),
    Indent(char),
    Symbol(char),
}

impl Token {
    pub fn push_to_string(&self, to: &mut String) {
        match self {
            Self::Word(word) => to.push_str(word),
            Self::Space(c) | Self::Newline(c) | Self::Indent(c) | Self::Symbol(c) => to.push(*c),
        }
    }
    pub fn get_as_string(&self) -> String {
        match self {
            Self::Word(word) => word.clone(),
            Self::Space(c) | Self::Newline(c) | Self::Indent(c) | Self::Symbol(c) => c.to_string(),
        }
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut output: Vec<Token> = vec![];

    let mut current_word: usize = 0;
    for (idx, character) in s.char_indices() {
        match character {
            ' ' => {
                push_token(
                    Token::Space(character),
                    &mut output,
                    &mut current_word,
                    idx,
                    s,
                );
            }
            '\n' => {
                push_token(
                    Token::Newline(character),
                    &mut output,
                    &mut current_word,
                    idx,
                    s,
                );
            }
            '\t' => {
                push_token(
                    Token::Indent(character),
                    &mut output,
                    &mut current_word,
                    idx,
                    s,
                );
            }
            x if !x.is_alphanumeric() => {
                push_token(
                    Token::Symbol(character),
                    &mut output,
                    &mut current_word,
                    idx,
                    s,
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

use super::{lexer::Token, parser::{errors::ErrorState, state::TokenPos}};

pub struct DrawingInfo<'a> {
	pub(crate) line_number_length: usize,
	pub(crate) tokens: &'a [Token],
	pub(crate) lines: Vec<(usize, &'a[Token])>,
	pub(crate) line_offset: (usize, usize),
}

impl<'a> DrawingInfo<'a> {
	pub fn from(tokens: &'a [Token]) -> Self {
		let cidx: usize = 0;
		let lines: Vec<&[Token]> = tokens.split(|x| matches!(x, Token::Newline(_))).collect();
		let lines = {
			let mut out = Vec::new();
			let mut len: usize = 0;
			for x in lines {
				out.push((len, x));
				len = x.len()+1;
			}
			out
		};
		Self {
			line_number_length: 3,
			tokens,
			lines,
			line_offset: (0, 0),
		}
	}
}

fn draw_error<T>(line_number: usize, err: ErrorState<T>, info: &DrawingInfo) -> String {
	
}

fn draw_line<T>(line_number: usize, err: ErrorState<T>, info: &DrawingInfo) -> Option<String> {
	let mut output = draw_line_number(line_number, info);
	let mut error_line = turn_to_chars(output.clone(), ' ');
	if let Some(line) = info.lines.get(line_number) {
		for (token_idx, token) in line.1.iter().enumerate() {
			output.push_str(&token.get_as_string());
			let token_pos = TokenPos {
				idx: line.0 + token_idx,
				line: line_number,
				column: token_idx,
			};
			let char = if token_pos.is_in(&err.text_position) {
				' '
			} else {
				'^'
			};
			error_line.push_str(&turn_to_chars(token.get_as_string(), char));
		}
	} else {
		return None
	}

	error_line = error_line.trim_end().to_string();
	if !error_line.is_empty() {
		if err.text_position.is_one_line() {
			error_line.push_str("GET THE ERROR MESSAGE");
		}
		Some(format!("{}\n{}", output, error_line))
	} else {
		Some(output)
	}
}

fn turn_to_chars(string: String, chr: char) -> String {
	string.chars().map(|x| match x {
		'\t' => chr.to_string().repeat(4),
		_ => chr.to_string(),
	}).collect()
}

fn as_string(t: &[Token]) -> String {
	let mut output = String::new();
	for x in t.iter() {
		x.push_to_string(&mut output);
	}
	output
}

fn draw_line_number(line: usize, info: &DrawingInfo) -> String {
	let mut output = line.to_string();
	while output.len() < info.line_number_length + 1 {
		output.push(' ');
	}
	output.push_str("│ ");
	output
}


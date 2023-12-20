use super::{lexer::Token, parser::{errors::{ErrorState, ErrorKind}, state::TokenPos}, html::{ScopedError, Inside}};
use colored::*;

pub struct DrawingInfo<'a> {
	pub(crate) line_number_length: usize,
	pub(crate) tokens: &'a [Token],
	pub(crate) lines: Vec<(usize, &'a[Token])>,
	pub(crate) line_offset: (usize, usize),
}

impl<'a> DrawingInfo<'a> {
	pub fn from(tokens: &'a [Token]) -> Self {
		let lines: Vec<&[Token]> = tokens.split(|x| matches!(x, Token::Newline(_))).collect();
		let lines = {
			let mut out = Vec::new();
			let mut len: usize = 0;
			for x in lines {
				out.push((len, x));
				len += x.len()+1;
			}
			out
		};
		Self {
			line_number_length: 3,
			tokens,
			lines,
			line_offset: (2, 2),
		}
	}
}

pub fn draw_error<T: ErrorKind>(err: &ErrorState<T>, info: &DrawingInfo) -> String {
	let minimum_line = {
		let x = err.text_position.get_start_line();
		if x < info.line_offset.0 { 0 }
		else { x - info.line_offset.0 }
	};
	let maximum_line = {
		let x = err.text_position.get_end_line();
		if x > info.lines.len() - info.line_offset.1 { info.lines.len() }
		else { x + info.line_offset.1 }
	};

	let mut output = String::new();

	output.push_str(&" ERROR ".black().on_red().to_string());
	output.push('\n');

	
	for line_number in minimum_line..=maximum_line {
		if let Some(string) = draw_line(line_number, err, info) {
			output.push_str(&string);
			output.push('\n');
		}
	}

	if !err.text_position.is_one_line() {
		output.push_str(&format!("\n{}", err.error.get_text()));
	}

	output
}

fn draw_line<T: ErrorKind>(line_number: usize, err: &ErrorState<T>, info: &DrawingInfo) -> Option<String> {
	let mut output = draw_line_number(line_number, info).bright_black().to_string();
	let mut error_line = turn_to_chars(draw_line_number(line_number, info), ' ');
	if let Some(line) = info.lines.get(line_number) {
		for (token_idx, token) in line.1.iter().enumerate() {
			output.push_str(&token.get_as_string());
			let token_pos = TokenPos::new_at(line.0 + token_idx, line_number, token_idx);
			let char = if token_pos.is_in(&err.text_position) {
				'^'
			} else {
				' '
			};
			error_line.push_str(&turn_to_chars(token.get_as_string(), char));
		}
	} else {
		return None
	}

	error_line = error_line.trim_end().to_string();
	if !error_line.is_empty() {
		if err.text_position.is_one_line() {
			error_line.push_str(&format!(" {}", err.error.get_text()));
		} else {
			error_line.push_str(" Error happened here");
		}
		Some(format!("{}\n{}", output, error_line.yellow()))
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

pub fn draw_scoped_error<T: ErrorKind>(err: &ScopedError<T>) -> String {
	draw_error(&err.error, &DrawingInfo::from(err.scope))
}

pub fn draw_packed_error(err: &Inside) -> String {
	let mut output = String::new();
	match err {
		Inside::In(x) => {
			for err in x {
				output.push_str(&format!("{}\n", draw_scoped_error(err)))
			}
		}
		Inside::Err(x) => output.push_str(&format!("{}\n", draw_scoped_error(x))),
	}

	output
}

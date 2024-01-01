use std::fmt::Debug;

use crate::kismesis::{KisID, Kismesis, FileRef};

use super::{
	errors::{ErrorKind, ErrorState},
	html::ScopedError,
	lexer::Token,
	parser::{state::TokenPos, errors::Hint},
};
use colored::*;

pub struct DrawingInfo<'a> {
	pub(crate) line_number_length: usize,
	pub(crate) scope: &'a FileRef,
	pub(crate) lines: Vec<(usize, &'a [Token])>,
	pub(crate) line_offset: (usize, usize),
}

impl<'a> DrawingInfo<'a> {
	pub fn from(scope: KisID, engine :&'a Kismesis) -> Self {
		let scope = engine.get_file(scope).unwrap();
		let lines: Vec<&[Token]> = scope
			.tokens
			.split_inclusive(|x| matches!(x, Token::Newline(_)))
			.collect();
		let lines = {
			let mut out = Vec::new();
			let mut len: usize = 0;
			for x in lines {
				out.push((len, x));
				len += x.len();
			}
			out
		};
		Self {
			line_number_length: 3,
			scope,
			lines,
			line_offset: (2, 2),
		}
	}
}

pub fn draw_error<T: ErrorKind + Debug>(err: &ErrorState<T>, info: &DrawingInfo, engine: &Kismesis) -> String {
	let minimum_line = {
		let x = err.text_position.get_start_line();
		if x < info.line_offset.0 {
			0
		} else {
			x - info.line_offset.0
		}
	};
	let maximum_line = {
		let x = err.text_position.get_end_line();
		if x > info.lines.len() - info.line_offset.1 {
			info.lines.len()
		} else {
			x + info.line_offset.1
		}
	};

	let mut output = String::new();

	output.push_str(&" ERROR ".black().on_red().to_string());
	output.push_str(&" in `".black().on_red().to_string());
	match info.scope.path {
		Some(ref path) => {
			output.push_str(
				&path
					.to_string_lossy()
					.to_string()
					.black()
					.on_red()
					.to_string(),
			);
			output.push_str(&"` ".black().on_red().to_string());
		}
		None => output.push_str(&"input` ".black().on_red().to_string()),
	}
	output.push('\n');

	for line_number in minimum_line..=maximum_line {
		if let Some(string) = draw_line(line_number, err, info) {
			output.push_str(&string);
			output.push('\n');
		}
	}

	for x in err.hints.iter() {
		match x {
			Hint::Stateful(x) => output.push_str(&draw_hint(&x.error, &DrawingInfo::from(x.scope, engine))),
			Hint::Stateless(x) => {
				output.push_str(&format!("Hint: {}", x.get_text()))
			}
		}
	}

	if !err.text_position.is_one_line() {
		output.push_str(&format!("\n{}", err.error.get_text()));
	}

	output
}

pub fn draw_hint<T: ErrorKind + Debug>(err: &ErrorState<T>, info: &DrawingInfo) -> String {
	let minimum_line = {
		let x = err.text_position.get_start_line();
		if x < info.line_offset.0 {
			0
		} else {
			x - info.line_offset.0
		}
	};
	let maximum_line = {
		let x = err.text_position.get_end_line();
		if x > info.lines.len() - info.line_offset.1 {
			info.lines.len()
		} else {
			x + info.line_offset.1
		}
	};

	let mut output = String::new();

	output.push_str(&" HINT ".black().on_yellow().to_string());
	output.push_str(&" in `".black().on_yellow().to_string());
	match info.scope.path {
		Some(ref path) => {
			output.push_str(
				&path
					.to_string_lossy()
					.to_string()
					.black()
					.on_yellow()
					.to_string(),
			);
			output.push_str(&"` ".black().on_yellow().to_string());
		}
		None => output.push_str(&"input` ".black().on_red().to_string()),
	}
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

fn draw_line<T: ErrorKind>(
	line_number: usize,
	err: &ErrorState<T>,
	info: &DrawingInfo,
) -> Option<String> {
	let mut output = draw_line_number(line_number, info)
		.bright_black()
		.to_string();
	let mut error_line = turn_to_chars(draw_line_number(line_number, info), ' ');
	if let Some(line) = info.lines.get(line_number) {
		for (token_idx, token) in line.1.iter().enumerate() {
			let token_pos = TokenPos::new_at(line.0 + token_idx, line_number, token_idx);
			let tkstr = match token {
				Token::Newline(_) if token_pos.is_in(&err.text_position) => "~".to_string(),
				Token::Newline(_) => "".to_string(),
				Token::Indent(_) => " ".repeat(4),
				x => x.get_as_string(),
			};
			output.push_str(&tkstr);
			let char = if token_pos.is_in(&err.text_position) {
				'^'
			} else {
				' '
			};
			error_line.push_str(&turn_to_chars(tkstr, char));
		}
	} else {
		return None;
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
	string
		.chars()
		.map(|x| match x {
			'\t' => chr.to_string().repeat(4),
			_ => chr.to_string(),
		})
		.collect()
}

fn draw_line_number(line: usize, info: &DrawingInfo) -> String {
	let mut output = (line + 1).to_string();
	while output.len() < info.line_number_length + 1 {
		output.push(' ');
	}
	output.push_str("│ ");
	output
}

pub fn draw_scoped_error<T: ErrorKind + Debug>(err: &ScopedError<T>, engine: &Kismesis) -> String {
	draw_error(&err.error, &DrawingInfo::from(err.scope, engine), engine)
}


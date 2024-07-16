//! Print Kismesis' aesthetically pleasing and information-ful errors.

use std::fmt::Debug;
use std::fmt::Write;

use crate::KisTokenId;
use crate::{FileRef, Kismesis};

use super::{
	errors::{ErrorKind, ErrorState, StatelessError},
	html::ScopedError,
	lexer::Token,
	parser::{errors::Hint, state::TokenPos},
};
use colored::Colorize;

#[derive(Clone, Copy)]
pub enum ReportKind {
	Error,
	Hint,
	Fatal,
	Help,
}

impl ReportKind {
	fn draw_stateless_band(self) -> String {
		match self {
			Self::Error => " ERROR ".black().on_red().to_string(),
			Self::Hint => " HINT ".black().on_yellow().to_string(),
			Self::Fatal => " FATAL ERROR ".black().on_red().to_string(),
			Self::Help => " HELP ".black().on_red().to_string(),
		}
	}

	fn draw_scoped_band(self, scope: &FileRef) -> String {
		let mut output = String::new();
		match self {
			Self::Error => {
				output.push_str(&" ERROR".black().on_red().to_string());
				output.push_str(&" in `".black().on_red().to_string());
				match scope.path {
					Some(ref path) => {
						output.push_str(
							&path.to_string_lossy().as_ref().black().on_red().to_string(),
						);
						output.push_str(&"` ".black().on_red().to_string());
					}
					None => output.push_str(&"input` ".black().on_red().to_string()),
				}
			}
			Self::Hint => {
				output.push_str(&" HINT".black().on_yellow().to_string());
				output.push_str(&" for `".black().on_yellow().to_string());
				match scope.path {
					Some(ref path) => {
						output.push_str(
							&path
								.to_string_lossy()
								.as_ref()
								.black()
								.on_yellow()
								.to_string(),
						);
						output.push_str(&"` ".black().on_yellow().to_string());
					}
					None => output.push_str(&"input` ".black().on_yellow().to_string()),
				}
			}
			Self::Fatal => {
				output.push_str(&" FATAL ERROR".black().on_red().to_string());
				output.push_str(&" in `".black().on_red().to_string());
				match scope.path {
					Some(ref path) => {
						output.push_str(
							&path.to_string_lossy().as_ref().black().on_red().to_string(),
						);
						output.push_str(&"` ".black().on_red().to_string());
					}
					None => output.push_str(&"input` ".black().on_red().to_string()),
				}
			}
			Self::Help => {
				output.push_str(&" HELP".black().on_green().to_string());
				output.push_str(&" for `".black().on_green().to_string());
				match scope.path {
					Some(ref path) => {
						output.push_str(
							&path
								.to_string_lossy()
								.as_ref()
								.black()
								.on_green()
								.to_string(),
						);
						output.push_str(&"` ".black().on_green().to_string());
					}
					None => output.push_str(&"input` ".black().on_green().to_string()),
				}
			}
		};
		output
	}
}

impl<T> From<(KisTokenId, ErrorState<T>)> for ScopedError<T> {
	fn from(value: (KisTokenId, ErrorState<T>)) -> Self {
		Self {
			error: value.1,
			scope: value.0,
		}
	}
}
pub trait Report {
	fn create_report(
		&self,
		kind: ReportKind,
		info: &DrawingInfo,
		engine: &Kismesis,
		depth: usize,
	) -> String;
	fn report(&self, kind: ReportKind, info: &DrawingInfo, engine: &Kismesis, depth: usize) {
		eprintln!("{}", self.create_report(kind, info, engine, depth));
	}
}

/// Information related to how the error reporter will report an error
pub struct DrawingInfo {
	pub(crate) line_number_length: usize,
	pub(crate) line_offset: (usize, usize),
}

/// Errors that may occurr during error reporting
#[derive(Debug)]
enum ReportingError {
	InvalidKismesisID,
}

impl ErrorKind for ReportingError {
	fn get_text(&self) -> String {
		match self {
			Self::InvalidKismesisID => "Tried to report an error ocurring on a file with an invalid Kismesis ID.\nPlease contact the developer of the engine you're using.".into(),
		}
	}
}

impl Default for DrawingInfo {
	fn default() -> Self {
		Self {
			line_number_length: 3,
			line_offset: (2, 2),
		}
	}
}

impl<T> Report for ScopedError<T>
where
	T: ErrorKind + Debug,
{
	fn create_report(
		&self,
		kind: ReportKind,
		info: &DrawingInfo,
		engine: &Kismesis,
		depth: usize,
	) -> String {
		let Some(scope) = engine.get_file(self.scope) else {
			let err = ReportingError::InvalidKismesisID.stateless();
			return err.create_report(ReportKind::Fatal, info, engine, depth);
		};
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
		let minimum_line = {
			let x = self.error.text_position.get_start_line();
			if x < info.line_offset.0 {
				0
			} else {
				x - info.line_offset.0
			}
		};
		let maximum_line = {
			let x = self.error.text_position.get_end_line();
			if x + info.line_offset.1 > lines.len() {
				lines.len()
			} else {
				x + info.line_offset.1
			}
		};

		let mut output = String::new();

		output.push_str(&kind.draw_scoped_band(scope));

		output.push('\n');

		for line_number in minimum_line..=maximum_line {
			if let Some(string) = draw_line(&lines, line_number, &self.error, info) {
				output.push_str(&string);
				output.push('\n');
			}
		}

		output.push('\n');

		for x in &self.error.hints {
			let hint = match x {
				Hint::Stateful(x) => x.create_report(ReportKind::Hint, info, engine, depth + 1),
				Hint::Stateless(x) => x.create_report(ReportKind::Hint, info, engine, depth + 1),
			};
			output.push_str(&hint);
		}

		if !self.error.text_position.is_one_line() {
			output.push_str(&format!("\n{}", self.error.error.get_text()));
		}

		output.split('\n').fold(String::new(), |mut output, y| {
			let _ = writeln!(output, "{}{y}", " ".repeat(depth));
			output
		})
	}
}

impl<T> Report for StatelessError<T>
where
	T: ErrorKind + Debug,
{
	fn create_report(
		&self,
		kind: ReportKind,
		info: &DrawingInfo,
		engine: &Kismesis,
		depth: usize,
	) -> String {
		let mut output = String::new();

		output.push_str(&kind.draw_stateless_band());
		output.push('\n');

		output.push_str(&self.error.get_text());

		output.push('\n');

		for x in &self.hints {
			let hint = match x {
				Hint::Stateful(x) => x.create_report(ReportKind::Hint, info, engine, depth + 1),
				Hint::Stateless(x) => x.create_report(kind, info, engine, depth + 1),
			};
			output.push_str(&hint);
		}

		output.split('\n').fold(String::new(), |mut output, y| {
			let _ = writeln!(output, "{}{y}", " ".repeat(depth * 2));
			output
		})
	}
}

/// Returns a line. It will contain pointers to the provided error if the provided error is in the rendered line
fn draw_line<T: ErrorKind>(
	lines: &[(usize, &[Token])],
	line_number: usize,
	err: &ErrorState<T>,
	info: &DrawingInfo,
) -> Option<String> {
	let mut output = draw_line_number(line_number, info).white().to_string();
	let mut error_line = turn_to_chars(&draw_line_number(line_number, info), ' ');
	let termsize = termsize::get().map_or(40, |size| size.cols) as usize;
	let termsize = if termsize >= err.error.get_text().len() {
		std::cmp::min(termsize, termsize - err.error.get_text().len())
	} else {
		termsize
	};

	let initial_spaces = error_line.clone();
	let mut error_spaces = String::new();

	if let Some(line) = lines.get(line_number) {
		let mut char_idx: usize = 0;
		for (token_idx, token) in line.1.iter().enumerate() {
			let token_pos = TokenPos::new_at(line.0 + token_idx, line_number, token_idx);
			let tkstr = match token {
				Token::Newline(_) if token_pos.is_in(&err.text_position) => "~".to_string(),
				Token::Newline(_) => String::new(),
				Token::Indent(_) => " ".repeat(4),
				x => x.get_as_string(),
			};
			char_idx += tkstr.len();
			if char_idx + tkstr.len() >= termsize && token_idx != 0 {
				output.push('\n');

				if error_line.chars().any(|x| !x.is_whitespace()) {
					output.push_str(error_line.yellow().to_string().trim_end());
					output.push('\n');
				}

				output.push_str(&initial_spaces);
				error_line.clone_from(&initial_spaces);
				char_idx = tkstr.len();
			}
			output.push_str(&tkstr);
			let char = if token_pos.is_in(&err.text_position) {
				if error_spaces.is_empty() {
					error_spaces.push_str(&error_line);
				}
				'^'
			} else {
				' '
			};
			error_line.push_str(&turn_to_chars(&tkstr, char));
			if token_pos.is_at_an_end(&err.text_position) {
				if err.text_position.is_one_line() {
					let text = err
						.error
						.get_text()
						.replace('\n', &format!("\n  {}", &error_spaces));
					error_line.push_str(&format!(" {text}"));
				} else {
					error_line.push_str(" Error happened here");
				}
			}
		}
	} else {
		return None;
	}

	error_line = error_line.trim_end().to_string();
	if error_line.is_empty() {
		Some(output)
	} else {
		Some(format!("{}\n{}", output, error_line.yellow()))
	}
}

/// Turns a string of characters into a repeated sequence of a given character.
/// Tabs are turned into four instances of the character
fn turn_to_chars(string: &str, chr: char) -> String {
	string
		.chars()
		.map(|x| match x {
			'\t' => chr.to_string().repeat(4),
			_ => chr.to_string(),
		})
		.collect()
}

/// Draws the line enumerator of the given line number, using the given drawinginfo to figure out
/// the desired width.
fn draw_line_number(line: usize, info: &DrawingInfo) -> String {
	let mut output = (line + 1).to_string();
	while output.len() < info.line_number_length + 1 {
		output.push(' ');
	}
	output.push_str("│ ");
	output
}

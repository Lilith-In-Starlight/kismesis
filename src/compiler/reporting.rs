use core::fmt;

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

impl Pipe {
	fn is_similar_to(&self, other: Option<&GridObject>) -> bool {
		matches!(other, Some(GridObject::Pipe(Pipe { color, ..})) if color == &self.color)
	}
	fn is_forced_up(&self) -> bool {
		matches!(self.forced_direction, Some(Direction::Up))
	}
	fn is_forced_down(&self) -> bool {
		matches!(self.forced_direction, Some(Direction::Down))
	}
	fn is_forced_left(&self) -> bool {
		matches!(self.forced_direction, Some(Direction::Left))
	}
	fn is_forced_right(&self) -> bool {
		matches!(self.forced_direction, Some(Direction::Right))
	}
}

#[derive(Debug)]
pub struct CharGrid(Vec<Vec<GridObject>>);

impl fmt::Display for CharGrid {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut output = String::new();
		for (line_idx, line) in self.0.iter().enumerate() {
			for (column, char) in line.iter().enumerate() {
				match char {
					GridObject::Char(char) => output.push(*char),
					GridObject::Pipe(pipe) => {
						let up_dir = pipe.is_forced_up() || pipe.is_similar_to(self.get(column, line_idx - 1));
						let down_dir = pipe.is_forced_down() || pipe.is_similar_to(self.get(column, line_idx + 1));
						let left_dir = pipe.is_forced_left() || pipe.is_similar_to(self.get(column - 1, line_idx));
						let right_dir = pipe.is_forced_right() || pipe.is_similar_to(self.get(column + 1, line_idx));

						let char = match (up_dir, down_dir, left_dir, right_dir) {
							(false, false, false, false) => 'x',
							(true, false, false, false) => '╵',
							(false, true, false, false) => '╷',
							(true, true, false, false) => '│',
							(false, false, true, false) => '╴',
							(true, false, true, false) => '╯',
							(false, true, true, false) => '╮',
							(true, true, true, false) => '┤',
							(false, false, false, true) => '╶',
							(true, false, false, true) => '╰',
							(false, true, false, true) => '╭',
							(true, true, false, true) => '├',
							(false, false, true, true) => '━',
							(true, false, true, true) => '┴',
							(false, true, true, true) => '┬',
							(true, true, true, true) => '┼',
						};
						output.push(char);
					},
				}
			}
			output.push('\n');
		}
		write!(f, "{}", output)
	}
}

#[derive(Debug)]
enum Direction {
	Up,
	Down,
	Left,
	Right,
}

#[derive(Debug)]
struct Pipe {
	color: usize,
	forced_direction: Option<Direction>
}

#[derive(Debug)]
enum GridObject {
	Char(char),
	Pipe(Pipe)
}

impl CharGrid {
	fn set(&mut self, x: usize, y: usize, c: GridObject) {
		while self.0.len() <= y {
			self.0.push(Vec::new());
		}
		if let Some(line) = self.0.get_mut(y) {
			while line.len() <= x {
				line.push(GridObject::Char(' '));
			}
			if let Some(character) = line.get_mut(x) {
				*character = c;
			}
		}
	}
	
	fn get(&self, x: usize, y: usize) -> Option<&GridObject> {
		if let Some(line) = self.0.get(y) {
			if let Some(character) = line.get(x) {
				return Some(character)
			}
		}
		return None
	}

	fn draw_token_line<T>(&mut self, line: usize, err: &ErrorState<T>, starty: usize, info: &DrawingInfo) {
		let Some((init_pos, token_line)) = info.lines.get(line) else { return };
		let mut idx: usize = 0;
		self.draw_string((0, 0), &draw_line_number(line, info));
		let padding = info.line_number_length + 3;
		for (token_idx, token) in token_line.iter().enumerate() {
			for (char_in_token, char) in token.get_as_string().chars().enumerate() {
				self.set(idx + padding, starty, GridObject::Char(char));

				let tokpos = TokenPos::new_at(init_pos + token_idx, starty, idx);

				let forced_direction = {
					if char_in_token == 0 && tokpos.is_at_a_start(&err.text_position) {
						Some(Direction::Up)
					} else if char_in_token == token.get_as_string().len() - 1 && tokpos.is_at_an_end(&err.text_position) {
						Some(Direction::Up)
					} else {
						None
					}
				};

				if tokpos.is_in(&err.text_position) {
					self.set(idx + padding, starty+1, GridObject::Pipe(Pipe {
						color: 0,
						forced_direction,
					}));
				}
				if char_in_token == 0 && tokpos.is_at_a_start(&err.text_position) {
					self.draw_connection((idx + 1 + padding, starty + 2), (20 + padding, starty + 3));
					self.draw_string((20 + padding, starty + 3), "Nothing values cannot be used as content\nThey exist solely as logical elements")
				}
				idx += 1;
			}
		}
	}

	fn draw_connection(&mut self, start: (usize, usize), end: (usize, usize)) {
		let mut x = start.0;
		let mut y = start.1;

		while y < end.1 {
			self.set(x, y, GridObject::Pipe(Pipe { color: 0, forced_direction: None }));
			y += 1;
		}
		self.set(x, y, GridObject::Pipe(Pipe { color: 0, forced_direction: None }));
		while x < end.0 {
			self.set(x, y, GridObject::Pipe(Pipe { color: 0, forced_direction: None }));
			x += 1;
		}
	}
	
	fn draw_string(&mut self, start: (usize, usize), string: &str) {
		let mut x = start.0;
		let mut y = start.1;
		for chr in string.chars() {
			match chr {
				'\n' => {
					x = start.0;
					y += 1;
					continue
				}
				_ => self.set(x, y, GridObject::Char(chr)),
			}
			x += 1;
		}
	}
}

pub fn get_error_graph<T>(err: &ErrorState<T>, info: &DrawingInfo) -> CharGrid {
	let minimum_line = {
		let x = err.text_position.get_start_line();
		if x < info.line_offset.0 { x }
		else { x - info.line_offset.0 }
	};
	let maximum_line = {
		let x = err.text_position.get_end_line();
		if x > info.lines.len() - info.line_offset.1 { info.lines.len() }
		else { x + info.line_offset.1 }
	};

	let mut grid = CharGrid(Vec::new());
	
	for line_num in minimum_line..=maximum_line {
		grid.draw_token_line(line_num, err, line_num-minimum_line, info)
	}

	grid
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


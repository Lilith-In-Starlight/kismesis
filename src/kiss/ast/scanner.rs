pub struct Scanner<T> {
	cursor: Option<usize>,
	pub items: Vec<T>,
}


impl<T:std::cmp::PartialEq> Scanner<T> where T:Clone {
	pub fn new(string: Vec<T>) -> Self {
		Self {
			cursor: None,
			items: string,
		}
	}

	pub fn advance(&mut self) -> Result<(), &'static str> {
		match self.cursor {
			None => return Err("Attempted to advance uninitialized scanner"),
			Some(x) => {
				self.cursor = Some(x + 1);
				return Ok(())
			},
		}
	}

	pub fn get_char(&mut self) -> Option<&T> {
		match self.cursor {
			Some(cursor) => {
				self.cursor = Some(cursor + 1);
				match self.items.get(cursor) {
					None => {
						self.cursor = None;
						return None
					}
					Some(x) => {
						return Some(x)
					}
				}
			},
			None => {
				self.cursor = Some(0);
				return self.items.first()
			}
		}
	}

	pub fn peek_current(&mut self) -> Result<&T, &'static str> {
		match self.cursor {
			Some(x) => {
				match self.items.get(x) {
					Some(x) => return Ok(x),
					None => Err("Cursor was at impossible position")
				}
			},
			None => {
				self.cursor = Some(0);
				return Ok(&self.items[0])
			}
		}
	}

	pub fn skip_over (&mut self, target: &T) -> Result<Vec<T>, &'static str> {
		let starting_point = match self.cursor {
			Some(x) => x,
			None => return Err("Called skip_over on uninitialized scanner")
		};
		loop {
			match self.peek_current() {
				Err(x) => return Ok(self.items[starting_point..self.items.len() - 1].to_vec()),
				Ok(character) => {
					if character == target { 
						let ending_point = match self.cursor {
							Some(x) => x,
							None => self.items.len() - 1,
						};
						return Ok(self.items[starting_point..ending_point].to_vec())
					}
					else { 
						if let Err(x) = self.advance() {
							return Err(x)
						}
					}
				}
			}
		}
	}

	pub fn get_cursor(&self) -> Option<usize> { self.cursor }
}
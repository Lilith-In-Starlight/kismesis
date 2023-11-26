use std::io::IsTerminal;
use std::env;

pub enum Colors {
	Red,
}

pub trait Colorable {
	fn red(self) -> String where Self: Sized {
		self.colored(Colors::Red)
	}
	fn colored(self, c: Colors) -> String where Self: Sized;
}


impl Colorable for String {
	fn colored(self, c: Colors) -> String {
		println!("{}", can_color());
		String::new()
	}
}

pub fn can_color() -> bool {
	match env::var("NO_COLOR") {
		Ok(x) => {
			println!("{}", x);
			true
		},
		Err(x) => {
			println!("{}", x);
			false
		},
	}
}

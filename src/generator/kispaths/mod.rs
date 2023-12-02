use std::path::Path;


pub fn templates() -> &'static Path {
	if cfg!(debug_assertions) {
		&Path::new("test/templates")
	} else {
		&Path::new("templates")
	}
}

pub fn main_template() -> &'static Path {
	if cfg!(debug_assertions) {
		&Path::new("test/templates/main.kiss")
	} else {
		&Path::new("templates/main.kiss")
	}
}

pub fn input() -> &'static Path {
	if cfg!(debug_assertions) {
		&Path::new("test/input")
	} else {
		&Path::new("input")
	}
}

pub fn output() -> &'static Path {
	if cfg!(debug_assertions) {
		&Path::new("test/output")
	} else {
		&Path::new("output")
	}
}


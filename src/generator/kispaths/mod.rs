use std::path::Path;


pub fn templates() -> &'static Path {
	&Path::new("templates")
}

pub fn main_template() -> &'static Path {
	&Path::new("templates/main.kiss")
}

pub fn input() -> &'static Path {
	&Path::new("input")
}

pub fn output() -> &'static Path {
	&Path::new("output")
}


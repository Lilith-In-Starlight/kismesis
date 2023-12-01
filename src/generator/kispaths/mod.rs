use std::path::Path;


pub fn templates() -> &'static Path {
	&Path::new("test/templates")
}

pub fn input() -> &'static Path {
	&Path::new("test/input")
}

pub fn output() -> &'static Path {
	&Path::new("test/output")
}

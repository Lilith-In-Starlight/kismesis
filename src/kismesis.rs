use std::{path::{Path, PathBuf}, io, fs};

use crate::compiler::{parser::{types::{ParsedFile, Scoped}, errors::Err, self}, html::{ScopedError, CompilerError}, lexer};

enum Error<'a> {
	IOError(io::Error, PathBuf),
	CompileError(ScopedError<'a, CompilerError>),
	ParsingError((Err, PathBuf, &[Token])),
}

struct Kismesis<'a> {
	templates: Vec<ParsedFile<'a>>,
	inputs: Vec<ParsedFile<'a>>,
	errors: Vec<Error<'a>>,
}

impl<'a> Kismesis<'a> {
	fn register_template(&mut self, path: PathBuf) -> Result<(), Error> {
		let text = match fs::read_to_string(&path) {
			Err(x) => {
				return Err(Error::IOError(x, path));
			},
			Ok(x) => x,
		};

		let tokens = lexer::tokenize(&text);

		match parser::file(tokens, Some(path.clone())) {
			Ok(x) => self.templates.push(x),
			Err(x) => return Err(Error::ParsingError((x.0, (&x.1, Some(path))))),
		}

		Ok(())
	}
}

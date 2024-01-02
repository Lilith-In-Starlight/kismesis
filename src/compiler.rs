use std::{
	fs::{self, File},
	io::{self, Write},
	path::{Path, PathBuf},
};

use crate::kismesis::{Kismesis, KismesisError, KisID};

use self::{options::Settings, reporting::{DrawingInfo, draw_error}, parser::errors::Err};

mod errors;
pub(crate) mod html;
pub(crate) mod lexer;
pub(crate) mod options;
pub(crate) mod parser;
mod reporting;

pub enum Error {
	IOError(io::Error, PathBuf),
	NoMainTemplate,
	OutputNotInOutputFolder(PathBuf),
	TemplateInOutputFolder(PathBuf),
	ParseError(Err, KisID),
}

pub fn compile_project() {
	let mut errors = Vec::new();
	let mut engine =  Kismesis::new();

	let main_template_path = PathBuf::from("templates/main.ks");
	let template_paths = recursive_crawl(&PathBuf::from("templates")).0;

	for path in template_paths {
		match engine.register_file(path) {
			Ok(x) => {engine.register_template(x);},
			Err(x) => errors.push(x.into()),
		}
	}

	if !errors.is_empty() {
		report_errors(errors, &engine);
		return;
	}

	let Some(main_template_id) = engine.verify_template_id(main_template_path) else {
		errors.push(Error::NoMainTemplate);
		report_errors(errors, &engine);
		return;
	};
	let input_paths = recursive_crawl(&PathBuf::from("input")).0;
	let mut input_files = Vec::new();

	for path in input_paths {
		match engine.register_file(path) {
			Ok(mut x) => {
				x.template = Some(main_template_id.clone());
				input_files.push(x);
			}
			Err(x) => errors.push(x.into()),
		}
	}

	if !errors.is_empty() {
		report_errors(errors, &engine);
		return;
	}

	let settings = Settings::new();
	for file in input_files.iter() {
		match html::generate_html(&file, vec![], &settings, &engine) {
			Ok(x) => {
				let output_path = PathBuf::from("output");
				if let Some(path) = &engine.get_file(file.file_id).unwrap().path {
					let mut output_path =
						output_path.join::<PathBuf>(path.iter().skip(1).collect());
					output_path.set_extension("html");
					match output_path.parent() {
						Some(parent) => match std::fs::create_dir_all(parent) {
							Ok(_) => (),
							Err(x) => {
								errors.push(Error::IOError(x, output_path.clone()));
								continue;
							}
						},
						None => errors.push(Error::OutputNotInOutputFolder(output_path.clone())),
					};
					let mut file = match File::create(&output_path) {
						Ok(x) => x,
						Err(x) => {
							errors.push(Error::IOError(x, output_path.clone()));
							continue;
						}
					};
					let file_text = match x.to_string() {
						Ok(x) => x,
						Err(_) => {
							errors.push(Error::TemplateInOutputFolder(path.clone()));
							continue;
						}
					};
					match write!(file, "{}", file_text) {
						Ok(x) => x,
						Err(x) => {
							errors.push(Error::IOError(x, output_path.clone()));
							continue;
						}
					};
				}
			}
			Err(errors) => {
				for error in errors {
					eprintln!("{}", reporting::draw_scoped_error(&error, &engine));
				}
			}
		}
	}
}

pub fn recursive_crawl(path: &Path) -> (Vec<PathBuf>, Vec<io::Error>) {
	let mut errors = Vec::new();
	let mut paths = Vec::new();
	let entries = match fs::read_dir(path) {
		Ok(x) => x,
		Err(x) => return (vec![], vec![x]),
	};
	for entry in entries {
		let entry = match entry {
			Ok(x) => x,
			Err(x) => {
				errors.push(x);
				continue;
			}
		};
		let path = entry.path();
		if path.is_dir() {
			let (mut a, mut b) = recursive_crawl(&path);
			errors.append(&mut b);
			paths.append(&mut a)
		} else if let Some(ext) = path.extension() {
			if ext.to_string_lossy().to_string() == "ks" {
				paths.push(path)
			}
		}
	}

	(paths, errors)
}

impl From<KismesisError> for Error {
	fn from(value: KismesisError) -> Self {
        match value {
			KismesisError::IOError(x, y) => Error::IOError(x, y),
			KismesisError::ParseError(x, y) => Error::ParseError(x, y),
		}
    }
}

pub fn report_errors(errors: Vec<Error>, engine: &Kismesis) {
	for error in errors {
		match error {
            Error::IOError(error, path) => eprintln!("Error reading `{}`: {}", path.to_string_lossy(), error),
            Error::NoMainTemplate => eprintln!("Coudln't compile project because it doesn't have a template in templates/main.ks"),
            Error::OutputNotInOutputFolder(path) => eprintln!("Tried to output {} to a location outside the project's output folder.\n\nThis is meant to be impossible, please contact the developer at https://ampersandia.net/", path.to_string_lossy()),
            Error::TemplateInOutputFolder(path) => eprintln!("{} is a template, but it is in the input folder", path.to_string_lossy()),
            Error::ParseError(error, id) => eprintln!("{}", draw_error(&error.unpack(), &DrawingInfo::from(id, engine, false), engine)),
        }
	}
}

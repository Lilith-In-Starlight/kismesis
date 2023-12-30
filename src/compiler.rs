use std::{
	collections::HashMap,
	fs::{self, File},
	io::{self, Write},
	path::{Path, PathBuf},
};

use self::{options::Settings, reporting::DrawingInfo};

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
}

pub fn compile_project() {
	let mut errors = Vec::new();

	let main_template_path = PathBuf::from("templates/main.ks");
	let template_paths = recursive_crawl(&PathBuf::from("templates")).0;
	let mut template_files = HashMap::new();
	for path in template_paths.iter() {
		let input_string = match fs::read_to_string(path) {
			Ok(input_string) => input_string,
			Err(error) => {
				errors.push(Error::IOError(error, path.clone()));
				continue;
			}
		};
		let tokens = lexer::tokenize(&input_string);
		match parser::file(tokens, Some(path.clone()), None, None) {
			Ok(parsed_file) => {
				template_files.insert(path.clone(), parsed_file);
			}
			Err(error) => eprintln!(
				"{}",
				reporting::draw_error(
					&error.0.unpack(),
					&DrawingInfo::from((&error.1, Some(&path)))
				)
			),
		}
	}

	if !errors.is_empty() {
		report_errors(errors);
		return;
	}

	let Some(main_template) = template_files.get(&main_template_path) else {
		errors.push(Error::NoMainTemplate);
		report_errors(errors);
		return;
	};
	let input_paths = recursive_crawl(&PathBuf::from("input")).0;
	let mut input_files = Vec::new();

	for path in input_paths.iter() {
		let input_string = match fs::read_to_string(path) {
			Ok(input_string) => input_string,
			Err(error) => {
				errors.push(Error::IOError(error, path.clone()));
				continue;
			}
		};
		let tokens = lexer::tokenize(&input_string);
		match parser::file(
			tokens,
			Some(path.clone()),
			Some(&main_template),
			Some(&template_files),
		) {
			Ok(mut x) => {
				x.template = Some(&main_template);
				input_files.push(x);
			}
			Err(x) => eprintln!(
				"{}",
				reporting::draw_error(&x.0.unpack(), &DrawingInfo::from((&x.1, Some(&path))))
			),
		}
	}

	if !errors.is_empty() {
		report_errors(errors);
		return;
	}

	let settings = Settings::new();
	for file in input_files.iter() {
		match html::generate_html(&file, vec![], &settings) {
			Ok(x) => {
				let output_path = PathBuf::from("output");
				if let Some(path) = &file.path {
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
					eprintln!("{}", reporting::draw_scoped_error(&error));
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

pub fn report_errors(errors: Vec<Error>) {
	for error in errors {
		match error {
            Error::IOError(error, path) => eprintln!("Error reading `{}`: {}", path.to_string_lossy(), error),
            Error::NoMainTemplate => eprintln!("Coudln't compile project because it doesn't have a template in templates/main.ks"),
            Error::OutputNotInOutputFolder(path) => eprintln!("Tried to output {} to a location outside the project's output folder.\n\nThis is meant to be impossible, please contact the developer at https://ampersandia.net/", path.to_string_lossy()),
            Error::TemplateInOutputFolder(path) => eprintln!("{} is a template, but it is in the input folder", path.to_string_lossy()),
        }
	}
}

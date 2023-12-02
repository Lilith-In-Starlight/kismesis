use std::{ collections::HashMap, path::{Path, PathBuf}, fs};
use crate::kiss;
use crate::errors::TemplatingError;
use crate::kiss::parser::ParsedFile;

type TemplateDict = HashMap<PathBuf, ParsedFile>;

pub fn get_template(from: &Path) -> (TemplateDict, Vec<TemplatingError>) {
	let mut errors = Vec::new();
	let mut templates = TemplateDict::new();

	if from.is_dir() {
		match fs::read_dir(from) {
			Ok(entries) => {
				for entry in entries {
					let entry = match entry {
						Ok(x) => x,
						Err(x) => {
							errors.push(TemplatingError::IOError(x, from.to_path_buf()));
							continue
						}
					};
					let path = entry.path();
					let (templates_in_entry, mut errors_in_entry) = get_template(&path);
					templates.extend(templates_in_entry);
					errors.append(&mut errors_in_entry);
				}
			},
			Err(x) => errors.push(TemplatingError::CouldntReadDir(x, from.to_path_buf())),
		}
	} else {
		match kiss::parse_template(&from) {
			Ok(ast) => { templates.insert(from.to_path_buf(), ast); },
			Err(x) => { errors.push(x); },
		}
	}
	(templates, errors)
}


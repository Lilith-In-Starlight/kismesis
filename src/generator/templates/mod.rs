use std::{ collections::HashMap, path::{Path, PathBuf}, fs};
use crate::kiss::{self, compiler_options::CompilerOptions};
use crate::errors::{TemplatingError, report_error};
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
			Ok(ast) => {
				match kiss::to_html(ast.1, &ast.0, CompilerOptions::for_templates()) {
					Ok(_) => { templates.insert(from.to_path_buf(), ast.0); },
					Err(x) => {
						report_error(&from, &x.scanner, x.unresolved, x.resolved);
						errors.push(TemplatingError::ParseFailed(from.to_path_buf()));
					},
				}
				// templates.insert(from.to_path_buf(), ast);
			},
			Err(x) => { errors.push(x); },
		}
	}
	(templates, errors)
}


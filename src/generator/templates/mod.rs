use std::{ collections::HashMap, path::{Path, PathBuf}, ffi::OsStr, fs};
use crate::kiss;
use crate::errors::TemplatingError;
use crate::generator::kispaths;
use crate::kiss::parser::ParsedFile;

type TemplateDict = HashMap<PathBuf, ParsedFile>;

pub fn get_templates() -> Result<TemplateDict, TemplatingError> {
	let mut templates = TemplateDict::new();
	
	if kispaths::templates().is_dir() {
		for entry in fs::read_dir(kispaths::templates()).unwrap() {
			let entry = entry.map_err(TemplatingError::IOError)?;
			let path = entry.path();
			if !matches!(path.extension().and_then(OsStr::to_str), Some("kiss")) {
				continue
			}
			match get_template(&path) {
				Ok(x) => templates.extend(x.into_iter()),
				Err(x) => return Err(x),
			}
		}
	}
	Ok(templates)
}

fn get_template(from: &Path) -> Result<TemplateDict, TemplatingError> {
	let mut templates = TemplateDict::new();

	if from.is_dir() {
		for entry in fs::read_dir(from).unwrap() {
			let entry = entry.map_err(|x| TemplatingError::IOError(x))?;
			let path = entry.path();
			get_template(&path);
		}
	} else {
		let ast = match kiss::parse_template(&from) {
			Ok(x) => x,
			Err(x) => return Err(x)
		};
		templates.insert(from.to_path_buf(), ast);
	}
	Ok(templates)
}


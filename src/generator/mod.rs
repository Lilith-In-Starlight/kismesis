mod kispaths;
mod templates;

use std::{fs, path::{Path, PathBuf}};

use crate::{errors::{TemplatingError, self, report_error}, kiss::{self, parser::{ParsedFile, tag_stack::elements::Macro}}};


pub fn generate() {
	let (templates, template_errors) = templates::get_template(kispaths::templates());

	let path = kispaths::input();
	let out = kispaths::output();
	if !path.is_dir() {
		errors::write_stateless_report(None, TemplatingError::ExpectedADir(path.to_path_buf()));
		return
	}
	let generation_errors = generate_path(path, out, &templates.get(&PathBuf::from("test/templates/main.kiss")).unwrap());

	for error in generation_errors.into_iter().chain(template_errors.into_iter()) {
		errors::write_stateless_report(None, error);
	}
}

pub fn generate_path(path: &Path, outpath: &Path, template: &ParsedFile) -> Vec<TemplatingError> {
	let mut errors = Vec::new();
	let entries = match fs::read_dir(path) {
		Ok(x) => x,
		Err(x) => return vec![TemplatingError::CouldntReadDir(x, path.to_path_buf())]
	};
	for entry in entries {
		let entry = match entry {
			Ok(x) => x,
			Err(x) => {
				errors.push(TemplatingError::IOError(x, path.to_path_buf()));
				continue
			}
		};
		let path = entry.path();

		if path.is_dir() {
			errors.append(&mut generate_path(&path, outpath, template));
			continue
		}
		
		let (token_scanner, parsed) = match kiss::parse_file(&path) {
			Ok(x) => x,
			Err(x) => {
				errors.push(x);
				continue
			},
		};
		
		
		let content_macro = Macro {
			name: "content".into(),
			args: Vec::new(),
			children: parsed.body,
			line: 0,
			pos_in_line: 0,
		};

		let mut full_out = template.clone();
		full_out.macros.push(content_macro);

		
		let string = match kiss::to_html(token_scanner, full_out) {
			Ok(x) => x,
			Err(x) => { report_error(&path, &x.scanner, None, x.resolved); continue },
		};
	}
	return errors
}

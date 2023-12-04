mod kispaths;
mod templates;

use std::{fs, path::{Path, PathBuf}, collections::HashMap};

use crate::{errors::{TemplatingError, self, report_error}, kiss::{self, parser::{ParsedFile, tag_stack::elements::{Macro, }}, compiler_options::CompilerOptions}};

type Templates = HashMap<PathBuf, ParsedFile>;


pub fn generate() {
	let (templates, template_errors) = templates::get_template(kispaths::templates());

	let path = kispaths::input();
	if !path.is_dir() {
		println!("{}", errors::write_stateless_report(None, TemplatingError::ExpectedADir(path.to_path_buf())));
	}
	
	let mut generation_errors = Vec::new();
	match templates.get(kispaths::main_template()) {
		Some(main_template) => generation_errors.append(&mut generate_path(path, main_template, &templates)),
		None => generation_errors.push(TemplatingError::NoTemplate(kispaths::main_template().to_path_buf())),
	}

	for error in generation_errors.into_iter().chain(template_errors.into_iter()) {
		println!("{}", errors::write_stateless_report(None, error));
	}
}

pub fn generate_path(path: &Path, main_template: &ParsedFile, templates: &Templates) -> Vec<TemplatingError> {
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
			errors.append(&mut generate_path(&path, main_template, &templates));
			continue
		}
		
		let (token_scanner,parsed) = match kiss::parse_file(&path) {
			Ok(x) => x,
			Err(x) => {
				errors.push(x);
				continue
			},
		};
		
		let template = {
			let obtained = match parsed.consts.iter().find(|x| x.name == "template") {
				None => main_template,
				Some(x) => match templates.get(&PathBuf::from(x.value.clone())) {
					Some(x) => { x },
					None => continue,
				},
			};
			let mut clone = obtained.clone();
			let mut consts = parsed.consts.clone();
			consts.append(&mut clone.consts);
			clone.consts = consts;
			clone
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

		let string = match kiss::to_html(token_scanner, &full_out, CompilerOptions::default()) {
			Ok(x) => {
				let mut can_succed = true;
				for lamb in full_out.lambdaconsts {
					if !parsed.consts.iter().any(|x| x.name == lamb) {
						can_succed = false;
						errors.push(TemplatingError::UnsetLambda(path.clone(), lamb))
					}
				}
				if can_succed { x } else { continue }
			},
			Err(x) => { report_error(&path, &x.scanner, None, x.resolved); continue },
		};

		let output_path: &Path = kispaths::output();
		let output_path: PathBuf = output_path.iter().chain(path.iter().skip(1)).collect();

		fs::create_dir_all(output_path.parent().unwrap()).unwrap();
		fs::write(output_path, string).unwrap();
		
	}
	return errors
}


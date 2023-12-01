mod kispaths;
mod templates;

use std::{fs, path::{Path, PathBuf}};

use crate::{errors::TemplatingError, kiss::{self, parser::{ParsedFile, tag_stack::elements::Macro}}};



pub fn generate() -> Result<(), TemplatingError> {
	let templates = templates::get_templates()?;
	println!("made it past templates");

	let path = kispaths::input();
	let out = kispaths::output();
	if !path.is_dir() {
		return Err(TemplatingError::InputNotDir)
	}

	generate_this(path, out, &templates.get(&PathBuf::from("test/templates/main.kiss")).unwrap());
	
	Ok(())
}

pub fn generate_this(path: &Path, outpath: &Path, template: &ParsedFile) -> Result<(), TemplatingError> {
	for entry in fs::read_dir(kispaths::input()).unwrap() {
		let entry = entry.map_err(|x| TemplatingError::IOError(x))?;
		let path = entry.path();

		if path.is_dir() {
			generate_this(&path, outpath, template);
			return Ok(());
		}
		
		let (token_scanner, parsed) = match kiss::parse_file(&path) {
			Ok(x) => x,
			Err(_) => continue,
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

		
		let string = match kiss::to_html(token_scanner, full_out, None) {
			Ok(x) => x,
			Err(x) => {dbg!(&x); continue},
		};
		println!("{}", string);
	}
	Ok(())
}

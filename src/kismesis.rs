//! # Kismesis
//! Module for everything related to the Kismesis templating engine.
//!
//! # Using the engine
//! This module contains the `Kismesis` struct, which is utilized in
//! every templating operation. It also works as an arena that holds
//! templates and token strings.

pub(crate) mod compiler;
mod plugins;

use extism::{Wasm, Plugin, Manifest};
use std::{
	collections::HashMap,
	fs, io,
	path::{Path, PathBuf},
};



use compiler::{
	lexer::{self, Token},
	parser::{self, errors::Err, types::{ParsedFile, TextPos, Ranged, HtmlNodes}},
};



pub type KisResult<T> = Result<T, KismesisError>;

pub enum KismesisError {
	IOError(io::Error, PathBuf),
	ParseError(Err, KisID),
}

#[derive(Debug)]
pub struct FileRef {
	pub tokens: Vec<Token>,
	pub path: Option<PathBuf>,
}

#[derive(Default, Debug)]
pub struct Kismesis {
	tokens: HashMap<KisID, FileRef>,
	templates: HashMap<KisTemplateID, ParsedFile>,
	plugins: HashMap<String, Manifest>,
	id: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KisID(usize);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum KisTemplateID {
	Input(usize),
	File(PathBuf),
}

impl Kismesis {
	pub fn new() -> Self {
		Self {
			tokens: HashMap::new(),
			templates: HashMap::new(),
			plugins: HashMap::new(),
			id: 0,
		}
	}

	pub fn drop_id(&mut self, id: &KisID) {
		self.tokens.remove(id);
	}

	pub fn run_plugin(
		&self,
		name: &str,
		range: TextPos,
		params: Ranged<Vec<Token>>,
		body: Option<Ranged<Vec<Token>>>,
		project_path: Option<PathBuf>,
	) -> Vec<HtmlNodes>
	{
		todo!();
	}

	pub fn register_plugin(
		&mut self,
		name: String,
		path: &Path
	) {
		let plugin = Wasm::file(path);
		let manifest = Manifest::new([plugin]);
		self.plugins.insert(name, manifest);
	}

	pub fn call_plugin(&self, name: &str) {
		let manifest = self.plugins.get(name).unwrap().clone();
		let mut plugin = Plugin::new(&manifest, [], false).unwrap();
		println!("{}", plugin.call::<_, String>("parser", "").unwrap());
	}

	pub fn register_tokens(&mut self, tokens: Vec<Token>, path: Option<PathBuf>) -> KisID {
		let new_kis_id = KisID(self.id);
		self.id += 1;
		self.tokens.insert(new_kis_id.clone(), FileRef { tokens, path });
		new_kis_id
	}

	pub fn register_file(&mut self, path: PathBuf, project: Option<PathBuf>) -> KisResult<ParsedFile> {
		let text =
			fs::read_to_string(&path).map_err(|x| KismesisError::IOError(x, path.clone()))?;
		let tokens = lexer::tokenize(&text);
		let tokens = self.register_tokens(tokens, Some(path));
		let file =
			parser::file(tokens, self, None, project).map_err(|x| KismesisError::ParseError(x, tokens))?;
		Ok(file)
	}

	pub fn register_template(&mut self, file: ParsedFile) -> KisTemplateID {
		let output_id = match self.get_file(file.file_id).and_then(|x| x.path.clone()) {
			Some(path) => KisTemplateID::File(path),
			None => KisTemplateID::Input(self.templates.len()),
		};
		self.templates.insert(output_id.clone(), file);
		output_id
	}

	pub fn get_template<T>(&self, id: T) -> Option<&ParsedFile>
	where
		T: Into<KisTemplateID>,
	{
		self.templates.get(&id.into())
	}
	pub fn verify_template_id<T>(&self, id: T) -> Option<KisTemplateID>
	where
		T: Into<KisTemplateID>,
	{
		let id = id.into();
		if self.has_template(id.clone()) {
			Some(id)
		} else {
			None
		}
	}
	pub fn has_template<T>(&self, id: T) -> bool
	where
		T: Into<KisTemplateID>,
	{
		self.templates.get(&id.into()).is_some()
	}
	pub fn get_file(&self, id: KisID) -> Option<&FileRef> {
		self.tokens.get(&id)
	}
}

impl From<PathBuf> for KisTemplateID {
	fn from(val: PathBuf) -> KisTemplateID {
		KisTemplateID::File(val)
	}
}

impl From<&Path> for KisTemplateID {
	fn from(val: &Path) -> KisTemplateID {
		KisTemplateID::File(val.to_path_buf())
	}
}

impl From<&str> for KisTemplateID {
	fn from(val: &str) -> KisTemplateID {
		KisTemplateID::File(PathBuf::from(val))
	}
}

impl From<&KisTemplateID> for KisTemplateID {
	fn from(val: &KisTemplateID) -> KisTemplateID {
		val.clone()
	}
}

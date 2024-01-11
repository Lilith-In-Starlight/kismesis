//! # Kismesis
//! Module for everything related to the Kismesis templating engine.
//!
//! # Using the engine
//! This module contains the `Kismesis` struct, which is utilized in
//! every templating operation. It also works as an arena that holds
//! templates and token strings.

pub(crate) mod compiler;
mod plugins;

#[cfg(feature="plugins")]
use extism::{Wasm, Plugin, Manifest, convert::Json};

use self::compiler::parser::types::Ranged;
use std::{
	collections::HashMap,
	fs, io,
	path::{Path, PathBuf},
};

use compiler::{
	lexer::{self, Token},
	parser::{self, errors::Err, types::{ParsedFile, HtmlNodes}},
};

pub type KisResult<T> = Result<T, KismesisError>;

pub enum KismesisError {
	IOError(io::Error, PathBuf),
	ParseError(Err, KisID),
}

/// # FileRef
/// The tokens and path (if any) of a file.
#[derive(Debug)]
pub struct FileRef {
	pub tokens: Vec<Token>,
	pub path: Option<PathBuf>,
}

/// # Kismesis Engine
/// A struct which contains Kismesis data that might be self-referential, such as templates (which might be recursuve)
#[derive(Default, Debug)]
#[cfg(feature="plugins")]
pub struct Kismesis {
	/// The tokens of all the files.
	tokens: HashMap<KisID, FileRef>,
	/// The loaded templates.
	templates: HashMap<KisTemplateID, ParsedFile>,
	/// The loaded plugins.
	plugins: HashMap<String, Manifest>,
	/// The next ID to be used for the ID in case templates are registered from input that is not from a file.
	id: usize,
}

#[derive(Default, Debug)]
#[cfg(not(feature="plugins"))]
/// # Kismesis Engine
/// A struct which contains Kismesis data that might be self-referential, such as templates (which might be recursuve)
pub struct Kismesis {
	/// The tokens of all the files.
	tokens: HashMap<KisID, FileRef>,
	/// The loaded templates.
	templates: HashMap<KisTemplateID, ParsedFile>,
	/// The next ID to be used for the ID in case templates are registered from input that is not from a file.
	id: usize,
}

/// ID newtype for kismesis tokens
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KisID(usize);

/// IDs for kismesis templates
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum KisTemplateID {
	/// Input is not from a file
	Input(usize),
	/// Input is from a file
	File(PathBuf),
}

impl Kismesis {
	#[cfg(feature="plugins")]
	pub fn new() -> Self {
		Self {
			tokens: HashMap::new(),
			templates: HashMap::new(),
			plugins: HashMap::new(),
			id: 0,
		}
	}
	#[cfg(not(feature="plugins"))]
	pub fn new() -> Self {
		Self {
			tokens: HashMap::new(),
			templates: HashMap::new(),
			id: 0,
		}
	}

	/// Remove a set file from the engine's registry
	pub fn drop_id(&mut self, id: &KisID) {
		self.tokens.remove(id);
	}

	/// Register a plugin from a path
	#[cfg(feature="plugins")]
	pub fn register_plugin(
		&mut self,
		name: String,
		path: &Path
	) {
		let plugin = Wasm::file(path);
		let manifest = Manifest::new([plugin]);
		self.plugins.insert(name, manifest);
	}

	/// Register a plugin from a path
	#[cfg(not(feature="plugins"))]
	pub fn register_plugin(
		&mut self,
		_name: String,
		_path: &Path
	) {
		()
	}

	/// Send tokens and body to a plugin with a given `name`
	#[cfg(feature="plugins")]
	pub fn call_plugin(&self, name: &str, tokens: Ranged<Vec<Token>>, body: Option<Ranged<Vec<Token>>>) -> Result<Vec<HtmlNodes>, ()> {
		let manifest = self.plugins.get(name).unwrap().clone();
		let mut plugin = Plugin::new(&manifest, [], false).unwrap();
		let param_field_json = serde_json::to_string(&tokens).unwrap();
		let body_json = serde_json::to_string(&body).unwrap();
		let input_json = serde_json::to_string(&[param_field_json, body_json]).unwrap();
		let Json(text) = plugin.call::<_, Json<Vec<HtmlNodes>>>("parser", input_json).unwrap();
		Ok(text)
	}

	#[cfg(not(feature="plugins"))]
	/// Send tokens and body to a plugin with a given `name`
	pub fn call_plugin(&self, _name: &str, _tokens: Vec<Token>) -> Result<Vec<HtmlNodes>, ()> {
		Err(())
	}

	/// Register a file
	pub fn register_tokens(&mut self, tokens: Vec<Token>, path: Option<PathBuf>) -> KisID {
		let new_kis_id = KisID(self.id);
		self.id += 1;
		self.tokens.insert(new_kis_id.clone(), FileRef { tokens, path });
		new_kis_id
	}

	/// Parse and register a file
	// TODO: This is a misnomer
	pub fn register_file(&mut self, path: PathBuf, project: Option<PathBuf>) -> KisResult<ParsedFile> {
		let text =
			fs::read_to_string(&path).map_err(|x| KismesisError::IOError(x, path.clone()))?;
		let tokens = lexer::tokenize(&text);
		let tokens = self.register_tokens(tokens, Some(path));
		let file =
			parser::file(tokens, self, None, project).map_err(|x| KismesisError::ParseError(x, tokens))?;
		Ok(file)
	}

	/// Register a template
	pub fn register_template(&mut self, file: ParsedFile) -> KisTemplateID {
		let output_id = match self.get_file(file.file_id).and_then(|x| x.path.clone()) {
			Some(path) => KisTemplateID::File(path),
			None => KisTemplateID::Input(self.templates.len()),
		};
		self.templates.insert(output_id.clone(), file);
		output_id
	}

	/// Get a template from an ID
	pub fn get_template<T>(&self, id: T) -> Option<&ParsedFile>
	where
		T: Into<KisTemplateID>,
	{
		self.templates.get(&id.into())
	}

	/// Verify whether a template ID is registered
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

	/// Verify whether a template ID is registered
	pub fn has_template<T>(&self, id: T) -> bool
	where
		T: Into<KisTemplateID>,
	{
		self.templates.get(&id.into()).is_some()
	}

	/// Get the registered file that corresponds to the given ID, if any
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

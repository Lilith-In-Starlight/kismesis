pub(crate) mod compiler;
mod plugins;

use std::{
	collections::HashMap,
	fs, io,
	path::{Path, PathBuf},
};

use rhai::{Engine, AST, Scope, Array};

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
	tokens: Vec<FileRef>,
	templates: HashMap<KisTemplateID, ParsedFile>,
	plugin_engine: Engine,
	plugins: HashMap<String, AST>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct KisID(usize);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum KisTemplateID {
	Input(usize),
	File(PathBuf),
}

impl Kismesis {
	pub fn new() -> Self {
		Self {
			plugin_engine: plugins::new_engine(),
			tokens: vec![],
			templates: HashMap::new(),
			plugins: HashMap::new(),
		}
	}

	pub fn register_plugin(&mut self, plugin: &str, name: &str) {
		let ast = self.plugin_engine.compile(plugin).unwrap();
		self.plugins.insert(name.to_string(), ast);
	}

	pub fn run_plugin(&self, name: &str, range: TextPos, params: Ranged<Vec<Token>>, body: Option<Ranged<Vec<Token>>>) -> Vec<HtmlNodes> {
		let plugin = self.plugins.get(name).unwrap();
		let string: Array = self.plugin_engine.call_fn(&mut Scope::new(), plugin, "token_call", (range, params.value, body.map(|x| x.value).unwrap_or(vec![]))).unwrap();
		plugins::into_html_nodes(string)
	}

	pub fn register_tokens(&mut self, tokens: Vec<Token>, path: Option<PathBuf>) -> KisID {
		self.tokens.push(FileRef { tokens, path });
		KisID(self.tokens.len() - 1)
	}

	pub fn register_file(&mut self, path: PathBuf) -> KisResult<ParsedFile> {
		let text =
			fs::read_to_string(&path).map_err(|x| KismesisError::IOError(x, path.clone()))?;
		let tokens = lexer::tokenize(&text);
		let tokens = self.register_tokens(tokens, Some(path));
		let file =
			parser::file(tokens, self, None).map_err(|x| KismesisError::ParseError(x, tokens))?;
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
		self.tokens.get(id.0)
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

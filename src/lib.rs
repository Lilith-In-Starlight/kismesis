//! This crate is used to create engines based on Kismesis.
//!
//! # Basic Use
//! First, we create the engine and register some files.
//! ```
//! use kismesis::Kismesis;
//! let mut engine = Kismesis::new(); // Engine must be mutable
//! let mut my_parsed_file = engine.register_str("<p: this is a kismesis string!>").unwrap();
//! ```
//!
//! This will give us a [`ParsedFile`] that we can then use during templating. Templates can only be used through a [`KisTemplateId`] because they might have other templates and even be self referential. The engine provides [`register_template`](Kismesis::register_template) to store the file and obtain the ID.
//!
//! ```
//! # use kismesis::Kismesis;
//! # use kismesis::html;
//! # let mut engine = Kismesis::new(); // Engine must be mutable
//! # let mut my_parsed_file = engine.register_str("<p: this is a kismesis string!>").unwrap();
//! // This obtains a ParsedFile.
//! let my_template = engine.register_str("# Test\n<content!>").unwrap();
//! // This registers that file as a template, returning a KisTemplateId
//! let my_template = engine.register_template(my_template);
//!
//! my_parsed_file.template = Some(my_template);
//!
//! println!("{}", html::compile(&my_parsed_file, &engine).unwrap());
//! ```
//!
//!

pub mod errors;
pub mod html;
pub mod lexer;
pub mod options;
pub mod parser;
#[cfg(feature = "pdk")]
pub mod pdk;
pub mod plugins;

#[cfg(feature = "reporting")]
pub mod reporting;

#[cfg(feature = "plugins")]
use extism::{convert::Json, Manifest, Plugin, Wasm};
use html::CompileResult;
use parser::types::MultilineRange;

use options::Settings;
use plugins::PluginInput;
use plugins::PostProcPluginInput;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use self::parser::errors::ParseError;
use self::parser::types::Ranged;

use std::fmt::Display;
use std::{
	collections::HashMap,
	fs, io,
	path::{Path, PathBuf},
};

use lexer::Token;
use parser::{
	errors::Err,
	types::{HtmlNodes, ParsedFile},
};

pub type KisResult<T> = Result<T, KismesisError>;

#[derive(Debug)]
pub enum KismesisError {
	IOError(io::Error, PathBuf),
	ParseError(Vec<Err>, KisTokenId),
}

/// Error struct used in plugin parsing
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg(any(feature = "plugins", feature = "pdk"))]
pub struct PluginParseError {
	/// The message displayed as an error
	message: String,
	/// Hints displayed giving more information to the error
	hints: Vec<PluginParseError>,
	/// Where the error is located in the text. The Some variant represents a position, whereas the None variant represents statlessness.
	state: Option<MultilineRange>,
}

#[cfg(any(feature = "plugins", feature = "pdk"))]
impl PluginParseError {
	#[must_use]
	pub fn new(message: String, state: Option<MultilineRange>) -> Self {
		Self {
			message,
			hints: vec![],
			state,
		}
	}
	/// Adds a hint to the error struct
	pub fn add_hint(&mut self, message: String, state: Option<MultilineRange>) {
		self.hints.push(Self::new(message, state));
	}
}

/// The tokens and path (if any) of a file.
#[derive(Debug)]
pub struct FileRef {
	pub tokens: Vec<Token>,
	pub path: Option<PathBuf>,
}

/// A struct which contains Kismesis data that might be self-referential, such as templates (which might be recursuve)
#[derive(Default, Debug)]
#[cfg(feature = "plugins")]
pub struct Kismesis {
	/// The next ID to be used for the ID in case templates are registered from input that is not from a file.
	id: usize,
	/// The loaded plugins.
	plugins: HashMap<String, Manifest>,
	/// The settings to be used for this instance of Kismesis
	pub settings: Settings,
	/// The loaded templates.
	templates: HashMap<KisTemplateId, ParsedFile>,
	/// The tokens of all the files.
	tokens: HashMap<KisTokenId, FileRef>,
}

#[derive(Default, Debug)]
#[cfg(not(feature = "plugins"))]
/// A struct which contains Kismesis data that might be self-referential, such as templates (which might be recursuve)
pub struct Kismesis {
	/// The next ID to be used for the ID in case templates are registered from input that is not from a file.
	id: usize,
	/// The settings to be used for this instance of Kismesis
	pub settings: Settings,
	/// The loaded templates.
	templates: HashMap<KisTemplateId, ParsedFile>,
	/// The tokens of all the files.
	tokens: HashMap<KisTokenId, FileRef>,
}

/// ID newtype for kismesis tokens
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct KisTokenId(usize);

impl Display for KisTokenId {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}

/// IDs for kismesis templates
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum KisTemplateId {
	/// Input is not from a file
	Input(usize),
	/// Input is from a file
	File(PathBuf),
}

impl Kismesis {
	#[cfg(feature = "plugins")]
	#[must_use]
	pub fn new() -> Self {
		Self {
			tokens: HashMap::new(),
			templates: HashMap::new(),
			plugins: HashMap::new(),
			id: 0,
			settings: Settings::default(),
		}
	}
	#[cfg(not(feature = "plugins"))]
	#[must_use]
	pub fn new() -> Self {
		Self {
			tokens: HashMap::new(),
			templates: HashMap::new(),
			id: 0,
			settings: Settings::default(),
		}
	}

	/// Remove a set file from the engine's registry
	pub fn drop_id(&mut self, id: &KisTokenId) {
		self.tokens.remove(id);
	}

	/// Register a plugin from a path
	#[cfg(feature = "plugins")]
	pub fn register_plugin(&mut self, name: String, path: &Path) {
		let plugin = Wasm::file(path);
		let manifest = Manifest::new([plugin]);
		let manifest = manifest.with_allowed_hosts(std::iter::once("*".to_string()));
		let manifest = manifest.with_allowed_paths(
			[
				(PathBuf::from("input"), PathBuf::from("input")),
				(PathBuf::from("output"), PathBuf::from("output")),
			]
			.into_iter(),
		);
		self.plugins.insert(name, manifest);
	}

	/// Register a plugin from a path
	#[cfg(not(feature = "plugins"))]
	pub fn register_plugin(&mut self, name: String, _path: &Path) {
		drop(name);
	}

	/// Send tokens and body to a plugin with a given `name`
	/// # Errors
	/// When the plugin cannot be called, and when the pluing returns an error
	#[cfg(feature = "plugins")]
	pub fn call_plugin(
		&self,
		name: &Ranged<String>,
		input: PluginInput,
	) -> Result<Vec<HtmlNodes>, Err> {
		use errors::ErrorKind;

		use crate::parser::errors::{Hintable, Hints};

		if self.settings.has_plugin(&name.value) {
			return Err(ParseError::PluginIsUndeclared
				.error_at_pos(name.range.clone())
				.cut());
		}

		let manifest = match self.plugins.get(&name.value) {
			Some(x) => x.clone(),
			None => {
				return Err(ParseError::PluginDoesntExist
					.error_at_pos(name.range.clone())
					.cut())
			}
		};
		let mut plugin = match Plugin::new(manifest, [], true) {
			Ok(x) => x,
			Err(x) => {
				return Err(ParseError::ExtismError(format!("{x}"))
					.error_at_pos(name.range.clone())
					.cut())
			}
		};
		let input = Json(input);
		match plugin.call::<_, Json<Result<_, PluginParseError>>>("parser", input) {
			Ok(Json(x)) => match x {
				Ok(x) => Ok(x),
				Err(x) => {
					let error = ParseError::PluginError(x.message);
					let mut error = ErrorKind::with_state_at(
						error,
						x.state.unwrap_or_else(|| name.range.clone()),
					);
					for hint in x.hints {
						// TODO let plugin hints be stateful
						let new_hint = Hints::CustomMessage(hint.message).stateless();
						error.add_hint(new_hint);
					}
					Err(Err::Failure(error))
				}
			},
			Err(x) => Err(ParseError::ExtismError(format!("{x}"))
				.error_at_pos(name.range.clone())
				.cut()),
		}
	}

	/// # Errors
	/// Never
	#[cfg(not(feature = "plugins"))]
	pub fn call_post_processing_plugins(
		&self,
		input: PostProcPluginInput,
	) -> CompileResult<(ParsedFile, Vec<ParsedFile>)> {
		Ok(input.body)
	}

	/// # Errors
	/// Whenever any plugin in the post-processing pipeline fails
	#[cfg(feature = "plugins")]
	pub fn call_post_processing_plugins(
		&self,
		mut input: PostProcPluginInput,
	) -> CompileResult<(ParsedFile, Vec<ParsedFile>)> {
		let current_file = input.current_file.clone();
		for plugin in self.settings.post_processing() {
			let body = self.call_post_processing_plugin(input, plugin)?;
			input = PostProcPluginInput {
				body,
				current_file: current_file.clone(),
			}
		}

		Ok(input.body)
	}

	/// Send file to a post-processing plugin with a given `name`
	/// # Errors
	/// When the plugin cannot be called, and when the pluing returns an error
	#[cfg(feature = "plugins")]
	pub fn call_post_processing_plugin(
		&self,
		input: PostProcPluginInput,
		name: &str,
	) -> CompileResult<(ParsedFile, Vec<ParsedFile>)> {
		use errors::ErrorKind;

		use crate::{
			html::CompilerError,
			parser::errors::{Hintable, Hints},
		};

		let manifest = match self.plugins.get(name) {
			Some(x) => x.clone(),
			None => {
				return Err(vec![
					CompilerError::PluginDoesntExist(name.to_owned()).unscoped()
				])
			}
		};
		let mut plugin = match Plugin::new(manifest, [], true) {
			Ok(x) => x,
			Err(x) => return Err(vec![CompilerError::ExtismError(format!("{x}")).unscoped()]),
		};
		let input = Json(input);
		match plugin.call::<_, Json<Result<_, PluginParseError>>>("parser", input) {
			Ok(Json(x)) => match x {
				Ok(x) => Ok(x),
				Err(x) => {
					// TODO let errors be stateless
					let mut error = CompilerError::PluginError(x.message).stateless();
					for hint in x.hints {
						// TODO let plugin hints be stateful
						let new_hint = Hints::CustomMessage(hint.message).stateless();
						error.add_hint(new_hint);
					}
					Err(vec![error.into()])
				}
			},
			Err(_) => todo!(),
		}
	}

	#[cfg(not(feature = "plugins"))]
	/// Send tokens and body to a plugin with a given `name`
	/// # Errors
	/// Whenever called, as the plugins feature is not enabled
	pub fn call_plugin(
		&self,
		name: &Ranged<String>,
		input: PluginInput,
	) -> Result<Vec<HtmlNodes>, Err> {
		drop(input);
		Err(ParseError::PluginsDisabled
			.error_at_pos(name.range.clone())
			.cut())
	}

	/// Register a file
	pub fn register_tokens(&mut self, tokens: Vec<Token>, path: Option<PathBuf>) -> KisTokenId {
		let new_kis_id = KisTokenId(self.id);
		self.id += 1;
		self.tokens.insert(new_kis_id, FileRef { tokens, path });
		new_kis_id
	}

	/// Parse and register a file
	/// # Errors
	/// If the file cannot be parsed, or if there is an IO error when accessing or reading `path`.
	pub fn register_file(&mut self, path: PathBuf) -> KisResult<ParsedFile> {
		let text =
			fs::read_to_string(&path).map_err(|x| KismesisError::IOError(x, path.clone()))?;
		let tokens = lexer::tokenize(&text);
		let tokens = self.register_tokens(tokens, Some(path.clone()));
		let file = parser::file(tokens, self, None, Some(path))
			.map_err(|x| KismesisError::ParseError(x, tokens))?;
		Ok(file)
	}

	/// Parse and register a string of text
	/// # Errors
	/// If the string cannot be parsed.
	pub fn register_str(&mut self, string: &str) -> KisResult<ParsedFile> {
		let tokens = lexer::tokenize(string);
		let tokens = self.register_tokens(tokens, None);
		let file = parser::file(tokens, self, None, None)
			.map_err(|x| KismesisError::ParseError(x, tokens))?;
		Ok(file)
	}

	/// Register a template.
	pub fn register_template(&mut self, file: ParsedFile) -> KisTemplateId {
		let output_id = match self.get_file(file.file_id).and_then(|x| x.path.clone()) {
			Some(path) => KisTemplateId::File(path),
			None => KisTemplateId::Input(self.templates.len()),
		};
		self.templates.insert(output_id.clone(), file);
		output_id
	}

	/// Get a template from an ID.
	pub fn get_template<T>(&self, id: T) -> Option<&ParsedFile>
	where
		T: Into<KisTemplateId>,
	{
		self.templates.get(&id.into())
	}

	/// Verify whether a template ID is registered
	pub fn verify_template_id<T>(&self, id: T) -> Option<KisTemplateId>
	where
		T: Into<KisTemplateId>,
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
		T: Into<KisTemplateId>,
	{
		self.templates.contains_key(&id.into())
	}

	/// Get the registered file that corresponds to the given ID, if any
	#[must_use]
	pub fn get_file(&self, id: KisTokenId) -> Option<&FileRef> {
		self.tokens.get(&id)
	}
}

impl From<PathBuf> for KisTemplateId {
	fn from(val: PathBuf) -> Self {
		Self::File(val)
	}
}

impl From<&Path> for KisTemplateId {
	fn from(val: &Path) -> Self {
		Self::File(val.to_path_buf())
	}
}

impl From<&str> for KisTemplateId {
	fn from(val: &str) -> Self {
		Self::File(PathBuf::from(val))
	}
}

impl From<&Self> for KisTemplateId {
	fn from(val: &Self) -> Self {
		val.clone()
	}
}

/// Trait for things that implement Push to convert into that type before pushing
pub trait PushInto<T> {
	/// Converts into T before pushing
	fn push_into<B: Into<T>>(&mut self, value: B);
}

pub trait GiveRange {
	fn with_range(self, range: MultilineRange) -> Ranged<Self>
	where
		Self: Sized,
	{
		Ranged { value: self, range }
	}
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod test {
	use std::{path::PathBuf, str::FromStr};

	use crate::{
		html,
		reporting::{DrawingInfo, Report, ReportKind},
		Kismesis,
	};

	#[test]
	fn test_file() {
		let mut engine = Kismesis::new();
		let template = engine
			.register_file(PathBuf::from_str("test/templating/template.kis").unwrap())
			.unwrap();
		let template = engine.register_template(template);
		let mut input = engine
			.register_file(PathBuf::from_str("test/templating/file.kis").unwrap())
			.unwrap();
		input.template = Some(template);
		let x = html::compile(&input, &engine).unwrap_err();
		for a in x {
			a.report(ReportKind::Error, &DrawingInfo::default(), &engine, 0);
		}
	}
}

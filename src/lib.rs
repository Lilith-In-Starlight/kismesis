//! # Kismesis
//! Module for everything related to the Kismesis templating engine.
//!
//! # Using the engine
//! This module contains the `Kismesis` struct, which is utilized in
//! every templating operation. It also works as an arena that holds
//! templates and token strings.

pub mod plugins;
pub mod errors;
pub mod html;
mod lexer;
pub mod options;
pub mod parser;

#[cfg(feature = "reporting")]
pub mod reporting;

#[cfg(feature = "plugins")]
use extism::{convert::Json, Manifest, Plugin, Wasm};

use plugins::PluginInput;

use parser::types::TextPos;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use self::parser::errors::ParseError;
use self::parser::types::Ranged;

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

pub enum KismesisError {
	IOError(io::Error, PathBuf),
	ParseError(Err, KisID),
}

/// Error struct used in plugin parsing
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PluginParseError {
	/// The message displayed as an error
	message: String,
	/// Hints displayed giving more information to the error
	hints: Vec<PluginParseError>,
	/// Where the error is located in the text. The Some variant represents a position, whereas the None variant represents statlessness.
	state: Option<TextPos>,
}

impl PluginParseError {
	pub fn new(message: String, state: Option<TextPos>) -> Self {
		PluginParseError {
			message,
			hints: vec![],
			state,
		}
	}
	/// Adds a hint to the error struct
	pub fn add_hint(&mut self, message: String, state: Option<TextPos>) {
		self.hints.push(Self::new(message, state))
	}
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
#[cfg(feature = "plugins")]
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
#[cfg(not(feature = "plugins"))]
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
	#[cfg(feature = "plugins")]
	pub fn new() -> Self {
		Self {
			tokens: HashMap::new(),
			templates: HashMap::new(),
			plugins: HashMap::new(),
			id: 0,
		}
	}
	#[cfg(not(feature = "plugins"))]
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
	#[cfg(feature = "plugins")]
	pub fn register_plugin(&mut self, name: String, path: &Path) {
		let plugin = Wasm::file(path);
		let manifest = Manifest::new([plugin]);
		let manifest = manifest.with_allowed_hosts(["*".to_string()].into_iter());
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
	pub fn register_plugin(&mut self, _name: String, _path: &Path) {}

	/// Send tokens and body to a plugin with a given `name`
	#[cfg(feature = "plugins")]
	pub fn call_plugin(
		&self,
		name: &Ranged<String>,
		input: PluginInput,
	) -> Result<Vec<HtmlNodes>, Err> {
    use errors::ErrorKind;

    use crate::parser::errors::{Hintable, Hints};


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
				return Err(ParseError::ExtismError(format!("{}", x))
					.error_at_pos(name.range.clone())
					.cut())
			}
		};
		let input = Json(input);
		match plugin.call::<_, Json<Result<Vec<HtmlNodes>, PluginParseError>>>("parser", input) {
			Ok(Json(x)) => match x {
				Ok(x) => Ok(x),
				Err(x) => {
					let error = ParseError::PluginError(x.message);
					let mut error = ErrorKind::with_state_at(error, x.state.unwrap_or(name.range.clone()));
					for hint in x.hints {
						// TODO let plugin hints be stateful
						let new_hint = Hints::CustomMessage(hint.message).stateless();
						error.add_hint(new_hint);
					}
					Err(Err::Failure(error))
				}
			},
			Err(x) => Err(ParseError::ExtismError(format!("{}", x))
				.error_at_pos(name.range.clone())
				.cut()),
		}
	}

	#[cfg(not(feature = "plugins"))]
	/// Send tokens and body to a plugin with a given `name`
	pub fn call_plugin(
		&self,
		name: &Ranged<String>,
		input: PluginInput,
	) -> Result<Vec<HtmlNodes>, Err> {
		Err(ParseError::PluginsDisabled.error_at_pos(name.range.clone()).cut())
	}

	/// Register a file
	pub fn register_tokens(&mut self, tokens: Vec<Token>, path: Option<PathBuf>) -> KisID {
		let new_kis_id = KisID(self.id);
		self.id += 1;
		self.tokens.insert(new_kis_id, FileRef { tokens, path });
		new_kis_id
	}

	/// Parse and register a file
	// TODO: This is a misnomer
	pub fn register_file(
		&mut self,
		path: PathBuf,
		project: Option<PathBuf>,
	) -> KisResult<ParsedFile> {
		let text =
			fs::read_to_string(&path).map_err(|x| KismesisError::IOError(x, path.clone()))?;
		let tokens = lexer::tokenize(&text);
		let tokens = self.register_tokens(tokens, Some(path));
		let file = parser::file(tokens, self, None, project)
			.map_err(|x| KismesisError::ParseError(x, tokens))?;
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

/// Exports important types as public for Plugin developers to use
#[cfg(feature="pdk")]
pub mod pdk {
	pub use super::lexer::Token;
	pub use super::parser::types::Argument;
	pub use super::parser::types::Attribute;
	pub use super::parser::types::ForTag;
	pub use super::parser::types::HtmlNodes;
	pub use super::parser::types::HtmlTag;
	pub use super::parser::types::IfTag;
	pub use super::parser::types::Macro;
	pub use super::parser::types::Ranged;
	pub use super::parser::types::Section;
	pub use super::parser::types::TextPos;

	pub use super::PluginParseError as PluginError;
	pub use super::plugins::PluginInput;

	pub type RangedTokens = Ranged<Vec<Token>>;
	pub type InputTuple = (RangedTokens, Option<RangedTokens>);
	pub type AST = Vec<HtmlNodes>;

	pub type PlugResult = Result<AST, PluginError>;
}

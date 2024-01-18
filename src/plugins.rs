use std::path::PathBuf;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{parser::types::Ranged, lexer::Token};

#[cfg_attr(feature = "serde", derive(Deserialize))]
pub struct PluginData {
	pub name: String,
	pub authors: Vec<String>,
	pub version: String,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct PluginInput {
	pub parameters: Ranged<Vec<Token>>,
	pub body: Option<Ranged<Vec<Token>>>,
	pub current_file: Option<PathBuf>,
}

#[cfg(feature="plugins")]
use serde::Deserialize;

#[cfg_attr(feature="plugins", derive(Deserialize))]
pub(crate) struct PluginData {
	pub(crate) name: String,
	pub(crate) authors: Vec<String>,
	pub(crate) version: String,
}

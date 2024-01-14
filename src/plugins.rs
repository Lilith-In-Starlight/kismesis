#[cfg(feature = "serde")]
use serde::Deserialize;

#[cfg_attr(feature = "serde", derive(Deserialize))]
pub(crate) struct PluginData {
	pub(crate) name: String,
	pub(crate) authors: Vec<String>,
	pub(crate) version: String,
}

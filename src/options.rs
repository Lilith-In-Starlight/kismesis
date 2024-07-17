//! Kismesis configuration data structure

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

const DEFAULT_INLINE: [&str; 41] = [
	"p", "b", "i", "strong", "italic", "sub", "sup", "h1", "h2", "h3", "h4", "h5", "h6", "a", "li",
	"title", "span", "emphasis", "code", "abbr", "bdo", "button", "cite", "dfn", "em", "img",
	"input", "kbd", "label", "map", "object", "output", "q", "script", "select", "small",
	"textarea", "time", "tt", "var", "pre",
];

const DEFAULT_ONLY_OPENER: [&str; 3] = ["meta", "img", "link"];
const DEFAULT_ONLY_CLOSER: [&str; 2] = ["br", "hr"];

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Settings {
	#[cfg_attr(feature = "serde", serde(default))]
	plugins: Vec<String>,
	#[cfg_attr(feature = "serde", serde(default))]
	post_processing_pipeline: Vec<String>,
	#[cfg_attr(feature = "serde", serde(default = "default_inline"))]
	#[cfg_attr(feature = "serde", serde(skip_serializing_if = "is_default_inline"))]
	inline: Vec<String>,
	#[cfg_attr(feature = "serde", serde(default = "default_only_closer"))]
	#[cfg_attr(
		feature = "serde",
		serde(skip_serializing_if = "is_default_only_closer")
	)]
	only_closer: Vec<String>,
	#[cfg_attr(feature = "serde", serde(default = "default_only_opener"))]
	#[cfg_attr(
		feature = "serde",
		serde(skip_serializing_if = "is_default_only_opener")
	)]
	only_opener: Vec<String>,
}

fn is_default_inline(obj: &[String]) -> bool {
	obj == DEFAULT_INLINE
}

fn is_default_only_closer(obj: &[String]) -> bool {
	obj == DEFAULT_ONLY_CLOSER
}

fn is_default_only_opener(obj: &[String]) -> bool {
	obj == DEFAULT_ONLY_OPENER
}

fn default_inline() -> Vec<String> {
	DEFAULT_INLINE.iter().map(ToString::to_string).collect()
}

fn default_only_closer() -> Vec<String> {
	DEFAULT_ONLY_CLOSER.iter().map(ToString::to_string).collect()
}

fn default_only_opener() -> Vec<String> {
	DEFAULT_ONLY_OPENER.iter().map(ToString::to_string).collect()
}

impl Default for Settings {
	fn default() -> Self {
		Self {
			inline: string_vec(&DEFAULT_INLINE),
			only_opener: string_vec(&DEFAULT_ONLY_OPENER),
			only_closer: string_vec(&DEFAULT_ONLY_CLOSER),
			post_processing_pipeline: vec![],
			plugins: vec![],
		}
	}
}

impl Settings {
	#[must_use]
	pub fn post_processing(&self) -> &[String] {
		&self.post_processing_pipeline
	}

	#[must_use]
	pub fn has_plugin(&self, plugin: &str) -> bool {
		self.plugins.iter().any(|x| x == plugin)
	}

	#[must_use]
	pub fn is_only_closer(&self, n: &str) -> bool {
		self.only_closer.iter().any(|x| x == n)
	}

	#[must_use]
	pub fn is_only_opener(&self, n: &str) -> bool {
		self.only_opener.iter().any(|x| x == n)
	}

	#[must_use]
	pub fn is_inline(&self, n: &str) -> bool {
		self.inline.iter().any(|x| x == n)
	}

	#[must_use]
	pub fn has_body(&self, n: &str) -> bool {
		!self.is_only_closer(n) && !self.is_only_opener(n)
	}
}

fn string_vec(s: &[&str]) -> Vec<String> {
	s.iter().map(ToString::to_string).collect()
}

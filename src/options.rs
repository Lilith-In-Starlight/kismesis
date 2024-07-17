//! Kismesis configuration data structure

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Settings {
	#[serde(default)]
	post_processing_pipeline: Vec<String>,
	#[serde(default)]
	inline: Vec<String>,
	#[serde(default)]
	only_closer: Vec<String>,
	#[serde(default)]
	only_opener: Vec<String>,
}

impl Default for Settings {
	fn default() -> Self {
		Self {
			inline: string_vec(&[
				"p", "b", "i", "strong", "italic", "sub", "sup", "h1", "h2", "h3", "h4", "h5",
				"h6", "a", "li", "title", "span", "emphasis", "code", "abbr", "bdo", "button",
				"cite", "dfn", "em", "img", "input", "kbd", "label", "map", "object", "output",
				"q", "script", "select", "small", "textarea", "time", "tt", "var", "pre",
			]),
			only_opener: string_vec(&["meta", "img", "link"]),
			only_closer: string_vec(&["br", "hr"]),
			post_processing_pipeline: vec![],
		}
	}
}

impl Settings {
	#[must_use]
	pub fn post_processing(&self) -> &[String] {
		&self.post_processing_pipeline
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

use super::parser::tag_stack::elements::ContentChild;

#[derive(Debug)]
pub struct CompilerOptions {
	pub in_previous_para: Vec<String>,
	pub inline: Vec<String>,
	pub only_closer: Vec<String>,
	pub only_opener: Vec<String>,
	pub no_params: Vec<String>,
	pub quote_mode: OutputQuotePreference,
	pub lambda_macros: Vec<String>,
	pub deprecated: Vec<String>,
}

#[derive(Debug)]
pub enum OutputQuotePreference {
	Default,
	ReverseDefault,
	PreferGiven,
	Prefer(QuotePreferences),
	Shorter(QuotePreferences),
}

#[derive(Debug)]
pub enum QuotePreferences {
	Single,
	Double,
}

impl CompilerOptions {
	pub fn default() -> Self{
		Self {
			in_previous_para: vec!["a", "b"].into_iter().map(String::from).collect(),
			inline: vec!["h1", "h2", "h3", "h4", "h5", "b", "p", "title", "a", "li", "span"].into_iter().map(String::from).collect(),
			only_closer: vec!["br"].into_iter().map(String::from).collect(),
			only_opener: vec!["meta", "base", "img"].into_iter().map(String::from).collect(),
			deprecated: vec!["marquee"].into_iter().map(String::from).collect(),
			no_params: vec!["br"].into_iter().map(String::from).collect(),
			quote_mode: OutputQuotePreference::Default,
			lambda_macros: vec![],
		}
	}

	pub fn for_templates() -> Self {
		Self {
			lambda_macros: vec!["content"].into_iter().map(String::from).collect(),
			..Self::default()
		}
	}

	pub fn is_parametric(&self, name: &str) -> bool {
		!self.no_params.iter().any(|x| x == name)
	}

	pub fn has_body(&self, name: &str) -> bool {
		!(self.only_closer.iter().any(|x| x == name) || self.only_opener.iter().any(|x| x == name))
	}

	pub fn is_inline(&self, name: &str) -> bool {
		self.inline.iter().any(|x| x == name)
	}

	pub fn is_in_previous_para(&self, name: &str) -> bool {
		self.in_previous_para.iter().any(|x| x == name)
	}

	pub fn is_only_closer(&self, name: &str) -> bool {
		self.only_closer.iter().any(|x| x == name)
	}

	pub fn is_only_opener(&self, name: &str) -> bool {
		self.only_opener.iter().any(|x| x == name)
	}

	pub fn is_one_sided(&self, name: &str) -> bool {
		!self.has_body(name)
	}

	pub fn is_deprecated(&self, name: &str) -> bool {
		self.deprecated.iter().any(|x| x == name)
	}

	pub fn is_lambda(&self, name: &str) -> bool {
		self.lambda_macros.iter().any(|x| x == name)
	}
}

impl From<String> for ContentChild {
	fn from(val: String) -> Self {
		Self::String(val)
	}
}

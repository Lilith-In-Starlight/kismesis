pub struct CompilerOptions {
	pub inline: Vec<String>,
	pub only_closer: Vec<String>,
	pub only_opener: Vec<String>,
	pub no_params: Vec<String>,
	pub quote_mode: OutputQuotePreference,
	pub lambda_macros: Vec<String>,
}

pub enum OutputQuotePreference {
	Default,
	ReverseDefault,
	PreferGiven,
	Prefer(QuotePreferences),
	Shorter(QuotePreferences),
}

pub enum QuotePreferences {
	Single,
	Double,
}

impl CompilerOptions {
	pub fn default() -> Self{
		Self {
			inline: vec!["h1", "h2", "h3", "h4", "b"].into_iter().map(|x| String::from(x)).collect(),
			only_closer: vec!["br"].into_iter().map(|x| String::from(x)).collect(),
			only_opener: vec!["meta", "base", "img"].into_iter().map(|x| String::from(x)).collect(),
			no_params: vec!["br"].into_iter().map(|x| String::from(x)).collect(),
			quote_mode: OutputQuotePreference::Default,
			lambda_macros: vec![],
		}
	}

	pub fn is_parametric(&self, name: &String) -> bool {
		!self.no_params.iter().any(|x| x == name)
	}

	pub fn has_body(&self, name: &String) -> bool {
		!(self.only_closer.iter().any(|x| x == name) || self.only_opener.iter().any(|x| x == name))
	}

	pub fn is_inline(&self, name: &String) -> bool {
		self.inline.iter().any(|x| x == name)
	}

	pub fn is_only_closer(&self, name: &String) -> bool {
		self.only_closer.iter().any(|x| x == name)
	}

	pub fn is_only_opener(&self, name: &String) -> bool {
		self.only_opener.iter().any(|x| x == name)
	}

	pub fn is_one_sided(&self, name: &String) -> bool {
		!self.has_body(name)
	}
}

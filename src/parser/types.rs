#[derive(Debug, Clone, PartialEq)]
pub(crate) enum StringParts {
    String(String),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Attribute {
    pub(crate) name: String,
    pub(crate) value: Vec<StringParts>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct HtmlTag {
	pub(crate) name: String,
	pub(crate) attributes: Vec<Attribute>,
	pub(crate) body: Vec<BodyTags>,
	pub(crate) subtags: Vec<HtmlTag>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BodyTags {
	HtmlTag(HtmlTag),
	Variable(String),
}

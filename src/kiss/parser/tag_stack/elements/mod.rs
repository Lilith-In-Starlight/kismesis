use crate::kiss::parser::TokenScanner;

#[derive(Debug, Clone)]
pub struct ContentTag {
	pub name: String,
	pub params: Vec<Param>,
	pub children: Vec<ContentChild>,
	pub line: usize,
	pub pos_in_line: usize,
}

#[derive(Debug, Clone)]
pub struct Macro {
	pub name: String,
	pub args: Vec<MacroArg>,
	pub children: Vec<ContentChild>,
	pub line: usize,
	pub pos_in_line: usize,
}

#[derive(Debug, Clone)]
pub struct Variable {
	pub name: String,
	pub line: usize,
	pub pos_in_line: usize,
}

impl From<Variable> for ContentChild {
	fn from(val: Variable) -> Self {
		Self::Variable(val)
	}
}

#[derive(Debug, Clone)]
pub enum Tag {
	MacroCall(Macro),
	MacroDef(Macro),
	ContentTag(ContentTag),
}

#[derive(Debug, Clone)]
pub enum Element {
	MacroCall(Macro),
	MacroDef(Macro),
	ContentTag(ContentTag),
	String(String),
	Variable(Variable),
}

#[derive(Debug, Clone)]
pub enum ContentChild {
	MacroCall(Macro),
	ContentTag(ContentTag),
	String(String),
	Variable(Variable),
}

pub enum ContentChildTag {
	MacroCall(Macro),
	ContentTag(ContentTag),
}

impl From<Macro> for ContentChildTag {
	fn from(val: Macro) -> Self {
		Self::MacroCall(val)
	}
}

impl From<ContentChildTag> for Tag {
	fn from(val: ContentChildTag) -> Self {
		match val {
			ContentChildTag::MacroCall(m) => Self::MacroCall(m),
			ContentChildTag::ContentTag(m) => Self::ContentTag(m),
		}
	}
}

impl From<ContentChildTag> for ContentChild {
	fn from(val: ContentChildTag) -> Self {
		match val {
			ContentChildTag::MacroCall(m) => Self::MacroCall(m),
			ContentChildTag::ContentTag(m) => Self::ContentTag(m),
		}
	}
}
impl From<ContentTag> for ContentChildTag {
	fn from(val: ContentTag) -> Self {
		Self::ContentTag(val)
	}
}
impl From<ContentTag> for ContentChild {
	fn from(val: ContentTag) -> Self {
		Self::ContentTag(val)
	}
}

impl MacroCallHolder for ContentChildTag {
	fn to_call(m: Macro) -> Self {
		Self::MacroCall(m)
	}
}

pub trait MacroDefHolder where Self: MacroCallHolder {
	fn to_def(m: Macro) -> Self;
}

pub trait MacroCallHolder {
	fn to_call(m: Macro) -> Self;
}

impl MacroDefHolder for Element {
	fn to_def(m: Macro) -> Self {
		Element::MacroDef(m)
	}
}

impl MacroCallHolder for Element {
	fn to_call(m: Macro) -> Self {
		Element::MacroCall(m)
	}
}

impl MacroDefHolder for Tag {
	fn to_def(m: Macro) -> Self {
		Self::MacroDef(m)
	}
}

impl MacroCallHolder for Tag {
	fn to_call(m: Macro) -> Self {
		Self::MacroCall(m)
	}
}

impl MacroCallHolder for ContentChild {
	fn to_call(m: Macro) -> Self {
		Self::MacroCall(m)
	}
}

impl From<ContentTag> for Element {
	fn from(val: ContentTag) -> Self {
		Self::ContentTag(val)
	}
}
impl From<ContentTag> for Tag {
	fn from(val: ContentTag) -> Self {
		Self::ContentTag(val)
	}
}

impl ContentTag {
	pub fn new(state: &TokenScanner) -> Self {
		Self {
			name: String::new(),
			params: Vec::new(),
			children: Vec::new(),
			line: state.current_line,
			pos_in_line: state.token_in_line,
		}
	}
	pub fn add_child<T: Into<ContentChild>>(&mut self, child: T) {
		self.children.push(child.into());
	}
	pub fn add_param(&mut self, param: Param) {
		self.params.push(param);
	}
}

impl Macro {
	pub fn new_def<T: MacroDefHolder>(state: &TokenScanner) -> T {
		T::to_def(Self {
			name: String::new(),
			args: Vec::new(),
			children: Vec::new(),
			line: state.current_line,
			pos_in_line: state.token_in_line,
		})
	}
	pub fn new_call<T: MacroCallHolder>(state: &TokenScanner) -> T {
		T::to_call(Self {
			name: String::new(),
			args: Vec::new(),
			children: Vec::new(),
			line: state.current_line,
			pos_in_line: state.token_in_line,
		})
	}
	pub fn add_child<T: Into<ContentChild>>(&mut self, child: T) {
		self.children.push(child.into());
	}
	pub fn add_arg(&mut self, arg: MacroArg) {
		self.args.push(arg);
	}
}

#[derive(Debug, Clone)]
pub struct Param {
	pub name: String,
	pub value: String,
}

impl Param {
	pub fn new() -> Self {
		Self {
			name: String::new(),
			value: String::new(),
		}
	}
}

impl MacroArg {
	pub fn new() -> Self {
		Self {
			name: String::new(),
			value: None,
		}
	}
}

#[derive(Debug, Clone)]
pub struct MacroArg {
	pub name: String,
	pub value: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Constant {
	pub name: String,
	pub value: String,
}

impl Default for Param {
	fn default() -> Self {
		Self::new()
	}
}

impl Default for MacroArg {
	fn default() -> Self {
		Self::new()
	}
}

pub trait OnlyTags {
	fn get_children(&self) -> &Vec<ContentChild>;
	fn get_children_mut(&mut self) -> &mut Vec<ContentChild>;
	fn set_name(&mut self, new_name: &str);
	fn get_name(&self) -> &str;
}

impl OnlyTags for Tag {
	fn get_children(&self) -> &Vec<ContentChild> {
		match self {
			Self::MacroCall(m) => &m.children,
			Self::MacroDef(m) => &m.children,
			Self::ContentTag(c) => &c.children,
		}
	}

	fn get_children_mut(&mut self) -> &mut Vec<ContentChild> {
		match self {
			Self::MacroCall(m) => &mut m.children,
			Self::MacroDef(m) => &mut m.children,
			Self::ContentTag(c) => &mut c.children,
		}
	}

	fn set_name(&mut self, new_name: &str) {
		match self {
			Self::MacroCall(m) | Self::MacroDef(m) => m.name = new_name.to_string(),
			Self::ContentTag(c) => c.name = new_name.to_string(),
		}
	}

	fn get_name(&self) -> &str {
		match self {
			Self::MacroCall(m) | Self::MacroDef(m) => &m.name,
			Self::ContentTag(c) => &c.name,
		}
	}
}

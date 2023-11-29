use crate::{errors::UnrecoverableError, kiss::parser::TokenScanner};

#[derive(Debug)]
pub enum BodyElems {
	ContentTag {
		name: String,
		params: Vec<Param>,
		children: Vec<BodyElems>,
		line: usize,
		pos_in_line: usize,
	},
	MacroCall {
		name: String,
		args: Vec<MacroArg>,
		children: Vec<BodyElems>,
		line: usize,
		pos_in_line: usize,
	},
	MacroDef {
		name: String,
		args: Vec<MacroArg>,
		children: Vec<BodyElems>,
		line: usize,
		pos_in_line: usize,
	},
	String(String),
	ValueTag {
		name: String,
		line: usize,
		pos_in_line: usize,
	},
}

impl BodyElems {
	pub fn new_content_tag(scanner: &TokenScanner) -> Self {
		Self::ContentTag { name: String::new(), params: vec![], children: vec![] , line: scanner.current_line, pos_in_line: scanner.token_in_line}
	}

	pub fn new_macro_def(scanner: &TokenScanner) -> Self {
		Self::MacroDef { name: String::new(), args: vec![], children: vec![], line: scanner.current_line, pos_in_line: scanner.token_in_line }
	}

	pub fn new_macro_call(scanner: &TokenScanner) -> Self {
		Self::MacroCall { name: String::new(), args: vec![], children: vec![], line: scanner.current_line, pos_in_line: scanner.token_in_line }
	}

	pub fn get_name_mut(&mut self) -> Option<&mut String> {
		match self {
			Self::ContentTag { name, .. } | Self::MacroCall { name, .. } | Self::MacroDef { name, .. } => Some(name),
			Self::ValueTag {..} | Self::String(_) => None,
		}
	}

	pub fn get_name(&self) -> Option<&String> {
		match self {
			Self::ContentTag { name, .. } | Self::MacroCall { name, .. } | Self::MacroDef {name, ..} => Some(name),
			Self::ValueTag {..} | Self::String(_) => None,
		}
	}

	pub fn add_param(&mut self, param: Param) -> Result<(), UnrecoverableError> {
		match self {
			Self::ContentTag { params, ..} => params.push(param),
			Self::MacroCall { .. } | Self::MacroDef { .. } => return Err(UnrecoverableError::AddedParamToMacro),
			_ => return Err(UnrecoverableError::ImpossibleNotTag),
		}
		Ok(())
	}

	pub fn add_arg(&mut self, arg: MacroArg) -> Result<(), UnrecoverableError> {
		match self {
			Self::ContentTag { .. } => return Err(UnrecoverableError::AddedParamToMacro),
			Self::MacroCall { args, .. } | Self::MacroDef { args, .. } => args.push(arg),
			_ => return Err(UnrecoverableError::ImpossibleNotTag),
		}
		Ok(())
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

#[derive(Debug)]
pub struct MacroDefTag {
	pub name: String,
	pub args: Vec<MacroArg>,
	pub children: Vec<BodyElems>,
}

#[derive(Debug)]
pub struct Constant {
	pub name: String,
	pub value: String,
}

use crate::kiss::ast::lexer::Token;

use super::compiler_options::CompilerOptions;

pub mod lexer;


#[derive(Debug)]
pub enum BodyElems {
	ContentTag {
		name: String,
		params: Vec<Param>,
		children: Vec<BodyElems>,
	},
	MacroCall {
		name: String,
		args: Vec<MacroArg>,
		children: Vec<BodyElems>,
	},
	String(String),
	ValueTag(String),
}

impl BodyElems {
	fn new_content_tag() -> Self {
		Self::ContentTag { name: String::new(), params: vec![], children: vec![] }
	}

	fn get_name_mut(&mut self) -> Option<&mut String> {
		match self {
			Self::ContentTag { name, .. } | Self::MacroCall { name, .. } => Some(name),
			_ => None,
		}
	}

	fn get_name(&self) -> Option<&String> {
		match self {
			Self::ContentTag { name, .. } | Self::MacroCall { name, .. } => Some(name),
			_ => None,
		}
	}

	fn add_param(&mut self, param: Param) -> Result<(), TagStackError> {
		match self {
			Self::ContentTag { params, ..} => params.push(param),
			_ => return Err(TagStackError::NonParametricTopTag),
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
	fn new() -> Self {
		Self {
			name: String::new(),
			value: String::new(),
		}
	}
}
#[derive(Debug)]
struct MacroArg {
	name: String,
	value: Option<String>,
}

#[derive(Debug)]
pub struct MacroDefTag {
	name: String,
	args: Vec<MacroArg>,
	children: Vec<BodyElems>,
}

#[derive(Debug)]
pub struct Constant {
	name: String,
	value: String,
}

#[derive(Debug)]
pub struct ParsedFile {
	pub macros: Vec<MacroDefTag>,
	pub body: Vec<BodyElems>,
	pub consts: Vec<Constant>,
}

impl ParsedFile {
	fn new() -> Self {
		Self {
			macros: vec![],
			body: vec![],
			consts: vec![],
		}
	}
}

enum States {
	ExpectAnyOpener, //< or #
	ExpectTagName,
	ExpectParamName,
	ExpectParamEquals,
	ExpectParamValue(ParamValueType),
	ExpectBody,
}

enum ParamValueType {
	Word,
	MultiWord(char)
}

pub enum TagStackError {
	WasEmpty,
	HadOneTag,
	NonMergeableTopTag,
	NonMergeableTopTags,
	NonParametricTopTag,
	NonBody,
	UnusableLastTag,
}

impl From<TagStackError> for &'static str {
    fn from(value: TagStackError) -> Self {
        match value {
			TagStackError::WasEmpty => "Stack was empty",
			TagStackError::HadOneTag => "Stack had only one tag",
			TagStackError::NonMergeableTopTag => "Tried to merge an element which wasn't mergeable",
			TagStackError::NonMergeableTopTags => "Tried to merge a tag with an element which wasn't mergeable",
			TagStackError::NonParametricTopTag => "Tried to add a parameter to an element which has no parameters",
			TagStackError::NonBody => "Tried to get the body of a bodyless tag",
			TagStackError::UnusableLastTag => "Last tag of the stack was not usable",
		}
    }
}

pub enum ParserStateError {
	UndefinedTransition,
	SetNameEmptyStack,
	RenameUnnamable,
}

impl From<ParserStateError> for &'static str {
    fn from(value: ParserStateError) -> Self {
        match value {
			ParserStateError::UndefinedTransition => "Undefined transition",
			ParserStateError::SetNameEmptyStack => "Attempted to set name on the top tag of an empty stack",
			ParserStateError::RenameUnnamable => "Attempted to set name on the top tag of a stack, but the top tag was unnamable",
		}
    }
}

struct Parser {
	state: States,
	tag_stack: TagStack,
	current_param: Param,
	file: ParsedFile,
	escape: bool,
}

impl Parser {
	fn new() -> Self { 
		Self {
			state: States::ExpectAnyOpener,
			tag_stack: TagStack::new(),
			current_param: Param::new(),
			file: ParsedFile::new(),
			escape: false,
		}
	}

	fn add_new_content_tag(&mut self) { self.tag_stack.add_new_content_tag() }

	fn set_top_tag_name(&mut self, to: String) -> Result<(), ParserStateError> {
		match self.tag_stack.last_mut() {
			None => return Err(ParserStateError::SetNameEmptyStack),
			Some(x) => match x.get_name_mut() {
				Some(previous_name) => *previous_name = to,
				None => return Err(ParserStateError::RenameUnnamable),
			},
		}
		Ok(())
	}

	fn change_state_to(&mut self, to: States) -> Result<(), ParserStateError>{
		match (&self.state, &to) {
			(States::ExpectAnyOpener, States::ExpectTagName) => (),
			(States::ExpectTagName, States::ExpectParamName) => self.current_param = Param::new(),
			(States::ExpectParamName, States::ExpectParamEquals) => (),
			(States::ExpectParamEquals, States::ExpectParamValue(_)) => (),
			(States::ExpectParamValue(_), States::ExpectParamName) => self.current_param = Param::new(),
			(States::ExpectParamValue(ParamValueType::Word), States::ExpectParamValue(_)) => (),
			(States::ExpectParamName, States::ExpectBody) => (),
			(States::ExpectTagName, States::ExpectBody) => (),
			(States::ExpectBody, States::ExpectTagName) => (),
			(States::ExpectBody, States::ExpectAnyOpener) => (),
			(States::ExpectParamName, States::ExpectAnyOpener) => (),
			_ => return Err(ParserStateError::UndefinedTransition)
		}
		self.escape = false;
		self.state = to;
		Ok(())
	}

	fn merge_or(&mut self) -> Result<(), TagStackError> {
		let Err(e) = self.tag_stack.merge() else { return Ok(()) };
		match e {
			TagStackError::HadOneTag => match self.tag_stack.pop() {
				Some(x) => {
					match x {
						BodyElems::ContentTag {..} => {
							self.file.body.push(x);
							match self.change_state_to(States::ExpectAnyOpener) {
								Err(_) => return Err(TagStackError::UnusableLastTag),
								_ => return Ok(()),
							}
						},
						BodyElems::MacroCall {..} => todo!("Implement adding tags to the macro list"),
						_ => return Err(TagStackError::UnusableLastTag),
					}
				},
				None => return Err(TagStackError::WasEmpty),
			}
			_ => return Err(e),
		}
		Ok(())
	}

	fn finish_current_param(&mut self) -> Result<(), TagStackError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(TagStackError::WasEmpty) };
		let r = top_tag.add_param(self.current_param.clone());
		self.current_param = Param::new();
		r
	}

	fn get_top_tag_children_mut(&mut self) -> Result<&mut Vec<BodyElems>, TagStackError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(TagStackError::WasEmpty) };
		match top_tag {
			BodyElems::ContentTag { children, .. } | BodyElems::MacroCall { children, .. } => Ok(children),
			_ => Err(TagStackError::NonBody),
		}
		
	}

	fn get_top_tag_name(&self) -> Option<&String> {
		let Some(top_tag) = self.tag_stack.last() else { return None };
		top_tag.get_name()
	}
}


#[derive(Debug)]
struct TagStack {
	content: Vec<BodyElems>,
}

impl TagStack {
	fn new() -> Self {
		Self { content: vec![] }
	}
	fn add_new_content_tag(&mut self) { self.content.push(BodyElems::new_content_tag()) }

	fn last_mut(&mut self) -> Option<&mut BodyElems> { self.content.last_mut() }
	fn last(&self) -> Option<&BodyElems> { self.content.last() }

	fn pop(&mut self) -> Option<BodyElems> { self.content.pop() }

	fn merge(&mut self) -> Result<(), TagStackError> {
		if self.content.len() == 1 { return Err(TagStackError::HadOneTag) }

		let current_tag = match self.content.pop() {
			Some(x) => {
				match x {
					BodyElems::ContentTag {..} => x,
					BodyElems::MacroCall {..} => x,
					_ => return Err(TagStackError::NonMergeableTopTag)
				}
			},
			None => return Err(TagStackError::WasEmpty)
	
		};
	
		match self.last_mut() {
			Some(x) => {
				match x {
					BodyElems::ContentTag { children , ..} => children.push(current_tag),
					BodyElems::MacroCall { children , ..} => children.push(current_tag),
					_ => return Err(TagStackError::NonMergeableTopTags)
				}
			},
			None => return Err(TagStackError::HadOneTag),
		};
		Ok(())
	}
}

pub fn get_ast(s: &str, options: CompilerOptions) -> Result<ParsedFile, &'static str> {
	let tokens = match lexer::tokenize(&s.replace('\r', "")) {
		Ok(x) => x,
		Err(x) => return Err(x),
	};

	let mut parser = Parser::new();
	;
	for token in tokens {
		match parser.state {
			States::ExpectAnyOpener => {
				match token {
					lexer::Token::OpenTag(_) => {
						parser.add_new_content_tag();
						parser.change_state_to(States::ExpectTagName)?;
					}
					lexer::Token::Hashtag(_) => {
						todo!("Handle variable definitions")
					}
					lexer::Token::Space(_) | lexer::Token::Newline(_) | lexer::Token::Indent(_) => continue,
					_ => return Err("Expected new tag or variable declaration")
				}
			}
			States::ExpectTagName => {
				match token {
					lexer::Token::Word(word) => {
						if word == "macro" {
							todo!("Allow macro declaration")
						} else {
							parser.set_top_tag_name(word)?;
							parser.change_state_to(States::ExpectParamName)?;
						}
					}
					lexer::Token::MacroName(word) => todo!("Macro definitions"),
					_ => return Err("Expected a name for the tag")
				}
			}
			States::ExpectParamName => {
				match token {
					lexer::Token::Newline(_) | lexer::Token::Bar(_) => {
						let Some(top_tag_name) = parser.get_top_tag_name() else { return Err("No top tag name") };
						if options.has_body(top_tag_name) {
							parser.change_state_to(States::ExpectBody)?
						} else {
							return Err("Tried to give body to a tag which takes no body")
						}
					},
					lexer::Token::Word(name) => {
						let Some(top_tag_name) = parser.get_top_tag_name() else { return Err("No top tag name") };
						if options.is_parametric(top_tag_name) {
							parser.current_param.name = name;
							parser.change_state_to(States::ExpectParamEquals)?;
						} else {
							return Err("Tried to give parameters to anon_parametric tag")
						}
					},
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					lexer::Token::CloseTag(_) => parser.merge_or()?,
					_ => return Err("Expected a tag body or a parameter name")
				}
			}
			States::ExpectParamEquals => {
				match token {
					lexer::Token::Equals(_) => parser.change_state_to(States::ExpectParamValue(ParamValueType::Word))?,
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					_ => return Err("Expected a tag body or a parameter name")
				}
			}
			States::ExpectParamValue(ref kind) => {
				match kind {
					ParamValueType::Word => {
						match token {
							lexer::Token::Quote(opener) => parser.change_state_to(States::ExpectParamValue(ParamValueType::MultiWord(opener)))?,
							lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
							lexer::Token::Word(word) => {
								parser.current_param.value = word;
								parser.finish_current_param()?;
								parser.change_state_to(States::ExpectParamName)?;
							}
							_ => return Err("Expected a a word or a quotation mark")
						}
					}
					ParamValueType::MultiWord(expecting_closer) => {
						match token {
							lexer::Token::Quote(obtained_closer) if obtained_closer == *expecting_closer && !parser.escape => {
								parser.finish_current_param()?;
								parser.change_state_to(States::ExpectParamName)?;
							}
							lexer::Token::Newline(_) => return Err("Expected a continuation to the string or a quotation mark, found newline (multiline parameters are not supported yet)"),
							lexer::Token::Escape(_) if !parser.escape => parser.escape = true,
							_ => {
								token.push_to_string(&mut parser.current_param.value);
								parser.escape = false;
							},
						}
					}
				}
			}
			States::ExpectBody => {
				match token {
					lexer::Token::OpenTag(_) if !parser.escape => {
						parser.add_new_content_tag();
						parser.change_state_to(States::ExpectTagName)?;
					}
					lexer::Token::CloseTag(_) if !parser.escape => parser.merge_or()?,
					lexer::Token::Escape(_) if !parser.escape => { parser.escape = true },
					_ => {
						parser.escape = false;
						let top_tag_body = parser.get_top_tag_children_mut()?;					
						match top_tag_body.last_mut() {
							None => match token {
								lexer::Token::Space(_) | lexer::Token::Newline(_) | Token::Indent(_) => continue,
								_ => {
									let mut new = String::new();
									token.push_to_string(&mut new);
									top_tag_body.push(BodyElems::String(new));
								}
							},
							Some(last_body_child) => match last_body_child {
								BodyElems::String(text) => token.push_to_string(text),
								_ => match token {
									lexer::Token::Space(_) | lexer::Token::Newline(_) | Token::Indent(_) => continue,
									_ => {
										let mut new = String::new();
										token.push_to_string(&mut new);
										top_tag_body.push(BodyElems::String(new));
									}
								},
							},
						};
					},
				}
			}
		}
	}
	Ok(parser.file)
}
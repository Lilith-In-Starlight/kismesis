pub(crate) mod tag_stack;

use crate::kiss::parser::lexer::Token;

use self::tag_stack::elements::{Param, BodyElems, Constant, MacroDefTag};

use super::{compiler_options::CompilerOptions, lexer};
use tag_stack::TagStack;
use tag_stack::errors::TagStackError;


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


pub fn get_ast(s: &str, options: CompilerOptions) -> Result<ParsedFile, &'static str> {
	let tokens = lexer::tokenize(&s.replace('\r', ""));

	let mut parser = Parser::new();
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
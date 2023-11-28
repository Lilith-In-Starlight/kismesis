pub(crate) mod tag_stack;

use std::sync::Arc;

use crate::kiss::parser::lexer::Token;
use crate::errors::{KismesisError, UnrecoverableError, ErrorState, CompilerErrorReport};

use self::tag_stack::elements::{Param, BodyElems, Constant, MacroArg};

use super::{compiler_options::CompilerOptions, lexer};
use tag_stack::TagStack;
use tag_stack::errors::TagStackError;


#[derive(Debug)]
pub struct ParsedFile {
	pub macros: MacroArray,
	pub body: Vec<BodyElems>,
	pub consts: Vec<Constant>,
}

impl ParsedFile {
	fn new() -> Self {
		Self {
			macros: MacroArray::new(),
			body: vec![],
			consts: vec![],
		}
	}
}

#[derive(Debug, Clone)]
pub enum AfterEquals {
	Param,
	Arg
}

#[derive(Debug, Clone)]
pub enum States {
	ExpectAnyOpener, //< or #
	ExpectTagName,
	ExpectParamName,
	ExpectEquals(AfterEquals),
	ExpectParamValue(ParamValueType, AfterEquals),
	ExpectBody,
	ExpectMacroName,
	ExpectArgNameOrBody,
	ExpectVariable(Arc<States>),
}

#[derive(Debug, Clone)]
pub enum ParamValueType {
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

#[derive(Debug)]
pub struct MacroArray {
	content: Vec<BodyElems>,
}

impl MacroArray {
	pub fn new() -> Self {
		Self {
			content: vec![],
		}
	}
	pub fn push(&mut self, el: BodyElems) -> Result<(), UnrecoverableError> {
		match el {
			BodyElems::MacroDef { .. } => self.content.push(el),
			_ => return Err(UnrecoverableError::NonMacroIntoMacroArray),
		}
		Ok(())
	}

	pub fn last_mut(&mut self) -> Option<&mut BodyElems>{
		self.content.last_mut()
	}

	pub fn get_content(&self) -> &Vec<BodyElems> {
		&self.content
	}
}

struct Parser {
	state: States,
	tag_stack: TagStack,
	macros: MacroArray,
	current_param: Param,
	current_arg: MacroArg,
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
			macros: MacroArray::new(),
			current_arg: MacroArg::new(),
		}
	}

	fn add_new_content_tag(&mut self) { self.tag_stack.add_new_content_tag() }
	fn add_new_macro_call(&mut self) { self.tag_stack.add_new_macro_call() }
	fn add_new_macro_def(&mut self) -> Result<(), KismesisError> { Ok(self.tag_stack.add_new_macro_def()?) }

	fn set_top_tag_name(&mut self, to: String) -> Result<(), UnrecoverableError> {
		match self.tag_stack.last_mut() {
			None => return Err(UnrecoverableError::ImpossibleEmpty),
			Some(x) => match x.get_name_mut() {
				Some(previous_name) => *previous_name = to,
				None => return Err(UnrecoverableError::TagStackHadNonTag),
			},
		}
		Ok(())
	}

	fn change_state_to(&mut self, to: States) {
		self.escape = false;
		self.state = to;
	}

	fn merge_or(&mut self) -> Result<(), KismesisError> {
		let Err(e) = self.tag_stack.merge() else { return Ok(()) };
		match e {
			TagStackError::HadOneTag => match self.tag_stack.pop() {
				Some(x) => {
					match x {
						BodyElems::ContentTag {..} => {
							self.file.body.push(x);
							self.change_state_to(States::ExpectAnyOpener);
							Ok(())
						},
						BodyElems::MacroDef { .. } => {
							self.file.macros.push(x)?;
							self.change_state_to(States::ExpectAnyOpener);
							Ok(())
						}
						BodyElems::MacroCall {..} => {
							self.file.body.push(x);
							self.change_state_to(States::ExpectArgNameOrBody);
							Ok(())
						},
						_ => return Err(KismesisError::UnrecoverableError(UnrecoverableError::TagStackHadNonTag)),
					}
				},
				None => return Err(KismesisError::ClosedTooManyTags),
			}
			x => return Err(x.into()),
		}
	}

	fn finish_current_param(&mut self) -> Result<(), UnrecoverableError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(UnrecoverableError::ImpossibleEmpty) };
		top_tag.add_param(self.current_param.clone())?;
		self.current_param = Param::new();
		Ok(())
	}

	fn finish_current_arg(&mut self) -> Result<(), UnrecoverableError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(UnrecoverableError::ImpossibleEmpty) };
		top_tag.add_arg(self.current_arg.clone())?;
		self.current_arg = MacroArg::new();
		Ok(())
	}

	fn get_top_tag_children_mut(&mut self) -> Result<&mut Vec<BodyElems>, UnrecoverableError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(UnrecoverableError::ImpossibleEmpty) };
		match top_tag {
			BodyElems::ContentTag { children, .. } | BodyElems::MacroCall { children, .. } | BodyElems::MacroDef { children, .. }=> Ok(children),
			_ => Err(UnrecoverableError::ImpossibleNotTag),
		}
		
	}

	fn get_top_tag_name(&self) -> Option<&String> {
		let Some(top_tag) = self.tag_stack.last() else { return None };
		top_tag.get_name()
	}
}

pub struct TokenScanner {
	pub tokens: Vec<Token>,
	position: Option<usize>,
	current_line: usize,
	token_in_line: usize,
}

impl TokenScanner {
	pub fn new(token_vec: Vec<Token>) -> Self {
		Self {
			position: None,
			tokens: token_vec,
			current_line: 0,
			token_in_line: 0,
		}
	}
	pub fn get_current_line_number(&self) -> usize {self.current_line}
	pub fn next(&mut self) -> Option<&Token> {
		let c = match self.position {
			None => {
				self.position = Some(0);
				self.token_in_line = 0;
				self.tokens.get(0)
			}
			Some(x) => {
				self.position = Some(x + 1);
				match self.tokens.get(x) {
					Some(previous_token) => {
						if let lexer::Token::Newline(_) = previous_token {
							self.current_line += 1;
							self.token_in_line = 0;
						} else {
							self.token_in_line += 1;
						}
 					},
					None => self.token_in_line = 0,
				}
				self.tokens.get(x + 1)
			}
		};
		c
	}

	pub fn get_position(&self) -> usize {
		match self.position { None => 0, Some(x) => x }
	}

	pub fn get_position_in_line(&self) -> usize { self.token_in_line }

	pub fn get_all_current_line(&self) -> Vec<(&Token, usize)> {
		let mut position = match self.position {None=>0,Some(x)=>x};
		let mut output = Vec::new();
		while let Some(token) = self.tokens.get(position) {
			if position == 0 { break }
			if let lexer::Token::Newline(_) = token { 
				position += 1;
				break
			}
			position -= 1;
		}
		
		while let Some(token) = self.tokens.get(position) {
			if let lexer::Token::Newline(_) = token {}
			else { output.push((token, position)) }
			position += 1;
		}

		output
	}
	pub fn get_lines_slices(&self) -> Vec<&[lexer::Token]> {
		self.tokens.split_inclusive(|x| if let lexer::Token::Newline(_) = x {true} else {false}).collect()
	}
}

pub fn get_ast(s: &str, options: CompilerOptions) -> Result<ParsedFile, CompilerErrorReport<KismesisError>> {
	let tokens = lexer::tokenize(&s.replace('\r', ""));

	let mut parser = Parser::new();
	let mut token_idx: usize = 0;
	let mut token_scanner = TokenScanner::new(tokens);
	let mut recovered_errors: Vec<ErrorState<KismesisError>> = Vec::new();
	let iterate = | | -> Result<ParsedFile, KismesisError> {
		while let Some(token) = token_scanner.next() {
			match parser.state {
				States::ExpectAnyOpener => {
					match token {
						lexer::Token::OpenTag(_) => {
							parser.change_state_to(States::ExpectTagName);
						}
						lexer::Token::Hashtag(_) => {
							parser.change_state_to(States::ExpectVariable(Arc::new(parser.state.clone())));
						}
						lexer::Token::Space(_) | lexer::Token::Newline(_) | lexer::Token::Indent(_) => continue,
						lexer::Token::CloseTag(_) => recovered_errors.push(KismesisError::ClosedTooManyTags.state(&token_scanner)),
						_ => recovered_errors.push(KismesisError::ExpectedAnyOpener.state(&token_scanner)),
					}
				}
				States::ExpectTagName => {
					match token {
						lexer::Token::Word(word) => {
							if word == "macro" {
								parser.add_new_macro_def()?;
								parser.change_state_to(States::ExpectMacroName);
							} else {
								parser.add_new_content_tag();
								parser.set_top_tag_name(word.clone())?;
								parser.change_state_to(States::ExpectParamName);
							}
						},
						lexer::Token::MacroName(word) => {
							parser.add_new_macro_call();
							parser.set_top_tag_name(word.trim_end_matches('!').to_string())?;
							parser.change_state_to(States::ExpectArgNameOrBody);
						},
						_ => {
							recovered_errors.push(KismesisError::ExpectedTagName.state(&token_scanner));
							parser.change_state_to(States::ExpectParamName);
						},
					}
				}
				States::ExpectParamName => {
					match token {
						lexer::Token::Newline(_) | lexer::Token::Bar(_) => {
							let Some(top_tag_name) = parser.get_top_tag_name() else { return Err(KismesisError::UnrecoverableError(UnrecoverableError::ImpossibleEmpty)) };
							if options.has_body(top_tag_name) {
								parser.change_state_to(States::ExpectBody)
							} else {
								return Err(KismesisError::UnrecoverableError(UnrecoverableError::TagStackHadNonTag))
							}
						},
						lexer::Token::Word(name) => {
							let Some(top_tag_name) = parser.get_top_tag_name() else { return Err(KismesisError::UnrecoverableError(UnrecoverableError::ImpossibleEmpty)) };
							if options.is_parametric(top_tag_name) {
								parser.current_param.name = name.clone();
							} else {
								recovered_errors.push(KismesisError::ParametersNotAllowed.state(&token_scanner));
							}
							parser.change_state_to(States::ExpectEquals(AfterEquals::Param));
						},
						lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
						lexer::Token::CloseTag(_) => parser.merge_or()?,
						_ => return Err(KismesisError::ExpectedTagBodyOrParam)
					}
				}
				States::ExpectEquals(ref what) => {
					match what {
						AfterEquals::Param => match token {
							lexer::Token::Equals(_) => parser.change_state_to(States::ExpectParamValue(ParamValueType::Word, what.clone())),
							lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
							_ => return Err(KismesisError::ExpectedCharacter { character: '=' })
						}
						AfterEquals::Arg => match token {
							lexer::Token::Equals(_) => parser.change_state_to(States::ExpectParamValue(ParamValueType::Word, what.clone())),
							lexer::Token::Space(_) | lexer::Token::Indent(_) => {
								parser.finish_current_arg()?;
								parser.change_state_to(States::ExpectArgNameOrBody);
							},
							lexer::Token::CloseTag(_) => {
								parser.finish_current_arg()?;
								parser.merge_or()?;
							}
							_ => return Err(KismesisError::ExpectedCharacter { character: '=' })
						}
					}
				}
				States::ExpectParamValue(ref wordicity, ref kind) => {
					match wordicity {
						ParamValueType::Word => {
							match token {
								lexer::Token::Hashtag(_) => parser.change_state_to(States::ExpectVariable(Arc::new(parser.state.clone()))),
								lexer::Token::Quote(opener) => parser.change_state_to(States::ExpectParamValue(ParamValueType::MultiWord(*opener), kind.clone())),
								lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
								lexer::Token::Word(word) => match kind {
									AfterEquals::Param => {
										parser.current_param.value = word.clone();
										parser.finish_current_param()?;
										parser.change_state_to(States::ExpectParamName);
									},
									AfterEquals::Arg => {
										parser.current_arg.value = Some(word.clone());
										parser.finish_current_arg()?;
										parser.change_state_to(States::ExpectArgNameOrBody);
									}
								}
								_ => {
									match kind {
										AfterEquals::Param => {
											recovered_errors.push(KismesisError::ExpectedParameterValue.state(&token_scanner));
											parser.change_state_to(States::ExpectParamName);
										}
										AfterEquals::Arg => {
											recovered_errors.push(KismesisError::ExpectedArgValue.state(&token_scanner));
											parser.change_state_to(States::ExpectArgNameOrBody);
										}
									}
								}
							}
						}
						ParamValueType::MultiWord(expecting_closer) => {
							match token {
								lexer::Token::Hashtag(_) if !parser.escape => parser.change_state_to(States::ExpectVariable(Arc::new(parser.state.clone()))),
								lexer::Token::Quote(obtained_closer) if obtained_closer == expecting_closer && !parser.escape => {
									match kind {
										AfterEquals::Param => {
											parser.finish_current_param()?;
											parser.change_state_to(States::ExpectParamName);
										},
										AfterEquals::Arg => {
											parser.finish_current_arg()?;
											parser.change_state_to(States::ExpectArgNameOrBody);
										}
									}
								}
								lexer::Token::Newline(_) => recovered_errors.push(KismesisError::NewlineInParameter.state(&token_scanner)),
								lexer::Token::Escape(_) if !parser.escape => parser.escape = true,
								_ => {
									match kind {
										AfterEquals::Param => {
											token.push_to_string(&mut parser.current_param.value);
										}
										AfterEquals::Arg => match parser.current_arg.value {
											Some(ref mut x) => token.push_to_string(x),
											None => parser.current_arg.value = Some(token.get_as_string()),
										}
									}
									parser.escape = false;
								},
							}
						},
					}
				}
				States::ExpectBody => {
					match token {
						lexer::Token::Hashtag(_) if !parser.escape => parser.change_state_to(States::ExpectVariable(Arc::new(parser.state.clone()))),
						lexer::Token::OpenTag(_) if !parser.escape => {
							parser.change_state_to(States::ExpectTagName);
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
				States::ExpectMacroName => {
					match token {
						lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
						lexer::Token::Word(name) | lexer::Token::MacroName(name) => {
							parser.set_top_tag_name(name.clone())?;
							parser.change_state_to(States::ExpectArgNameOrBody);
						}
						_ => {
							recovered_errors.push(KismesisError::ExpectedMacroName.state(&token_scanner));
							parser.change_state_to(States::ExpectArgNameOrBody);
						}
					}
				}
				States::ExpectArgNameOrBody => {
					match token {
						lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
						lexer::Token::Word(name) => {
							parser.current_arg.name = name.clone();
							parser.change_state_to(States::ExpectEquals(AfterEquals::Arg));
						}
						lexer::Token::Bar(_) | lexer::Token::Newline(_) => {
							parser.change_state_to(States::ExpectBody);
						}
						lexer::Token::CloseTag(_) => parser.merge_or()?,
						_ => return Err(KismesisError::ExpectedMacroBodyOrArg),
					}
				},
				States::ExpectVariable(ref prev) => {
					let lexer::Token::Word(name) = token else { return Err(KismesisError::ExpectedVariableName) };
					match *prev.to_owned() {
						States::ExpectBody => {
							let top_tag = parser.get_top_tag_children_mut()?;
							top_tag.push(BodyElems::ValueTag(name.clone()));
							parser.change_state_to(States::ExpectBody);
						}
						_ => return Err(UnrecoverableError::VariableInWrongPlace.into()),
					}
				}
			}
			token_idx += 1;
		}
		println!("{:#?}", parser.file.macros);
		Ok(parser.file)
	};
	match iterate() {
		Ok(x) => {
			if !recovered_errors.is_empty() {
				let report = CompilerErrorReport {
					scanner: token_scanner,
					unresolved: None,
					resolved: recovered_errors,
				};
				Err(report)
			} else {
				Ok(x)
			}
 		},
		Err(x) => {
			let final_err = x.state(&token_scanner);
			let report = CompilerErrorReport {
				scanner: token_scanner,
				unresolved: final_err.into(),
				resolved: recovered_errors,
			};
			Err(report)
		}
	}
}

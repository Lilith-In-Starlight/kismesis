pub(crate) mod tag_stack;

use std::fmt::Debug;
use std::sync::Arc;

use crate::kiss::parser::lexer::Token;
use crate::errors::{ParsingError, ImplementationError, ErrorReport};

use self::tag_stack::elements::{Param, Tag, Constant, MacroArg, Macro, OnlyTags, ContentChild, Variable};

use super::{compiler_options::CompilerOptions, lexer};
use tag_stack::TagStack;
use tag_stack::errors::TagStackError;


#[derive(Debug, Clone)]
pub struct ParsedFile {
	pub macros: MacroArray,
	pub body: Vec<ContentChild>,
	pub consts: Vec<Constant>,
	pub lambdaconsts: Vec<String>,
}

impl ParsedFile {
	fn new() -> Self {
		Self {
			macros: MacroArray::new(),
			body: vec![],
			consts: vec![],
			lambdaconsts: vec![],
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
	ExpectVariableName,
	ExpectVarDefEquals(String),
	ExpectVarDefValue(String, char, String),
	ExpectLambdaVarName,
	ExpectLambdaDefEquals(String),
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

#[derive(Debug, Clone)]
pub struct MacroArray {
	content: Vec<Macro>,
}

impl MacroArray {
	pub fn new() -> Self {
		Self {
			content: vec![],
		}
	}
	pub fn push<T: Into<Macro>>(&mut self, el: T) {
		self.content.push(el.into())
	}

	pub fn get_content(&self) -> &Vec<Macro> {
		&self.content
	}
}

struct Parser {
	state: States,
	tag_stack: TagStack,
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
			current_arg: MacroArg::new(),
		}
	}

	fn add_new_content_tag(&mut self, scanner: &TokenScanner) { self.tag_stack.new_content_tag(scanner) }
	fn add_new_macro_call(&mut self, scanner: &TokenScanner) { self.tag_stack.new_macro_call(scanner) }
	fn add_new_macro_def(&mut self, scanner: &TokenScanner) -> Result<(), ParsingError> { self.tag_stack.new_macro_def(scanner) }

	fn set_top_tag_name(&mut self, to: String) -> Result<(), ImplementationError> {
		match self.tag_stack.last_mut() {
			None => return Err(ImplementationError::ImpossibleEmpty),
			Some(x) => x.set_name(&to),
		}
		Ok(())
	}

	fn change_state_to(&mut self, to: States) {
		self.escape = false;
		self.state = to;
	}

	fn merge_or(&mut self) -> Result<(), ParsingError> {
		let Err(e) = self.tag_stack.merge() else { return Ok(()) };
		match e {
			TagStackError::HadOneTag => match self.tag_stack.pop() {
				Some(x) => {
					match x {
						Tag::ContentTag(tag) => {
							self.file.body.push(tag.into());
							self.change_state_to(States::ExpectAnyOpener);
							Ok(())
						},
						Tag::MacroDef(m) | Tag::MacroCall(m)=> {
							self.file.macros.push(m);
							self.change_state_to(States::ExpectAnyOpener);
							Ok(())
						}
					}
				},
				None => Err(ParsingError::ClosedTooManyTags),
			}
			x => Err(x.into()),
		}
	}

	fn finish_current_param(&mut self) -> Result<(), ImplementationError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(ImplementationError::ImpossibleEmpty) };
		match top_tag {
			Tag::MacroCall(_) | Tag::MacroDef(_) => return Err(ImplementationError::AddedParamToMacro),
			Tag::ContentTag(c) => c.add_param(self.current_param.clone()),
		}
		self.current_param = Param::new();
		Ok(())
	}

	fn finish_current_arg(&mut self) -> Result<(), ImplementationError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(ImplementationError::ImpossibleEmpty) };
		match top_tag {
			Tag::MacroCall(m) | Tag::MacroDef(m) => m.add_arg(self.current_arg.clone()),
			Tag::ContentTag(_) => return Err(ImplementationError::AddedArgToTag),
		}
		self.current_arg = MacroArg::new();
		Ok(())
	}

	fn get_top_tag_children_mut(&mut self) -> Result<&mut Vec<ContentChild>, ImplementationError> {
		let Some(top_tag) = self.tag_stack.last_mut() else { return Err(ImplementationError::ImpossibleEmpty) };
		Ok(top_tag.get_children_mut())
	}

	fn get_top_tag_name(&self) -> Option<&str> {
		let Some(top_tag) = self.tag_stack.last() else { return None };
		Some(top_tag.get_name())
	}
}

#[derive(Debug)]
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
		self.position.unwrap_or(0)
	}

	pub fn get_position_in_line(&self) -> usize { self.token_in_line }

	pub fn get_all_current_line(&self) -> Vec<(&Token, usize)> {
		let mut position = self.position.unwrap_or(0);
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
		self.tokens.split_inclusive(|x| matches!(x, Token::Newline(_))).collect()
	}
}

pub fn get_ast(s: &str, options: CompilerOptions) -> Result<(TokenScanner, ParsedFile, Option<Vec<ErrorReport<ParsingError>>>), (TokenScanner, Vec<ErrorReport<ParsingError>>, ErrorReport<ParsingError>)> {
	let tokens = lexer::tokenize(&s.replace('\r', ""));
	let mut filled_lambdas: Vec<String> = Vec::new();

	let mut parser = Parser::new();
	let mut token_scanner = TokenScanner::new(tokens);
	let mut recovered_errors: Vec<ErrorReport<ParsingError>> = Vec::new();
	let iterate = | | -> Result<ParsedFile, ParsingError> {
		while let Some(token) = token_scanner.next() {
			match parser.state {
				States::ExpectAnyOpener => {
					match token {
						lexer::Token::OpenTag(_) => parser.change_state_to(States::ExpectTagName),
						lexer::Token::Word(x) if x == "var" => parser.change_state_to(States::ExpectVariableName),
						lexer::Token::Word(x) if x == "lambda" => parser.change_state_to(States::ExpectLambdaVarName),
						lexer::Token::Space(_) | lexer::Token::Newline(_) | lexer::Token::Indent(_) => continue,
						lexer::Token::CloseTag(_) => recovered_errors.push(ParsingError::ClosedTooManyTags.state(&token_scanner)),
						_ => recovered_errors.push(ParsingError::ExpectedAnyOpener.state(&token_scanner)),
					}
				}
				States::ExpectTagName => {
					match token {
						lexer::Token::Word(word) => {
							if word == "macro" {
								parser.add_new_macro_def(&token_scanner)?;
								parser.change_state_to(States::ExpectMacroName);
							} else {
								let clone_word = word.clone();
								if options.is_deprecated(&clone_word) {
									recovered_errors.push(ParsingError::UseOfDeprecatedTag.state(&token_scanner));
								}
								parser.add_new_content_tag(&token_scanner);
								parser.set_top_tag_name(clone_word)?;
								parser.change_state_to(States::ExpectParamName);
							}
						},
						lexer::Token::MacroName(word) => {
							let lambdname = word.trim_end_matches('!');
							if options.is_lambda(lambdname) && !filled_lambdas.iter().any(|x| lambdname == x) {
								filled_lambdas.push(lambdname.to_string());
							}
							let clone_word = word.clone();
							let clone_word = clone_word.trim_end_matches('!');
							parser.add_new_macro_call(&token_scanner);
							parser.set_top_tag_name(clone_word.to_string())?;
							parser.change_state_to(States::ExpectArgNameOrBody);
						},
						_ => {
							recovered_errors.push(ParsingError::ExpectedTagName.state(&token_scanner));
							parser.change_state_to(States::ExpectParamName);
						},
					}
				}
				States::ExpectParamName => {
					match token {
						lexer::Token::Newline(_) | lexer::Token::Bar(_) => {
							parser.change_state_to(States::ExpectBody)
						},
						lexer::Token::Word(name) => {
							let Some(top_tag_name) = parser.get_top_tag_name() else { return Err(ParsingError::UnrecoverableError(ImplementationError::ImpossibleEmpty)) };
							if options.is_parametric(top_tag_name) {
								parser.current_param.name = name.clone();
							} else {
								recovered_errors.push(ParsingError::ParametersNotAllowed.state(&token_scanner));
							}
							parser.change_state_to(States::ExpectEquals(AfterEquals::Param));
						},
						lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
						lexer::Token::CloseTag(_) => parser.merge_or()?,
						_ => return Err(ParsingError::ExpectedTagBodyOrParam)
					}
				}
				States::ExpectEquals(ref what) => {
					match what {
						AfterEquals::Param => match token {
							lexer::Token::Equals(_) => parser.change_state_to(States::ExpectParamValue(ParamValueType::Word, what.clone())),
							lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
							_ => return Err(ParsingError::ExpectedCharacter { character: '=' })
						}
						AfterEquals::Arg => match token {
							lexer::Token::Newline(_) | lexer::Token::Bar(_) => {
								parser.finish_current_arg()?;
								parser.change_state_to(States::ExpectBody);
							},
							lexer::Token::Equals(_) => parser.change_state_to(States::ExpectParamValue(ParamValueType::Word, what.clone())),
							lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
							lexer::Token::Word(name) => {
								parser.finish_current_arg()?;
								parser.current_arg.name = name.clone();
							}
							lexer::Token::CloseTag(_) => {
								parser.finish_current_arg()?;
								parser.merge_or()?;
							}
							_ => return Err(ParsingError::ExpectedCharacter { character: '=' })
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
											recovered_errors.push(ParsingError::ExpectedParameterValue.state(&token_scanner));
											parser.change_state_to(States::ExpectParamName);
										}
										AfterEquals::Arg => {
											recovered_errors.push(ParsingError::ExpectedArgValue.state(&token_scanner));
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
								lexer::Token::Newline(_) => recovered_errors.push(ParsingError::NewlineInParameter.state(&token_scanner)),
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
										let new = token.get_as_string();
										top_tag_body.push(new.into());
									}
								},
								Some(last_body_child) => match last_body_child {
									ContentChild::String(text) => token.push_to_string(text),
									_ => match token {
										lexer::Token::Space(_) | lexer::Token::Newline(_) | Token::Indent(_) => continue,
										_ => {
											let new = token.get_as_string();
											top_tag_body.push(new.into());
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
							recovered_errors.push(ParsingError::ExpectedMacroName.state(&token_scanner));
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
						_ => return Err(ParsingError::ExpectedMacroBodyOrArg),
					}
				},
				States::ExpectVariable(ref prev) => {
					let lexer::Token::Word(name) = token else { return Err(ParsingError::ExpectedVariableName) };
					match *prev.to_owned() {
						States::ExpectBody => {
							let top_tag = parser.get_top_tag_children_mut()?;
							top_tag.push(Variable {
								name: name.clone(),
								line: token_scanner.current_line, 
								pos_in_line: token_scanner.token_in_line
							}.into());
							parser.change_state_to(States::ExpectBody);
						}
						States::ExpectParamValue(ref _wordicity, ref _kind) => return Err(ImplementationError::VariableInUnimplementedPlace.into()),
						_ => return Err(ImplementationError::VariableInWrongPlace.into()),
					}
				}
				States::ExpectVariableName => match token {
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					lexer::Token::Word(name) => parser.change_state_to(States::ExpectVarDefEquals(name.clone())),
					_ => recovered_errors.push(ParsingError::ExpectedVariableName.state(&token_scanner)),
				},
				States::ExpectVarDefEquals(ref name) => match token {
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					lexer::Token::Quote(opener) => parser.change_state_to(States::ExpectVarDefValue(name.clone(), opener.clone(), String::new())),
					_ => recovered_errors.push(ParsingError::ExpectedVariableName.state(&token_scanner)),
				}
				States::ExpectVarDefValue(ref name, opener, ref mut value) => match token {
					lexer::Token::Quote(closer) if *closer == opener => {
						parser.file.consts.push(Constant {
							name: name.clone(),
							value: value.clone(),
						});
						parser.change_state_to(States::ExpectAnyOpener);
					},
					x => x.push_to_string(value),
				}
				States::ExpectLambdaVarName => match token {
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					lexer::Token::Word(name) => parser.change_state_to(States::ExpectLambdaDefEquals(name.clone())),
					_ => {
						recovered_errors.push(ParsingError::ExpectedVariableName.state(&token_scanner));
						parser.change_state_to(States::ExpectLambdaDefEquals(String::new()));
					},
				},
				States::ExpectLambdaDefEquals(ref name) => match token {
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					lexer::Token::Newline(_) => {
						parser.file.lambdaconsts.push(name.clone());
						parser.change_state_to(States::ExpectAnyOpener);
					},
					_ => {
						recovered_errors.push(ParsingError::ExpectedVariableName.state(&token_scanner));
						parser.change_state_to(States::ExpectAnyOpener);
					},
				}
			}
		}
		if filled_lambdas.len() != options.lambda_macros.len() {
			recovered_errors.push(ErrorReport::Stateless(ParsingError::UncalledLambdas(options.lambda_macros.clone())))
		}
		Ok(parser.file)
	};

	match iterate() {
		Ok(x) => {
			if !recovered_errors.is_empty() {
				Ok((token_scanner, x, Some(recovered_errors)))
			} else {
				Ok((token_scanner, x, None))
			}
 		},
		Err(x) => {
			let final_err = x.state(&token_scanner);
			Err((token_scanner, recovered_errors, final_err))
		}
	}
}

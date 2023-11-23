mod lexer;



#[derive(Debug)]
enum BodyElems {
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
}

#[derive(Debug)]
struct Param {
	name: String,
	value: String,
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
struct MacroDefTag {
	name: String,
	args: Vec<MacroArg>,
	children: Vec<BodyElems>,
}

#[derive(Debug)]
struct Constant {
	name: String,
	value: String,
}

#[derive(Debug)]
pub struct ParsedFile {
	macros: Vec<MacroDefTag>,
	body: Vec<BodyElems>,
	consts: Vec<Constant>,
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


pub fn get_ast(s: &str) -> Result<ParsedFile, &'static str> {
	let tokens = match lexer::tokenize(&s.replace('\r', "")) {
		Ok(x) => x,
		Err(x) => return Err(x),
	};
	println!("{:#?}", tokens);
	let mut parsed_file = ParsedFile::new();

	let mut current_parameter = Param::new();
	let mut state = States::ExpectAnyOpener;
	let mut tag_stack: Vec<BodyElems> = vec![];

	for token in tokens {
		match state {
			States::ExpectAnyOpener => {
				match token {
					lexer::Token::OpenTag(_) => {
						tag_stack.push(BodyElems::new_content_tag());
						state = States::ExpectTagName
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
							match tag_stack.last_mut() {
								Some(x) => {
									match x {
										BodyElems::ContentTag { name , ..} => *name = word,
										BodyElems::MacroCall { name , ..} => *name = word,
										_ => return Err("Tried to insert a tag into a non-tag's body (invalid compilation state)")
									}
								},
								None => return Err("Attempted to add a parameter to a tag while there were no tags in the stack (invalid compilation state)")
							};
							state = States::ExpectParamName
						}
					}
					_ => return Err("Expected a name for the tag")
				}
			}
			States::ExpectParamName => {
				match token {
					lexer::Token::Newline(_) | lexer::Token::Bar(_) => state = States::ExpectBody,
					lexer::Token::Word(name) => {
						current_parameter.name = name;
						state = States::ExpectParamEquals;
					},
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					lexer::Token::CloseTag(_) => {
						match merge_top_tags(&mut tag_stack, &mut parsed_file.body) {
							Ok(_) => (),
							Err(x) => return Err(x),
						}
					}
					_ => return Err("Expected a tag body or a parameter name")
				}
			}
			States::ExpectParamEquals => {
				match token {
					lexer::Token::Equals(_) => state = States::ExpectParamValue(ParamValueType::Word),
					lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
					_ => return Err("Expected a tag body or a parameter name")
				}
			}
			States::ExpectParamValue(ref kind) => {
				match kind {
					ParamValueType::Word => {
						match token {
							lexer::Token::Quote(opener) => state = States::ExpectParamValue(ParamValueType::MultiWord(opener)),
							lexer::Token::Space(_) | lexer::Token::Indent(_) => continue,
							lexer::Token::Word(word) => {
								current_parameter.value = word;
								match add_param_to_top(current_parameter, &mut tag_stack) {
									Ok(_) => (),
									Err(x) => return Err(x),
								}
								current_parameter = Param::new();
								state = States::ExpectParamName
							}
							_ => return Err("Expected a a word or a quotation mark")
						}
					}
					ParamValueType::MultiWord(closer) => {
						match token {
							lexer::Token::Quote(closer) => {
								match add_param_to_top(current_parameter, &mut tag_stack) {
									Ok(_) => (),
									Err(x) => return Err(x),
								}
								current_parameter = Param::new();
								state = States::ExpectParamName
							}
							lexer::Token::Newline(c) => return Err("Expected a continuation to the string or a quotation mark, found newline (multiline parameters are not supported yet)"),
							lexer::Token::Word(word) => current_parameter.value.push_str(&word),
							lexer::Token::Space(c) => current_parameter.value.push(c),
							_ => todo!("Allow non-words into MultiWord params"),
						}
					}
				}
			}
			States::ExpectBody => {
				match token {
					lexer::Token::OpenTag(_) => {
						tag_stack.push(BodyElems::new_content_tag());
						state = States::ExpectTagName
					}
					lexer::Token::CloseTag(_) => {
						match merge_top_tags(&mut tag_stack, &mut parsed_file.body) {
							Ok(_) => (),
							Err(x) => return Err(x),
						}
					},
					lexer::Token::Word(word) => {
						let top_tag_body = match tag_stack.last_mut() {
							Some(x) => {
								match x {
									BodyElems::ContentTag { children , ..} => children,
									BodyElems::MacroCall { children , ..} => children,
									_ => return Err("Tried to insert a tag into a non-tag's body (invalid compilation state)")
								}
							},
							None => return Err("Attempted to add a word to a tag's body while there were no tags in the stack (invalid compilation state)")
						};						
						top_tag_body.push(BodyElems::String(word));
					}
					lexer::Token::Space(c) | lexer::Token::Newline(c) => {
						let top_tag_body = match tag_stack.last_mut() {
							Some(x) => {
								match x {
									BodyElems::ContentTag { children , ..} => children,
									BodyElems::MacroCall { children , ..} => children,
									_ => return Err("Tried to insert a tag into a non-tag's body (invalid compilation state)")
								}
							},
							None => return Err("Attempted to add a space to a tag's body while there were no tags in the stack (invalid compilation state)")
						};						
						top_tag_body.push(BodyElems::String(c.to_string()));
					}
					lexer::Token::Indent(_) => continue,
					_ => todo!("Allow other tokens inside body")
				}
			}
		}
	}
	Ok(parsed_file)
}

fn merge_top_tags(tag_stack: &mut Vec<BodyElems>, tags: &mut Vec<BodyElems>) -> Result<(), &'static str> {
	let current_tag = match tag_stack.pop() {
		Some(x) => {
			match x {
				BodyElems::ContentTag {..} => x,
				BodyElems::MacroCall {..} => x,
				_ => return Err("Tried to close a non-tag's body (invalid compilation state)")
			}
		},
		None => return Err("Attempted to merge tags while there were no tags in the stack (invalid compilation state)")

	};

	match tag_stack.last_mut() {
		Some(x) => {
			match x {
				BodyElems::ContentTag { children , ..} => children.push(current_tag),
				BodyElems::MacroCall { children , ..} => children.push(current_tag),
				_ => return Err("Tried to insert a tag into a non-tag's body (invalid compilation state)")
			}
		},
		None => tags.push(current_tag),
	};
	Ok(())
}

fn add_param_to_top(param: Param, tag_stack: &mut Vec<BodyElems>) -> Result<(), &'static str> {
	match tag_stack.last_mut() {
		Some(x) => {
			match x {
				BodyElems::ContentTag { params, .. } => params.push(param),
				_ => return Err("Adding a parameter to something that is not a tag (invalid compilation state)")
			}
		},
		None => return Err("Attempted to add a parameter to a tag while there were no tags in the stack (invalid compilation state)")
	};
	Ok(())
}
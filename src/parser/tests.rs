use std::fs::read_to_string;

use crate::{lexer::{Token, self}, parser::{literal, character, skip_spaces}};

use super::{zero_or_more, after_spaces, attribute, state::ParserState, Parser};

#[test]
fn test_multi_attrs() {
    let tokens = lexer::tokenize(&read_to_string("test/attr_test.ks").unwrap().replace('\r', ""));
	println!("{:#?}", zero_or_more(after_spaces(attribute)).parse(ParserState::new(&tokens)));
    let tokens = lexer::tokenize(&read_to_string("test/attr_test2.ks").unwrap().replace('\r', ""));
	println!("{:#?}", literal.cut().followed_by(zero_or_more(after_spaces(character('=').cut().followed_by(literal)))).parse(ParserState::new(&tokens)));
}

use crate::lexer::Token;


#[test]
fn test_tag_head() {
	use crate::parser::errors::Error;
	use crate::{lexer::tokenize, parser::errors::ParserResult, parser::errors::ErrorState};
	use super::ParserState;
	let input = tokenize("<tag attribute='value' att!bute='value'|");
	println!("{:#?}", super::tag_head(ParserState::new(&input)));
	let input = tokenize("<tag attribute='value' att!bute art='asdf'");
	println!("{:#?}", super::tag_head(ParserState::new(&input)));
	let input = tokenize("<tag attr='one' < othertag attr='two '|");
	println!("{:#?}", super::tag_head(ParserState::new(&input)));
}

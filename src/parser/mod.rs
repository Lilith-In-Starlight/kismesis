mod types;
mod state;
mod errors;
mod tests;

use std::convert::Infallible;

use crate::lexer::Token;

use self::types::{StringParts, Attribute, HtmlTag};
use self::errors::{ParserResult, Error, ErrorState};
use self::state::ParserState;

type FallibleParse<'a, T> = ParserResult<'a, T, ErrorState>;

trait Parser<'a, Output, Err> {
    fn parse(&self, state: ParserState<'a>) -> ParserResult<'a, Output, Err>;

    fn or<P, Err2>(self, other: P) -> BoxedParser<'a, Output, Err2> where 
        Self: Sized + 'a,
        P: Parser<'a, Output, Err2> + 'a, 
        Output: 'a,
        Err2: 'a,
        Err:  'a,
        Self: Parser<'a, Output, Err>,
    {
        BoxedParser::new(or(self, other))
    }
    fn followed_by<P, O2, Err2>(self, other: P) -> BoxedParser<'a, Output, Err> where 
        Self: Sized + 'a,
        P: Parser<'a, O2, Err2> + 'a, 
        Output: 'a,
        O2: 'a,
        Err: 'a,
        Err2: 'a,
    {
        BoxedParser::new(followed_by(self, other))
    }
    fn preceding<P, O2, E2>(self, other: P) -> BoxedParser<'a, O2, E2> where 
        Self: Sized + 'a,
        P: Parser<'a, O2, E2> + 'a, 
        Output: 'a,
        O2: 'a,
        E2: 'a,
    {
        BoxedParser::new(preceding(self, other))
    }
} 

impl<'a, Output, F> Parser<'a, Output, ErrorState> for F where F: Fn(ParserState<'a>) -> ParserResult<'a, Output, ErrorState> {
    fn parse(&self, state: ParserState<'a>) -> ParserResult<'a, Output, ErrorState> {
        self(state)
    }
}

impl<'a, Output, F> Parser<'a, Output, Infallible> for F where F: Fn(ParserState<'a>) -> ParserResult<'a, Output, Infallible> {
    fn parse(&self, state: ParserState<'a>) -> ParserResult<'a, Output, Infallible> {
        self(state)
    }
}

struct BoxedParser<'a, T, E> {
    parser: Box<dyn Parser<'a, T, E> + 'a>,
}

impl<'a, T, E> BoxedParser<'a, T, E> {
    fn new<P>(parser: P) -> Self where P: Parser<'a, T, E> + 'a {
        Self { parser: Box::new(parser) }
    }
}

impl<'a, T, E> Parser<'a, T, E> for BoxedParser<'a, T, E> {
    fn parse(&self, state: ParserState<'a>) -> ParserResult<'a, T, E> {
        self.parser.parse(state)
    }
}

// Parsers
fn quote_mark(state: ParserState) -> FallibleParse<&char> {
    character('\'').or(character('"')).parse(state)
}

fn quoted(state: ParserState) -> FallibleParse<Vec<StringParts>> {
    let (opener, mut state) = match quote_mark.parse(state) {
        ParserResult::Ok(opener, next_state) => (opener, next_state),
        ParserResult::Err(_, error_state) => return ParserResult::err(Error::ExpectedQuoteStart, error_state)
    };

    let mut output = Vec::<StringParts>::new();
    let mut escape = false;
    while let Some(token) = state.first_token() {
        match token {
            Token::Symbol(sym) if *sym == '@' && !escape => todo!("Variables in assignments"),
            Token::Symbol(sym) if sym == opener && !escape => return ParserResult::Ok(output, state.next_state()),
            Token::Symbol(sym) if *sym == '\\' && !escape => {
                escape = true;
                state= state.next_state();
            },
            tok @ _ => match output.pop() {
                Some(StringParts::String(mut string)) => {
                    tok.push_to_string(&mut string);
                    output.push(StringParts::String(string));
                    state = state.next_state();
                }
                Some(StringParts::Variable(var)) => {
                    output.push(StringParts::Variable(var));
                    output.push(StringParts::String(tok.get_as_string()));
                    state = state.next_state();
                }
                None => {
                    output.push(StringParts::String(tok.get_as_string()));
                    state = state.next_state();
                }
            },
        }
    }

    ParserResult::err(Error::UnclosedQuote, state)
}

fn space(state: ParserState) -> FallibleParse<&char> {
    match state.advanced() {
        (Some(Token::Space(space)), next_state) => ParserResult::Ok(space, next_state),
        (_, next_state) => ParserResult::err(Error::NotASpace, next_state),
    }
}

fn indent(state: ParserState) -> FallibleParse<&char> {
    match state.advanced() {
        (Some(Token::Indent(indent)), next_state) => ParserResult::Ok(indent, next_state),
        (_, next_state) => ParserResult::err(Error::NotAnIndent, next_state),
    }
}

fn some_symbol(state: ParserState) -> FallibleParse<&char> {
    match state.advanced() {
        (Some(Token::Symbol(x)), next_state) => ParserResult::Ok(x, next_state),
        _ => ParserResult::err(Error::NotSymbol, state),
    }
}
fn literal(state: ParserState) -> FallibleParse<&str> {
    match state.advanced() {
        (Some(Token::Word(x)), next_state) => ParserResult::Ok(x, next_state),
        _ => ParserResult::err(Error::NotSymbol, state),
    }
}

fn tag_head<'a>(state: ParserState<'a>) -> FallibleParse<'a, (String, Vec<Attribute>, Vec<HtmlTag>)> {
	let (name, state) = match character('<').preceding(literal).parse(state) {
		ParserResult::Ok(result, state) => (result.to_owned(), state),
		ParserResult::Err(error, state) => return ParserResult::Err(error, state),
	};

	let (attributes, state) = match zero_or_more(zero_or_more(space.or(indent)).preceding(attribute)).parse(state) {
		ParserResult::Ok(attrs, state) => (attrs, state),
		ParserResult::Err(never, state) => match never {},
	};

	let (subtags, state) = match zero_or_more(zero_or_more(space.or(indent)).preceding(character('<')).preceding(zero_or_more(space.or(indent)).preceding(subtag))).parse(state) {
		ParserResult::Ok(subtags, state) => (subtags, state),
		ParserResult::Err(never, state) => match never {},
	};
	ParserResult::Ok((name, attributes, subtags), state)
}


fn subtag<'a>(state: ParserState<'a>) -> FallibleParse<'a, HtmlTag> {
	let (name, state) = match zero_or_more(space.or(indent)).preceding(literal).parse(state) {
		ParserResult::Ok(result, state) => (result.to_owned(), state),
		ParserResult::Err(error, state) => return ParserResult::Err(error, state),
	};

	let (attributes, state) = match zero_or_more(zero_or_more(space.or(indent)).preceding(attribute)).parse(state) {
		ParserResult::Ok(attrs, state) => (attrs, state),
		ParserResult::Err(never, state) => match never {},
	};

	ParserResult::Ok(HtmlTag {name, attributes, subtags: vec![], body: vec![]}, state)
}

fn attribute<'a>(state: ParserState<'a>) -> FallibleParse<'a, Attribute> {
    let (name, state) = match literal.followed_by(zero_or_more(space.or(indent))).parse(state) {
        ParserResult::Ok(name, next_state) => (name.to_owned(), next_state),
        ParserResult::Err(error, error_state) => return ParserResult::Err(error, error_state),
    };

    let value = character('=')
        .preceding(zero_or_more(space.or(indent)))
        .preceding(quoted).parse(state);

    let (value, state) = match value {
        ParserResult::Ok(value, state) => (value, state),
        ParserResult::Err(error, mut state) => {
			state.errors.push(error);
			return ParserResult::Ok(Attribute { name, value: vec![] }, state);
		}
    };
    ParserResult::Ok(Attribute { name, value }, state)
}
// Generators

fn preceding<'a, P1, O1, E1, P2, O2, E2>(p1: P1, p2: P2) -> impl Parser<'a, O2, E2>
where
    P1: Parser<'a, O1, E1>,
    P2: Parser<'a, O2, E2>
{
    move |state| match p1.parse(state) {
        ParserResult::Ok(_, next_state) => match p2.parse(next_state) {
            ParserResult::Ok(result, next_state) => ParserResult::Ok(result, next_state),
            ParserResult::Err(err, next_state) => ParserResult::Err(err, next_state)
        },
        ParserResult::Err(error, next_state) => ParserResult::Err(error, next_state),
    }
}

fn followed_by<'a, P1, O1, E1, P2, O2, E2>(p1: P1, p2: P2) -> impl Parser<'a, O1, E1>
where
    P1: Parser<'a, O1, E1>,
    P2: Parser<'a, O2, E2>,
    E1: From<E2>,
{
    move |state| match p1.parse(state) {
        ParserResult::Ok(result, next_state) => match p2.parse(next_state) {
            ParserResult::Ok(_, next_state) => ParserResult::Ok(result.into(), next_state),
            ParserResult::Err(err, next_state) => ParserResult::Err(err.into(), next_state)
        },
        ParserResult::Err(error, next_state) => ParserResult::Err(error, next_state),
    }
}

fn or<'a, P1, O1, E1, P2, E2>(p1: P1, p2: P2) -> impl Parser<'a, O1, E2>
where
    P1: Parser<'a, O1, E1>,
    P2: Parser<'a, O1, E2>
{
    move |state| match p1.parse(state) {
        ParserResult::Ok(result, next_state) => ParserResult::Ok(result, next_state),
        ParserResult::Err(_, next_state) => p2.parse(next_state),
    }
}


fn character<'a>(chr: char) -> impl Parser<'a, &'a char, ErrorState> {
    move |state: ParserState<'a> | {
        match some_symbol.parse(state.clone()) {
            ParserResult::Ok(x, next_state) if x == &chr => ParserResult::Ok(x, next_state),
            ParserResult::Ok(x, _) => ParserResult::err(Error::CharacterNotMatch { expected: chr, got: Some(x.clone()) }, state),
            ParserResult::Err(error, error_state) => ParserResult::Err(error, error_state),
        }
    }
}

fn zero_or_more<'a, P, T, Err>(parser: P) -> impl Parser<'a, Vec<T>, Infallible> where P: Parser<'a, T, Err> {
    move | state: ParserState<'a> | {
        let mut state = state;
        let mut found = Vec::<T>::new();
		if matches!(state.first_token(), None) {
	        return ParserResult::Ok(found, state)
		}
        while let ParserResult::Ok(token, next_state) = parser.parse(state.clone()) {
            state = next_state;
            found.push(token);
        }
        ParserResult::Ok(found, state)
    }
}

// Tests

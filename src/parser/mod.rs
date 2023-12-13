mod types;
mod state;
mod errors;
mod tests;

use crate::lexer::Token;

use self::types::{StringParts, Attribute, HtmlTag, BodyTags};
use self::errors::{ParserResult, Error, Recoverable};
use self::state::ParserState;


trait Parser<'a, Output> {
    fn parse(&self, state: ParserState<'a>) -> ParserResult<'a, Output>;

    fn or<P>(self, other: P) -> BoxedParser<'a, Output> where 
        Self: Sized + 'a,
        P: Parser<'a, Output> + 'a, 
        Output: 'a,
    {
        BoxedParser::new(or(self, other))
    }
    fn or_d<P, O2>(self, other: P) -> BoxedParser<'a, ()> where 
        Self: Sized + 'a,
        P: Parser<'a, O2> + 'a, 
        Output: 'a,
		O2: 'a,
    {
        BoxedParser::new(or_d(self, other))
    }
    fn followed_by<P, O2>(self, other: P) -> BoxedParser<'a, Output> where 
        Self: Sized + 'a,
        P: Parser<'a, O2> + 'a, 
        Output: 'a,
        O2: 'a,
    {
        BoxedParser::new(followed_by(self, other))
    }
    fn preceding<P, O2>(self, other: P) -> BoxedParser<'a, O2> where 
        Self: Sized + 'a,
        P: Parser<'a, O2> + 'a, 
        Output: 'a,
        O2: 'a,
    {
        BoxedParser::new(preceding(self, other))
    }
} 

impl<'a, Output, F> Parser<'a, Output> for F where F: Fn(ParserState<'a>) -> ParserResult<'a, Output> {
    fn parse(&self, state: ParserState<'a>) -> ParserResult<'a, Output> {
        self(state)
    }
}

struct BoxedParser<'a, T> {
    parser: Box<dyn Parser<'a, T> + 'a>,
}

impl<'a, T> BoxedParser<'a, T> {
    fn new<P>(parser: P) -> Self where P: Parser<'a, T> + 'a {
        Self { parser: Box::new(parser) }
    }
}

impl<'a, T> Parser<'a, T> for BoxedParser<'a, T> {
    fn parse(&self, state: ParserState<'a>) -> ParserResult<'a, T> {
        self.parser.parse(state)
    }
}

// Parsers
fn quote_mark(state: ParserState) -> ParserResult<&char> {
    match character('\'').or(character('"')).parse(state) {
        ParserResult::Err(_, error_state) => ParserResult::err(Error::NotQuoteMark, error_state),
        ok @ _ => ok,
    }
}

fn quoted(state: ParserState) -> ParserResult<Vec<StringParts>> {
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

fn space(state: ParserState) -> ParserResult<&char> {
    match state.advanced() {
        (Some(Token::Space(space)), next_state) => ParserResult::Ok(space, next_state),
        (_, next_state) => ParserResult::err(Error::NotASpace, next_state),
    }
}

fn indent(state: ParserState) -> ParserResult<&char> {
    match state.advanced() {
        (Some(Token::Indent(indent)), next_state) => ParserResult::Ok(indent, next_state),
        (_, next_state) => ParserResult::err(Error::NotAnIndent, next_state),
    }
}

fn some_symbol(state: ParserState) -> ParserResult<&char> {
    match state.advanced() {
        (Some(Token::Symbol(x)), next_state) => ParserResult::Ok(x, next_state),
        _ => ParserResult::err(Error::NotSymbol, state),
    }
}
fn literal(state: ParserState) -> ParserResult<&str> {
    match state.advanced() {
        (Some(Token::Word(x)), next_state) => ParserResult::Ok(x, next_state),
        _ => ParserResult::err(Error::NotSymbol, state),
    }
}

fn tag_head<'a>(state: ParserState<'a>) -> ParserResult<'a, (String, Vec<Attribute>, Vec<HtmlTag>)> {
	let (name, state) = match character('<').preceding(literal).parse(state) {
		ParserResult::Ok(result, state) => (result.to_owned(), state),
		ParserResult::Err(error, state) => return ParserResult::Err(error, state),
	};

	let (attributes, state) = match zero_or_more(zero_or_more(space.or(indent)).preceding(attribute)).parse(state) {
		ParserResult::Ok(attrs, state) => (attrs, state),
		ParserResult::Err(error, state) => return ParserResult::Err(error, state),
	};

	let (subtags, state) = match zero_or_more(zero_or_more(space.or(indent)).preceding(character('<')).preceding(zero_or_more(space.or(indent)).preceding(subtag))).parse(state) {
		ParserResult::Ok(subtags, state) => (subtags, state),
		ParserResult::Err(error, state) => return ParserResult::Err(error, state),
	};
	ParserResult::Ok((name, attributes, subtags), state)
}

fn tag_body<'a>(state: ParserState<'a>) -> ParserResult<'a, Vec<BodyTags> {
    
}

fn subtag<'a>(state: ParserState<'a>) -> ParserResult<'a, HtmlTag> {
	let (name, state) = match zero_or_more(space.or(indent)).preceding(literal).parse(state) {
		ParserResult::Ok(result, state) => (result.to_owned(), state),
		ParserResult::Err(error, state) => return ParserResult::Err(error, state),
	};

	let (attributes, state) = match zero_or_more(zero_or_more(space.or(indent)).preceding(attribute)).parse(state) {
		ParserResult::Ok(attrs, state) => (attrs, state),
		ParserResult::Err(error, state) => return ParserResult::Err(error, state),
	};

	ParserResult::Ok(HtmlTag {name, attributes, subtags: vec![], body: vec![]}, state)
}

fn attribute<'a>(state: ParserState<'a>) -> ParserResult<'a, Attribute> {
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

fn preceding<'a, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<'a, O2>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>
{
    move |state| match p1.parse(state) {
        ParserResult::Ok(_, next_state) => match p2.parse(next_state) {
            ParserResult::Ok(result, next_state) => ParserResult::Ok(result, next_state),
            ParserResult::Err(err, next_state) => ParserResult::Err(err, next_state)
        },
        ParserResult::Err(error, next_state) => ParserResult::Err(error, next_state),
    }
}

fn followed_by<'a, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<'a, O1>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>
{
    move |state| match p1.parse(state) {
        ParserResult::Ok(result, next_state) => match p2.parse(next_state) {
            ParserResult::Ok(_, next_state) => ParserResult::Ok(result, next_state),
            ParserResult::Err(err, next_state) => ParserResult::Err(err, next_state)
        },
        ParserResult::Err(error, next_state) => ParserResult::Err(error, next_state),
    }
}

fn or<'a, P1, O1, P2>(p1: P1, p2: P2) -> impl Parser<'a, O1>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O1>
{
    move |state| match p1.parse(state) {
        ParserResult::Ok(result, next_state) => ParserResult::Ok(result, next_state),
        ParserResult::Err(_, next_state) => p2.parse(next_state),
    }
}

fn or_d<'a, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<'a, ()>
where
    P1: Parser<'a, O1>,
    P2: Parser<'a, O2>
{
    move |state| match p1.parse(state) {
        ParserResult::Ok(_, next_state) => ParserResult::Ok((), next_state),
        ParserResult::Err(_, next_state) => match p2.parse(next_state) {
			ParserResult::Ok(_, next_state) => ParserResult::Ok((), next_state),
			ParserResult::Err(err, next_state) => ParserResult::Err(err, next_state)
		},
    }
}

fn character<'a>(chr: char) -> impl Parser<'a, &'a char> {
    move |state: ParserState<'a> | {
        match some_symbol.parse(state.clone()) {
            ParserResult::Ok(x, next_state) if x == &chr => ParserResult::Ok(x, next_state),
            ParserResult::Ok(x, _) => ParserResult::err(Error::CharacterNotMatch { expected: chr, got: Some(x.clone()) }, state),
            ParserResult::Err(error, error_state) => ParserResult::Err(error, error_state),
        }
    }
}


fn recover<'a, P, T>(parser: P) -> impl Parser<'a, T> where P: Parser<'a, T>, T: Recoverable {
    move |state| match parser.parse(state) {
        ParserResult::Err(error, mut state) => {
            state.errors.push(error);
            ParserResult::Ok(T::empty(), state)
        }
        ok @ _ => ok,
    }
}

fn zero_or_more<'a, P, T>(parser: P) -> impl Parser<'a, Vec<T>> where P: Parser<'a, T> {
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

//! Module containing all of kismesis' parsing utilities.
//!
//! Kismesis uses parser combinators over tokens to get the job done.

#![allow(clippy::needless_pass_by_value)]
mod combinators;
pub mod errors;
mod semantics;
pub(crate) mod state;
pub mod types;

use combinators::{
	and_also, and_maybe, change_err, cut, dbg, followed_by, get_range, ignore, is, map, maybe,
	maybe_until, not, or, peek, preceding, repeated, zero_or_more,
};
use std::fmt::Debug;
use std::path::PathBuf;

use crate::errors::MaybeStateless;
use crate::html::ScopedError;
use crate::lexer::Token;
use crate::plugins::PluginInput;
use crate::{KisTemplateId, KisTokenId, Kismesis};

use self::errors::{Err, ParseError};
use self::semantics::{Semantics, Verify};
use self::state::State;
use self::types::{
	Argument, Attribute, BinFunc, BodyNodes, BodyTags, Content, Expression, ForTag, HtmlNodes,
	HtmlTag, IfTag, Lambda, Macro, Paragraph, ParsedFile, PlugCall, Ranged, Section, StringParts,
	Tag, TopNodes, UniFunc, Variable,
};

use super::errors::ErrorState;

type ParserResult<'a, T> = Result<(T, State<'a>), Err>;

pub(crate) trait Parser<'a, Output> {
	fn parse(&self, state: State<'a>) -> ParserResult<'a, Output>;

	fn map<F, T2>(self, fun: F) -> BoxedParser<'a, T2>
	where
		Self: Sized + 'a,
		F: Fn(Output) -> T2 + 'a,
		T2: 'a,
		Output: 'a,
	{
		BoxedParser::new(map(self, fun))
	}

	fn set_err<F>(self, fun: F) -> BoxedParser<'a, Output>
	where
		Self: Sized + 'a,
		F: Fn() -> ParseError + 'a,
		Output: 'a,
	{
		BoxedParser::new(change_err(self, fun))
	}

	fn is<F>(self, fun: F) -> BoxedParser<'a, Output>
	where
		Self: Sized + 'a,
		F: Fn(&Output) -> bool + 'a,
		Output: 'a,
	{
		BoxedParser::new(is(self, fun))
	}

	#[allow(dead_code)]
	fn dbg(self) -> BoxedParser<'a, Output>
	where
		Self: Sized + 'a,
		Output: Debug + 'a,
	{
		#[warn(dead_code)]
		{
			BoxedParser::new(dbg(self))
		}
	}
	fn or<P>(self, other: P) -> BoxedParser<'a, Output>
	where
		Self: Sized + 'a,
		P: Parser<'a, Output> + 'a,
		Output: 'a,
	{
		BoxedParser::new(or(self, other))
	}

	fn followed_by<P, O2>(self, other: P) -> BoxedParser<'a, Output>
	where
		Self: Sized + 'a,
		P: Parser<'a, O2> + 'a,
		Output: 'a,
		O2: 'a,
	{
		BoxedParser::new(followed_by(self, other))
	}
	fn preceding<P, O2>(self, other: P) -> BoxedParser<'a, O2>
	where
		Self: Sized + 'a,
		P: Parser<'a, O2> + 'a,
		Output: 'a,
		O2: 'a,
	{
		BoxedParser::new(preceding(self, other))
	}
	fn and_also<P, O2>(self, other: P) -> BoxedParser<'a, (Output, O2)>
	where
		Self: Sized + 'a,
		P: Parser<'a, O2> + 'a,
		Output: 'a,
		O2: 'a,
	{
		BoxedParser::new(and_also(self, other))
	}
	fn maybe_until<P, O2>(self, terminator: P, allow_empty: bool) -> BoxedParser<'a, Vec<Output>>
	where
		Self: Sized + 'a,
		P: Parser<'a, O2> + 'a,
		Output: 'a,
		O2: 'a,
	{
		BoxedParser::new(maybe_until(self, terminator, allow_empty))
	}
	fn and_maybe<P, O2>(self, other: P) -> BoxedParser<'a, (Output, Option<O2>)>
	where
		Self: Sized + 'a,
		P: Parser<'a, O2> + 'a,
		Output: 'a,
		O2: 'a,
	{
		BoxedParser::new(and_maybe(self, other))
	}
}

impl<'a, Output, F> Parser<'a, Output> for F
where
	F: Fn(State<'a>) -> ParserResult<'a, Output>,
{
	fn parse(&self, state: State<'a>) -> ParserResult<'a, Output> {
		self(state)
	}
}

pub(crate) struct BoxedParser<'a, T> {
	parser: Box<dyn Parser<'a, T> + 'a>,
}

impl<'a, T> BoxedParser<'a, T> {
	fn new<P>(parser: P) -> Self
	where
		P: Parser<'a, T> + 'a,
	{
		Self {
			parser: Box::new(parser),
		}
	}
}

impl<'a, T> Parser<'a, T> for BoxedParser<'a, T> {
	fn parse(&self, state: State<'a>) -> ParserResult<'a, T> {
		self.parser.parse(state)
	}
}

// Parsers
fn quote_mark(state: State) -> ParserResult<&char> {
	match specific_symbol('\'')
		.or(specific_symbol('"'))
		.parse(state.clone())
	{
		Err(_) => Err(ParseError::NotQuoteMark.error_at(&state)),
		ok => ok,
	}
}

fn tag_opener(state: State) -> ParserResult<&char> {
	match specific_symbol('<').parse(state.open_tag()) {
		Err(_) => Err(ParseError::ExpectedTagOpener.error_at(&state)),
		Ok((char, state)) => Ok((char, state)),
	}
}

fn subtag_opener(state: State) -> ParserResult<&char> {
	match specific_symbol('+').parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedTagOpener.error_at(&state)),
		ok => ok,
	}
}

fn tag_closer(state: State) -> ParserResult<&char> {
	match after_blanks(specific_symbol('>')).parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedTagCloser.error_at(&state)),
		Ok((val, next_state)) => match next_state.close_tag() {
			Ok(x) => Ok((val, x)),
			Err(x) => Err(x.error_at(&state).cut()),
		},
	}
}

fn tag_closer_superficial(state: State) -> ParserResult<&char> {
	match after_blanks(specific_symbol('>')).parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedTagCloser.error_at(&state)),
		Ok((val, next_state)) => Ok((val, next_state)),
	}
}

fn expr_opener(state: State) -> ParserResult<&char> {
	match specific_symbol('{').parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedExprStart.error_at(&state)),
		ok => ok,
	}
}

fn expr_closer(state: State) -> ParserResult<&char> {
	match specific_symbol('}').parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedExprEnd.error_at(&state)),
		ok => ok,
	}
}

fn macro_mark(state: State) -> ParserResult<&char> {
	match specific_symbol('!').parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedMacroMark.error_at(&state)),
		ok => ok,
	}
}

fn plugin_mark(state: State) -> ParserResult<&char> {
	match specific_symbol('?').parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedPluginMark.error_at(&state)),
		ok => ok,
	}
}

fn body_opener(state: State) -> ParserResult<&char> {
	match specific_symbol('|')
		.or(specific_symbol(':'))
		.parse(state.clone())
	{
		Err(_) => Err(ParseError::ExpectedBodyOpener.error_at(&state)),
		ok => ok,
	}
}

fn macro_name(state: State) -> ParserResult<&str> {
	match literal.parse(state.clone()) {
		Ok(ok) if ok.0 != "content" && ok.0 != "if" && ok.0 != "for" => Ok(ok),
		_ => Err(ParseError::ExpectedTagName.error_at(&state)),
	}
}

fn attr_name(state: State) -> ParserResult<String> {
	repeated(
		literal
			.map(ToString::to_string)
			.or(specific_symbol('-').map(ToString::to_string)),
		1..,
	)
	.map(|x| x.into_iter().collect())
	.set_err(|| ParseError::InvalidAttrName)
	.parse(state)
}

fn set_starter(state: State) -> ParserResult<&str> {
	match literal.parse(state.clone()) {
		Ok(ok) if ok.0 == "set" => Ok(ok),
		_ => Err(ParseError::ExpectedSetStarter.error_at(&state)),
	}
}

fn set_stmt(state: State) -> ParserResult<(String, String)> {
	let parser =
		set_starter.preceding(cut(after_spaces(literal)
			.and_also(after_spaces(equals).preceding(after_spaces(attr_string)))));

	match parser.parse(state.clone()) {
		Ok(((name, value), next_state)) => {
			let value = {
				let mut output = Vec::new();
				for part in &value {
					match part {
						StringParts::String(x) => output.push(x.clone()),
						StringParts::Expression(_) => {
							return Err(ParseError::ExpressionInSetStmt.error_at(&state))
						}
					}
				}
				output.into_iter().collect()
			};
			Ok(((name.to_string(), value), next_state))
		}
		Err(x) => Err(x),
	}
}

fn equals(state: State) -> ParserResult<&char> {
	match specific_symbol('=').parse(state.clone()) {
		Err(_) => Err(ParseError::ExpectedEquals.error_at(&state)),
		ok => ok,
	}
}

fn variable_name(state: State) -> ParserResult<&str> {
	literal
		.parse(state.clone())
		.map_err(|_x| ParseError::ExpectedVarName.error_at(&state))
}

fn check_tag_mismatch(state: State) -> ParserResult<()> {
	if let Some(opener) = state.tag_openers.last() {
		let inside = ErrorState {
			error: ParseError::TagOpenerMismatch,
			hints: vec![],
			text_position: types::MultilineRange::Single(*opener),
		};
		return Err(Err::Failure(ScopedError {
			error: MaybeStateless::Stateful(inside),
			scope: state.current_file,
		}));
	}
	Ok(((), state))
}

fn expr_array(state: State) -> ParserResult<Expression> {
	let parser = zero_or_more(get_range(expression).followed_by(after_blanks(
		specific_symbol(',').followed_by(skipped_blanks()),
	)))
	.and_maybe(get_range(expression))
	.map(|(mut vec, maybe)| {
		if let Some(last) = maybe {
			vec.push(last);
		}
		vec
	})
	.map(Expression::Array);

	parser.parse(state)
}

fn expression(state: State) -> ParserResult<Expression> {
	let parser = variable_name
		.map(|x| Expression::Variable(x.to_owned()))
		.or(attr_string.map(Expression::Literal))
		.or(wrapped_expr);
	parser.parse(state)
}

fn binary_func(state: State) -> ParserResult<BinFunc> {
	let (val, next_state) = literal
		.parse(state.clone())
		.map_err(|_x| ParseError::ExpectedBinFunc.error_at(&state))?;
	match val {
		"and" => Ok((BinFunc::And, next_state)),
		"or" => Ok((BinFunc::Or, next_state)),
		_ => Err(ParseError::ExpectedBinFunc.error_at(&state)),
	}
}

fn unary_func(state: State) -> ParserResult<UniFunc> {
	let (val, next_state) = literal
		.parse(state.clone())
		.map_err(|_x| ParseError::ExpectedUniFunc.error_at(&state))?;
	match val {
		"not" => Ok((UniFunc::Not, next_state)),
		_ => Err(ParseError::ExpectedUniFunc.error_at(&state)),
	}
}

fn binary_func_expr(state: State) -> ParserResult<Expression> {
	let parser = get_range(expression)
		.and_also(after_spaces(binary_func))
		.and_also(cut(after_spaces(get_range(expression))));
	let (((expr1, fun), expr2), next_state) = parser.parse(state)?;
	Ok((
		Expression::BinFunc(fun, Box::new(expr1), Box::new(expr2)),
		next_state,
	))
}

fn unary_func_expr(state: State) -> ParserResult<Expression> {
	let parser = unary_func.and_also(cut(after_spaces(get_range(expression))));
	let ((fun, expr), next_state) = parser.parse(state)?;
	Ok((Expression::UniFunc(fun, Box::new(expr)), next_state))
}

fn wrapped_expr(state: State) -> ParserResult<Expression> {
	let internal_parser = binary_func_expr
		.or(unary_func_expr)
		.or(expression)
		.or(specific_symbol('!').map(|_x| Expression::None))
		.or(expr_array);
	let parser = expr_opener.preceding(cut(
		after_blanks(internal_parser).followed_by(after_blanks(expr_closer))
	));

	parser.parse(state)
}

fn variable_definition(state: State) -> ParserResult<Variable> {
	let parser = var_def_starter
		.preceding(after_spaces(get_range(literal)))
		.and_also(cut(
			after_spaces(equals).preceding(after_spaces(get_range(expression)))
		));
	let ((name, value), next_state) = parser.parse(state)?;
	Ok((
		Variable {
			name: name.to_own(),
			value,
		},
		next_state,
	))
}

fn lambda_definition(state: State) -> ParserResult<Lambda> {
	let parser = lambda_def_starter.preceding(
		cut(after_spaces(get_range(literal)))
			.and_maybe(after_spaces(equals).preceding(after_spaces(get_range(expression)))),
	);
	let ((name, value), next_state) = parser.parse(state)?;
	Ok((
		Lambda {
			name: name.to_own(),
			value,
		},
		next_state,
	))
}

fn if_tag(state: State) -> ParserResult<IfTag> {
	let parser = specific_literal("if").preceding(cut(after_spaces(get_range(expression))
		.and_also(maybe(tag_body).map(Option::unwrap_or_default))
		.map(|(condition, body)| IfTag { condition, body })));

	parser.parse(state)
}

fn pre_tag(state: State) -> ParserResult<HtmlTag> {
	let parser = tag_head
		.is(|x| &x.0.value == "pre")
		.and_maybe(
			after_spaces(body_opener).preceding(
				specific_symbol('\\')
					.preceding(any)
					.or(any)
					.map(Token::to_string)
					.maybe_until(after_spaces(specific_symbol('>')), true),
			),
		)
		.map(|((name, attributes, subtags), body)| {
			let body = vec![HtmlNodes::Raw(
				body.unwrap_or_default().into_iter().collect(),
			)];
			HtmlTag {
				name,
				attributes,
				subtags,
				body,
			}
		});

	parser.parse(state)
}

fn for_tag(state: State) -> ParserResult<ForTag> {
	let parser = specific_literal("for").preceding(
		cut(after_spaces(get_range(literal)))
			.followed_by(after_spaces(specific_literal("in")))
			.and_also(after_spaces(get_range(expression)))
			.and_also(maybe(tag_body).map(Option::unwrap_or_default))
			.map(|((variable, iterator), body)| ForTag {
				variable: variable.to_own(),
				iterator,
				body,
			}),
	);

	parser.parse(state)
}

fn some_tag(state: State) -> ParserResult<Tag> {
	let parser = tag_opener.preceding(cut(after_spaces(
		macro_call
			.map(Tag::MacroCall)
			.or(macro_def.map(Tag::MacroDef))
			.or(plug_call.map(Tag::PlugCall))
			.or(content_macro.map(Tag::Content))
			.or(doctype.map(Tag::Doctype))
			.or(if_tag.map(Tag::If))
			.or(for_tag.map(Tag::For))
			.or(pre_tag.map(Tag::Html))
			.or(tag.map(Tag::Html))
			.set_err(|| ParseError::ExpectedSpecifierOrTag)
			.followed_by(tag_closer),
	)));

	parser.parse(state)
}

fn some_child_tag(state: State) -> ParserResult<BodyTags> {
	let parser = tag_opener
		.preceding(cut(after_spaces(
			macro_call
				.map(BodyTags::MacroCall)
				.or(plug_call.map(BodyTags::PlugCall))
				.or(content_macro.map(BodyTags::Content))
				.or(if_tag.map(BodyTags::If))
				.or(for_tag.map(BodyTags::For))
				.or(pre_tag.map(BodyTags::HtmlTag))
				.or(tag.map(|x| BodyTags::HtmlTag(x.merge_subtags())))
				.set_err(|| ParseError::ExpectedSpecifierOrTag)
				.followed_by(tag_closer),
		)))
		.or(section_block.map(|x| BodyTags::HtmlTag(x.into_tag())));

	parser.parse(state)
}

fn tag(state: State<'_>) -> ParserResult<'_, HtmlTag> {
	let parser = tag_head.and_maybe(tag_body);

	let (((name, attributes, subtags), body), state) = parser.parse(state)?;
	Ok((
		HtmlTag {
			name,
			attributes,
			body: body.unwrap_or(vec![]),
			subtags,
		}
		.merge_subtags(),
		state,
	))
}

fn section_block(state: State) -> ParserResult<Section> {
	let ((depth, title), state) = get_range(repeated(specific_symbol('#'), 1..))
		.map(|x| Ranged {
			range: x.range,
			value: x.value.len(),
		})
		.and_also(cut(after_spaces(get_range(paragraph_string))))
		.parse(state)?;
	if depth.value < state.section_depth + 1 {
		return Err(ParseError::WronglyNestedSection.error_at(&state));
	}
	let (subtitle, state) = maybe(
		skipped_blanks().preceding(
			specific_symbol('#')
				.preceding(specific_symbol(':'))
				.preceding(cut(after_spaces(get_range(paragraph_string)))),
		),
	)
	.parse(state)?;

	let state = state.below_scope();

	let (content, state) = zero_or_more(after_blanks(
		some_child_tag
			.map(|x| Paragraph(vec![x.into()]))
			.or(not(peek(specific_symbol('#'))).preceding(paragraph_string)),
	))
	.parse(state)?;

	let state = state.above_scope();

	let section = Section {
		depth,
		name: title,
		subtitle,
		content,
	};

	Ok((section, state))
}

fn content_macro(state: State<'_>) -> ParserResult<'_, Content> {
	let parser = get_range(specific_literal("content"))
		.followed_by(after_spaces(macro_mark))
		.map(|x| Content {
			position: x.range,
			content: None,
		});
	let (result, state) = parser.parse(state)?;
	Ok((result, state))
}

fn doctype(state: State<'_>) -> ParserResult<'_, String> {
	specific_symbol('!')
		.preceding(cut(after_spaces(specific_literal("doctype"))
			.preceding(after_spaces(literal.map(ToString::to_string)))))
		.parse(state)
}

fn plug_call(state: State<'_>) -> ParserResult<'_, Box<PlugCall>> {
	let parser = plugin_head.and_maybe(plugin_body);

	let (((name, parameters), body), state) = parser.parse(state)?;

	// let body = state.engine.call_plugin(&name, parameters, body)?;

	let input = PluginInput {
		parameters,
		body,
		current_file: state.file_path.clone(),
	};

	let (body, state) = match state.engine.call_plugin(&name, input, state.current_file) {
		Ok(x) => (x, state),
		Err(x) => (vec![], state.with_error(x)),
	};

	Ok((Box::new(PlugCall { name, body }), state))
}

fn macro_call(state: State<'_>) -> ParserResult<'_, Macro> {
	let parser = macro_call_head.and_maybe(tag_body);

	let (((name, arguments), body), state) = parser.parse(state)?;
	Ok((
		Macro {
			name,
			arguments,
			body: body.unwrap_or_default(),
		},
		state,
	))
}

fn macro_def(state: State<'_>) -> ParserResult<'_, Macro> {
	let parser = macro_def_head.and_maybe(tag_body);

	let (((name, arguments), body), state) = parser.parse(state)?;
	Ok((
		Macro {
			name,
			arguments,
			body: body.unwrap_or(vec![]),
		},
		state,
	))
}

fn space(state: State) -> ParserResult<&char> {
	match any.parse(state)? {
		(Token::Space { content: space, .. }, next_state) => Ok((space, next_state)),
		(_, next_state) => Err(ParseError::NotASpace.error_at(&next_state)),
	}
}

fn indent(state: State) -> ParserResult<&char> {
	match any.parse(state)? {
		(
			Token::Indent {
				content: indent, ..
			},
			next_state,
		) => Ok((indent, next_state)),
		(_, error_state) => Err(ParseError::NotAnIndent.error_at(&error_state)),
	}
}

fn newline(state: State) -> ParserResult<&char> {
	match any.parse(state)? {
		(
			Token::Newline {
				content: newline, ..
			},
			next_state,
		) => ParserResult::Ok((newline, next_state)),
		(_, error_state) => Err(ParseError::NotANewline.error_at(&error_state)),
	}
}

fn some_symbol(state: State) -> ParserResult<&char> {
	match any.parse(state)? {
		(Token::Symbol { content: x, .. }, next_state) => Ok((x, next_state)),
		(_, state) => Err(ParseError::NotSymbol.error_at(&state)),
	}
}

fn eof(state: State) -> ParserResult<()> {
	match any.parse(state.clone()) {
		Ok(_) => Err(ParseError::ExpectedEOF.error_at(&state)),
		Err(_) => Ok(((), state)),
	}
}

fn literal(state: State) -> ParserResult<&str> {
	match any.parse(state)? {
		(Token::Word { content: x, .. }, next_state) => Ok((x, next_state)),
		(_, state) => Err(ParseError::NotLiteral.error_at(&state)),
	}
}

fn non_macro_starter(state: State) -> ParserResult<&str> {
	literal
		.set_err(|| ParseError::ExpectedTagName)
		.is(|x| x != &"macro" && x != &"if" && x != &"for")
		.set_err(|| ParseError::UnexpectedMacroDef)
		.parse(state)
}

fn var_def_starter(state: State) -> ParserResult<&str> {
	(literal.is(|x| x == &"const"))
		.set_err(|| ParseError::ExpectedLambdaStart)
		.parse(state)
}

fn lambda_def_starter(state: State) -> ParserResult<&str> {
	(literal.is(|x| x == &"mut"))
		.set_err(|| ParseError::ExpectedLambdaStart)
		.parse(state)
}

fn macro_starter(state: State) -> ParserResult<&str> {
	literal
		.set_err(|| ParseError::ExpectedTagNameOrMacroDef)
		.is(|x| x == &"macro")
		.set_err(|| ParseError::NotMacroStart)
		.parse(state)
}

fn tag_head(state: State) -> ParserResult<(Ranged<String>, Vec<Attribute>, Vec<HtmlTag>)> {
	let cut_cond = space
		.or(indent)
		.or(body_opener)
		.or(tag_closer_superficial)
		.or(tag_opener)
		.or(subtag_opener);
	let parser = get_range(non_macro_starter)
		.followed_by(peek(cut_cond))
		.and_also(cut(zero_or_more(after_spaces(attribute))))
		.and_also(zero_or_more(after_spaces(subtag)));

	let (((name, attributes), subtags), state) = parser.parse(state)?;

	Ok(((name.to_own(), attributes, subtags), state))
}

fn plugin_head(state: State) -> ParserResult<(Ranged<String>, Ranged<Vec<Token>>)> {
	let parser = get_range(non_macro_starter)
		.followed_by(plugin_mark)
		.followed_by(skip_spaces());

	let (name, mut state) = parser.parse(state)?;
	let start = state.get_end_position();
	let mut tokens = Vec::new();
	let mut escape = false;

	while let Some(token) = state.first_token() {
		match token {
			Token::Symbol {
				content: symbol, ..
			} if symbol == &'\\' && !escape => {
				escape = true;
				state = state.next_state();
			}
			Token::Symbol { content: x, .. }
				if !escape && (x == &'>' || x == &'|' || x == &':') =>
			{
				let end = state.get_end_position();
				return Ok((
					(
						name.to_own(),
						Ranged {
							value: tokens,
							range: types::MultilineRange::Range(start, end),
						},
					),
					state,
				));
			}
			Token::Newline { .. } => return Err(ParseError::ExpectedBodyOpener.error_at(&state)),
			tok => {
				escape = false;
				tokens.push(tok.clone());
				state = state.next_state();
			}
		}
	}

	Err(ParseError::EndlessString.error_at(&state).cut())
}

fn macro_call_head(state: State) -> ParserResult<(Ranged<String>, Vec<Argument>)> {
	let parser = get_range(macro_name)
		.followed_by(macro_mark)
		.and_also(cut(zero_or_more(skip_spaces().preceding(argument))));

	let ((name, attributes), state) = parser.parse(state)?;

	Ok(((name.to_own(), attributes), state))
}

fn macro_def_head(state: State) -> ParserResult<(Ranged<String>, Vec<Argument>)> {
	let parser = after_spaces(macro_starter).preceding(
		cut(after_spaces(get_range(literal))).and_also(zero_or_more(after_spaces(argument))),
	);

	let ((name, attributes), state) = parser.parse(state)?;

	Ok(((name.to_own(), attributes), state))
}

fn skip_spaces<'a>() -> impl Parser<'a, Vec<&'a char>> {
	zero_or_more(space.or(indent))
}

fn after_spaces<'a, T1, P>(parser: P) -> impl Parser<'a, T1>
where
	P: Parser<'a, T1> + 'a,
	T1: 'a,
{
	skip_spaces().preceding(parser)
}

fn after_blanks<'a, T1, P>(parser: P) -> impl Parser<'a, T1>
where
	P: Parser<'a, T1> + 'a,
	T1: 'a,
{
	skipped_blanks().preceding(parser)
}

fn skipped_blanks<'a>() -> impl Parser<'a, Vec<&'a char>> {
	zero_or_more(space.or(indent).or(newline))
}

fn skip_newline_blanks<'a>() -> impl Parser<'a, Vec<&'a char>> {
	zero_or_more(
		skip_spaces()
			.preceding(newline)
			.followed_by(skipped_blanks()),
	)
}

fn tag_body(state: State) -> ParserResult<Vec<HtmlNodes>> {
	let parser = skip_spaces().preceding(body_opener).preceding(
		cut(skipped_blanks()).preceding(zero_or_more(
			skip_newline_blanks()
				.preceding(section_block.map(|x| HtmlNodes::HtmlTag(Section::into_tag(x))))
				.or(paragraph_string.map(|x| match (x.0.first(), x.0.len()) {
					(Some(HtmlNodes::String(_)), _) => HtmlNodes::Paragraph(x),
					(Some(x), 1) => x.clone(),
					_ => HtmlNodes::Paragraph(x),
				}))
				.or(skipped_blanks().preceding(some_child_tag.map(Into::into))),
		)),
	);

	parser.parse(state)
}

fn plugin_body(state: State) -> ParserResult<Ranged<Vec<Token>>> {
	let parser = skip_spaces().preceding(body_opener);
	let (_, mut state) = parser.parse(state)?;

	let start = state.get_end_position();
	let mut tokens = Vec::new();
	let mut escape = false;

	while let Some(token) = state.first_token() {
		match token {
			Token::Symbol {
				content: symbol, ..
			} if symbol == &'\\' && !escape => {
				escape = true;
				state = state.next_state();
			}
			Token::Symbol { content: x, .. } if !escape && (x == &'>') => {
				let end = state.get_end_position();
				return Ok((
					Ranged {
						value: tokens,
						range: types::MultilineRange::Range(start, end),
					},
					state,
				));
			}
			tok => {
				escape = false;
				tokens.push(tok.clone());
				state = state.next_state();
			}
		}
	}

	Err(ParseError::EndlessString.error_at(&state).cut())
}

fn string_tagless_content<'a>() -> impl Parser<'a, StringParts> {
	specific_symbol('\\')
		.preceding(any.map(|x| StringParts::String(x.to_string())))
		.or(specific_symbol('@').preceding(get_range(expression).map(StringParts::Expression)))
		.or(any.map(|x| StringParts::String(x.to_string())))
}

fn string_tagless(state: State) -> ParserResult<Vec<StringParts>> {
	let terminator = ignore(newline)
		.or(ignore(tag_opener))
		.or(ignore(tag_closer_superficial))
		.or(not(any));
	let parser = maybe_until(string_tagless_content(), terminator, false);
	parser.parse(state)
}

fn attr_string(state: State) -> ParserResult<Vec<StringParts>> {
	let (quote_mark, state) = quote_mark.parse(state)?;
	let terminator = newline.or(specific_symbol(*quote_mark));
	let parser = cut(maybe_until(string_tagless_content(), terminator, true)
		.followed_by(specific_symbol(*quote_mark)));
	parser.parse(state)
}

fn paragraph_string(state: State) -> ParserResult<Paragraph> {
	let inside = string_tagless
		.map(HtmlNodes::String)
		.or(some_child_tag.map(Into::into));

	let terminator = ignore(newline)
		.or(not(any))
		.or(ignore(tag_closer_superficial));

	after_blanks(inside.maybe_until(terminator, false))
		.map(Paragraph)
		.parse(state)
}

fn subtag(state: State) -> ParserResult<HtmlTag> {
	let parser = subtag_opener.preceding(
		cut(after_spaces(get_range(literal)))
			.and_also(zero_or_more(skip_spaces().preceding(attribute))),
	);
	let ((name, attributes), state) = parser.parse(state)?;
	Ok((
		HtmlTag {
			name: name.to_own(),
			attributes,
			subtags: vec![],
			body: vec![],
		},
		state,
	))
}

fn attribute(state: State) -> ParserResult<Attribute> {
	let parser = get_range(attr_name)
		.followed_by(skip_spaces())
		.and_also(cut(equals.preceding(
			zero_or_more(space.or(indent)).preceding(get_range(expression)),
		)));
	let ((name, value), state) = parser.parse(state)?;
	Ok((Attribute { name, value }, state))
}

fn argument(state: State) -> ParserResult<Argument> {
	let parser = get_range(attr_name)
		.followed_by(zero_or_more(space.or(indent)))
		.and_maybe(equals.preceding(cut(
			zero_or_more(space.or(indent)).preceding(get_range(expression)),
		)));
	let ((name, value), state) = parser.parse(state)?;
	Ok((Argument { name, value }, state))
}

fn statement(state: State) -> ParserResult<BodyNodes> {
	let parser = lambda_definition
		.map(BodyNodes::LambdaDef)
		.or(variable_definition.map(BodyNodes::VarDef))
		.or(set_stmt.map(|(x, y)| BodyNodes::SetStmt(x, y)))
		.set_err(|| ParseError::ExpectedStatement);

	parser.parse(state)
}

pub(crate) fn file(
	tokens_id: KisTokenId,
	engine: &Kismesis,
	default_template: Option<KisTemplateId>,
	file_path: Option<PathBuf>,
) -> Result<ParsedFile, Vec<Err>> {
	let parser = zero_or_more(
		skipped_blanks().preceding(
			some_tag
				.map(Into::into)
				.or(specific_symbol('$').preceding(after_spaces(cut(statement))))
				.or(section_block.map(|x| BodyNodes::HtmlTag(Section::into_tag(x))))
				.or(peek(cut(
					not(statement).set_err(|| ParseError::SuspiciousStmtString)
				))
				.preceding(paragraph_string.map(BodyNodes::Paragraph))),
		),
	)
	.followed_by(check_tag_mismatch)
	.followed_by(skipped_blanks())
	.followed_by(eof.or(ignore(tag_closer)));

	let state = State::new(
		tokens_id,
		&engine
			.get_file(tokens_id)
			.expect("Tried to parse a file that does not exist")
			.tokens,
		file_path,
		engine,
	);

	let mut ast_nodes = match parser.parse(state) {
		Ok((val, final_state)) => {
			if final_state.errors.is_empty() {
				val
			} else {
				return Err(final_state.errors);
			}
		}
		Err(err) => {
			drop(parser);
			return Err(vec![err]);
		}
	};
	drop(parser);
	semantic_check(&mut ast_nodes, tokens_id)?;
	let ast_nodes = ast_nodes;

	let mut output = ParsedFile::new(tokens_id);

	output.template = default_template;
	for node in ast_nodes {
		match node {
			BodyNodes::HtmlTag(tag) => output.body.push(TopNodes::HtmlTag(tag)),
			BodyNodes::MacroDef(mac) => output.defined_macros.push(mac),
			BodyNodes::MacroCall(mac) => output.body.push(TopNodes::MacroCall(mac)),
			BodyNodes::LambdaDef(lambda) => output.defined_lambdas.push(lambda),
			BodyNodes::VarDef(var) => output.defined_variables.push(var),
			BodyNodes::PlugCall(plug) => output.body.push(TopNodes::PlugCall(plug)),
			BodyNodes::Content(x) => output.body.push(TopNodes::Content(x)),
			BodyNodes::Doctype(x) => output.body.push(TopNodes::Doctype(x)),
			BodyNodes::If(x) => output.body.push(TopNodes::If(x)),
			BodyNodes::For(x) => output.body.push(TopNodes::For(x)),
			BodyNodes::Raw(x) => output.body.push(TopNodes::Raw(x)),
			BodyNodes::Paragraph(x) => output.body.push(TopNodes::Paragraph(x)),
			BodyNodes::SetStmt(config, value) => match config.as_str() {
				"template" => {
					let mut value = PathBuf::from(value);
					value.set_extension("ks");
					match engine.verify_template_id(value) {
						Some(template) => output.template = Some(template),
						None => panic!("Template does not exist"),
					}
				}
				_ => panic!("Non-existant config"),
			},
		}
	}

	Ok(output)
}

/// Verify that the semantics of the HTML tags at the top of the AST are done correctly
fn semantic_check(nodes: &mut [BodyNodes], scope: KisTokenId) -> Result<(), Vec<Err>> {
	let mut errors = vec![];

	let initial_semantics = Semantics::new(scope);

	for node in nodes.iter_mut() {
		if let Err(ref mut x) = node.check_semantics(&initial_semantics) {
			errors.append(x);
		}
	}

	if errors.is_empty() {
		Ok(())
	} else {
		Err(errors)
	}
}

// Generators
pub(super) fn specific_symbol<'a>(chr: char) -> impl Parser<'a, &'a char> {
	move |state: State<'a>| match some_symbol.parse(state.clone()) {
		Ok((x, next_state)) if x == &chr => Ok((x, next_state)),
		Ok((x, _)) => Err(ParseError::CharacterNotMatch {
			expected: chr,
			got: Some(*x),
		}
		.error_at(&state)),
		Err(error) => Err(error),
	}
}
pub(super) fn specific_literal<'a>(word: &'a str) -> impl Parser<'a, &'a str> {
	move |state: State<'a>| match literal.parse(state.clone()) {
		Ok((x, next_state)) if x == word => Ok((x, next_state)),
		Ok((x, _)) => Err(ParseError::LiteralNotMatch {
			expected: word.to_string(),
			got: Some(x.to_string()),
		}
		.error_at(&state)),
		Err(error) => Err(error),
	}
}

fn any(state: State) -> ParserResult<&Token> {
	match state.advanced() {
		(Some(token), next_state) => Ok((token, next_state)),
		(None, _) => Err(ParseError::ReachedEOF.error_at(&state)),
	}
}

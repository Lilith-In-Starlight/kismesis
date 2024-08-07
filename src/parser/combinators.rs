use std::{
	fmt::Debug,
	ops::{Bound, RangeBounds},
};

use super::{
	errors::{Err, ParseError, Unpack},
	state::State,
	types::{MultilineRange, Ranged},
	Parser, ParserResult,
};

pub(super) fn preceding<'a, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<'a, O2>
where
	P1: Parser<'a, O1> + 'a,
	P2: Parser<'a, O2> + 'a,
	O1: 'a,
	O2: 'a,
{
	p1.and_also(p2).map(|(_, y)| y)
}

pub(super) fn ignore<'a, P1, O1>(p1: P1) -> impl Parser<'a, ()>
where
	P1: Parser<'a, O1>,
{
	move |state| match p1.parse(state) {
		Ok((_, next_state)) => Ok(((), next_state)),
		Err(error) => Err(error),
	}
}

pub(super) fn maybe_until<'a, P1, O1, P2, O2>(
	p1: P1,
	p2: P2,
	allow_empty: bool,
) -> impl Parser<'a, Vec<O1>>
where
	P1: Parser<'a, O1>,
	P2: Parser<'a, O2>,
{
	move |mut state: State<'a>| {
		let mut found = Vec::new();
		loop {
			match p2.parse(state.clone()) {
				Ok((_, next_state)) => {
					if found.is_empty() && !allow_empty {
						return Err(ParseError::EmptyString.error_at(&next_state));
					}
					return Ok((found, state));
				}
				Err(_) => match p1.parse(state.clone()) {
					Ok((val, next_state)) => {
						found.push(val);
						state = next_state;
					}
					Err(_) => return Err(ParseError::ConditionUnmet.error_at(&state)),
				},
			}
		}
	}
}

pub(super) fn and_also<'a, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<'a, (O1, O2)>
where
	P1: Parser<'a, O1>,
	P2: Parser<'a, O2>,
{
	move |state| match p1.parse(state) {
		Ok((first_result, next_state)) => match p2.parse(next_state) {
			Ok((second_result, next_state)) => Ok(((first_result, second_result), next_state)),
			Err(err) => Err(err),
		},
		Err(error) => Err(error),
	}
}
pub(super) fn and_maybe<'a, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<'a, (O1, Option<O2>)>
where
	P1: Parser<'a, O1>,
	P2: Parser<'a, O2>,
{
	move |state| match p1.parse(state) {
		ParserResult::Ok((first_result, next_state)) => match p2.parse(next_state.clone()) {
			Ok((second_result, next_state)) => {
				Ok(((first_result, Some(second_result)), next_state))
			}
			Err(Err::Error(_)) => Ok(((first_result, None), next_state)),
			Err(x) => Err(x),
		},
		Err(error) => Err(error),
	}
}
pub(super) fn followed_by<'a, P1, O1, P2, O2>(p1: P1, p2: P2) -> impl Parser<'a, O1>
where
	P1: Parser<'a, O1> + 'a,
	P2: Parser<'a, O2> + 'a,
	O1: 'a,
	O2: 'a,
{
	p1.and_also(p2).map(|(x, _)| x)
}

pub(super) fn or<'a, P1, O1, P2>(p1: P1, p2: P2) -> impl Parser<'a, O1>
where
	P1: Parser<'a, O1>,
	P2: Parser<'a, O1>,
{
	move |state: State<'a>| match p1.parse(state.clone()) {
		Ok((result, next_state)) => Ok((result, next_state)),
		Err(Err::Failure(x)) => Err(Err::Failure(x)),
		Err(_) => p2.parse(state),
	}
}

pub(super) fn zero_or_more<'a, P, T>(parser: P) -> impl Parser<'a, Vec<T>>
where
	P: Parser<'a, T>,
{
	repeated(parser, 0..)
}

pub(super) fn repeated<'a, P, T>(
	parser: P,
	range: impl RangeBounds<usize>,
) -> impl Parser<'a, Vec<T>>
where
	P: Parser<'a, T>,
{
	move |state: State<'a>| {
		let mut state = state;
		let mut found = Vec::<T>::new();
		loop {
			match parser.parse(state.clone()) {
				Ok((token, next_state)) => {
					state = next_state;
					found.push(token);
				}
				Err(Err::Failure(x)) => return Err(Err::Failure(x)),
				_ => break,
			}
		}
		if range.contains(&found.len()) {
			Ok((found, state))
		} else {
			let start = match range.start_bound() {
				Bound::Included(x) => Bound::Included(*x),
				Bound::Excluded(x) => Bound::Excluded(*x),
				Bound::Unbounded => Bound::<usize>::Unbounded,
			};
			let end = match range.end_bound() {
				Bound::Included(x) => Bound::Included(*x),
				Bound::Excluded(x) => Bound::Excluded(*x),
				Bound::Unbounded => Bound::<usize>::Unbounded,
			};

			Err(ParseError::NotInRange(start, end).error_at(&state))
		}
	}
}
pub(super) fn peek<'a, P, T>(parser: P) -> impl Parser<'a, T>
where
	P: Parser<'a, T>,
{
	move |state: State<'a>| {
		let (val, _) = parser.parse(state.clone())?;
		Ok((val, state))
	}
}

#[allow(dead_code)]
pub(super) fn dbg<'a, P, T: Debug>(parser: P) -> impl Parser<'a, T>
where
	P: Parser<'a, T>,
{
	move |state: State<'a>| {
		let r = parser.parse(state);
		println!("{r:#?}");
		r
	}
}

pub(super) fn cut<'a, P, T>(parser: P) -> impl Parser<'a, T>
where
	P: Parser<'a, T>,
{
	move |state: State<'a>| match parser.parse(state) {
		Err(Err::Error(x)) => Err(Err::Failure(x)),
		pat => pat,
	}
}

pub(super) fn not<'a, P, T>(parser: P) -> impl Parser<'a, ()>
where
	P: Parser<'a, T>,
{
	move |state: State<'a>| match parser.parse(state.clone()) {
		Err(Err::Error(_)) => Ok(((), state)),
		Err(Err::Failure(x)) => Err(Err::Failure(x)),
		Ok((_, state)) => Err(ParseError::ConditionUnmet.error_at(&state)),
	}
}

pub(super) fn maybe<'a, P, T>(parser: P) -> impl Parser<'a, Option<T>>
where
	P: Parser<'a, T>,
{
	move |state: State<'a>| match parser.parse(state.clone()) {
		Err(Err::Error(_)) => Ok((None, state)),
		Err(Err::Failure(x)) => Err(Err::Failure(x)),
		Ok((x, state)) => Ok((Some(x), state)),
	}
}

pub(super) fn map<'a, P, F, T1, T2>(parser: P, fun: F) -> impl Parser<'a, T2>
where
	P: Parser<'a, T1>,
	F: Fn(T1) -> T2,
{
	move |state: State<'a>| parser.parse(state).map(|(val, state)| (fun(val), state))
}

pub(super) fn change_err<'a, P, F, T1>(parser: P, fun: F) -> impl Parser<'a, T1>
where
	P: Parser<'a, T1>,
	F: Fn() -> ParseError,
{
	move |state: State<'a>| match parser.parse(state) {
		x @ Ok(_) => x,
		Err(x) => {
			let failure = matches!(x, Err::Failure(_));
			let mut x = x.unpack();
			if failure {
				Err(Err::Failure(x))
			} else {
				*x.error.unpack_mut() = fun();
				Err(Err::Error(x))
			}
		}
	}
}

pub(super) fn is<'a, P, F, T1>(parser: P, fun: F) -> impl Parser<'a, T1>
where
	P: Parser<'a, T1>,
	F: Fn(&T1) -> bool,
	T1: 'a,
{
	move |state: State<'a>| {
		let (val, state) = match parser.parse(state) {
			Ok(x) => x,
			Err(x) => return Err(x),
		};
		if fun(&val) {
			Ok((val, state))
		} else {
			Err(ParseError::ConditionUnmet.error_at(&state))
		}
	}
}

pub(super) fn get_range<'a, P, T1>(parser: P) -> impl Parser<'a, Ranged<T1>>
where
	P: Parser<'a, T1>,
{
	move |state: State<'a>| {
		let start = state.get_start_position();
		let (val, next_state) = parser.parse(state)?;
		let end = next_state.get_end_position();
		let range = {
			if end.get_idx() == start.get_idx() + 1 {
				MultilineRange::Single(start)
			} else {
				MultilineRange::Range(start, end)
			}
		};
		Ok((Ranged { value: val, range }, next_state))
	}
}

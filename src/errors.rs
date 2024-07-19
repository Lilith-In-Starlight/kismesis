//! Data structures representing every possible failure during Kismesis' runtime.

use crate::{html::MaybeUnscoped, KisTokenId};

use super::{
	html::ScopedError,
	parser::{
		errors::{Hint, Hintable},
		types::MultilineRange,
	},
};

pub trait ErrorKind
where
	Self: Sized,
{
	fn get_text(&self) -> String;
	fn with_state_at(self, position: MultilineRange) -> ErrorState<Self> {
		ErrorState {
			error: self,
			hints: vec![],
			text_position: position,
		}
	}
	fn with_scope_at(self, scope: KisTokenId, position: MultilineRange) -> MaybeUnscoped<Self> {
		ScopedError {
			error: self.with_state_at(position).into(),
			scope,
		}
		.into()
	}
	fn stateless(self) -> StatelessError<Self> {
		StatelessError {
			error: self,
			hints: vec![],
		}
	}

	fn unscoped(self) -> MaybeUnscoped<Self> {
		MaybeUnscoped::Unscoped(self.stateless())
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum MaybeStateless<T> {
	Stateful(ErrorState<T>),
	Statelss(StatelessError<T>),
}

impl<T: ErrorKind> ErrorKind for MaybeStateless<T> {
	fn get_text(&self) -> String {
		match self {
			Self::Stateful(x) => x.get_text(),
			Self::Statelss(x) => x.get_text(),
		}
	}
}

impl<T: ErrorKind> ErrorKind for ErrorState<T> {
	fn get_text(&self) -> String {
		self.error.get_text()
	}
}

impl<T> Hintable for MaybeStateless<T> {
	fn add_hint(&mut self, hint: Hint) {
		match self {
			Self::Stateful(x) => x.hints.push(hint),
			Self::Statelss(x) => x.hints.push(hint),
		}
	}

	fn get_hints(&self) -> &[Hint] {
		match self {
			Self::Stateful(x) => &x.hints,
			Self::Statelss(x) => &x.hints,
		}
	}

	fn get_hints_mut(&mut self) -> &mut [Hint] {
		match self {
			Self::Stateful(x) => &mut x.hints,
			Self::Statelss(x) => &mut x.hints,
		}
	}
}

impl<T> From<ErrorState<T>> for MaybeStateless<T> {
	fn from(value: ErrorState<T>) -> Self {
		Self::Stateful(value)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct StatelessError<T> {
	pub error: T,
	pub hints: Vec<Hint>,
}

impl<T: ErrorKind> ErrorKind for StatelessError<T> {
	fn get_text(&self) -> String {
		self.error.get_text()
	}
}

impl<T> Hintable for ErrorState<T> {
	fn add_hint(&mut self, hint: Hint) {
		self.hints.push(hint);
	}

	fn get_hints(&self) -> &[Hint] {
		&self.hints
	}

	fn get_hints_mut(&mut self) -> &mut [Hint] {
		&mut self.hints
	}
}

impl<T> Hintable for StatelessError<T> {
	fn add_hint(&mut self, hint: Hint) {
		self.hints.push(hint);
	}

	fn get_hints(&self) -> &[Hint] {
		&self.hints
	}

	fn get_hints_mut(&mut self) -> &mut [Hint] {
		&mut self.hints
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorState<T> {
	pub error: T,
	pub text_position: MultilineRange,
	pub hints: Vec<Hint>,
}

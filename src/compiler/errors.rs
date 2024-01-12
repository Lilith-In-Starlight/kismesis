use crate::KisID;

use super::{
	html::ScopedError,
	parser::{
		errors::{Hint, Hintable},
		types::TextPos,
	},
};

pub trait ErrorKind
where
	Self: Sized,
{
	fn get_text(&self) -> String;
	fn with_state_at(self, position: TextPos) -> ErrorState<Self> {
		ErrorState {
			error: self,
			hints: vec![],
			text_position: position,
		}
	}
	fn with_scope_at(self, scope: KisID, position: TextPos) -> ScopedError<Self> {
		ScopedError {
			error: self.with_state_at(position),
			scope,
		}
	}
	fn stateless(self) -> StatelessError<Self> {
		StatelessError {
			error: self,
			hints: vec![],
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct StatelessError<T> {
	pub error: T,
	pub hints: Vec<Hint>,
}

impl<T> Hintable for ErrorState<T> {
	fn add_hint(&mut self, hint: Hint) {
		self.hints.push(hint);
	}
}

impl<T> Hintable for StatelessError<T> {
	fn add_hint(&mut self, hint: Hint) {
		self.hints.push(hint);
	}
}

pub trait Reportable {}

#[derive(Clone, Debug, PartialEq)]
pub struct ErrorState<T> {
	pub error: T,
	pub text_position: TextPos,
	pub hints: Vec<Hint>,
}

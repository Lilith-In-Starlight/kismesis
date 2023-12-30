use super::parser::types::TextPos;

pub trait ErrorKind {
	fn get_text(&self) -> String;
}

#[derive(Clone, Debug)]
pub struct ErrorState<T> {
	pub error: T,
	pub previous_errors: Vec<ErrorState<T>>,
	pub text_position: TextPos,
}

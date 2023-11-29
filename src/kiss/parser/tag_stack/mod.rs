pub(crate) mod elements;
pub(crate) mod errors;
use elements::{Tag, ContentTag};

use crate::errors::ParsingError;

use self::{errors::TagStackError, elements::{Macro, ContentChildTag}};

use super::TokenScanner;

#[derive(Debug)]
pub struct TagStack {
	pub content: Vec<Tag>,
}


impl TagStack {
	pub fn new() -> Self {
		Self {
			content: Vec::new(),
		}
	}
	pub fn push(&mut self, o: Tag) {
		self.content.push(o)
	}
	pub fn new_content_tag(&mut self, scanner: &TokenScanner) {
		self.content.push(ContentTag::new(scanner).into())
	}
	pub fn new_macro_call(&mut self, scanner: &TokenScanner) {
		self.push(Macro::new_call(scanner))
	}
	pub fn new_macro_def(&mut self, scanner: &TokenScanner) -> Result<(), ParsingError> {
		if self.content.is_empty() {
			self.push(Macro::new_def(scanner));
			Ok(())
		} else {
			Err(ParsingError::TriedMacroDefInTag)
		}
	}

	pub fn last_mut(&mut self) -> Option<&mut Tag> { self.content.last_mut() }
	pub fn last(&self) -> Option<&Tag> { self.content.last() }

	pub fn pop<T: From<Tag>>(&mut self) -> Option<T> { self.content.pop().map(|x| x.into()) }

	pub fn merge(&mut self) -> Result<(), TagStackError> {
		if self.content.len() == 1 { return Err(TagStackError::HadOneTag) }

		let current_tag: ContentChildTag = match self.content.pop() {
			Some(Tag::MacroDef(_)) => return Err(TagStackError::CantMergeFromMacroDef),
			Some(Tag::MacroCall(x)) => x.into(),
			Some(Tag::ContentTag(x)) => x.into(),
			None => return Err(TagStackError::WasEmpty)
		};
	
		match self.last_mut() {
			Some(x) => match x {
				Tag::MacroCall(x) | Tag::MacroDef(x) => x.add_child(current_tag),
				Tag::ContentTag(x) => x.add_child(current_tag),
			},
			None => return Err(TagStackError::HadOneTag),
		};
		Ok(())
	}
}

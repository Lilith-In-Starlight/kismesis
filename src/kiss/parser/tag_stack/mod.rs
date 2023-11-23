pub(crate) mod elements;
pub(crate) mod errors;
use elements::BodyElems;

use self::errors::TagStackError;

#[derive(Debug)]
pub struct TagStack {
	pub content: Vec<BodyElems>,
}

impl TagStack {
	pub fn new() -> Self {
		Self { content: vec![] }
	}
	pub fn add_new_content_tag(&mut self) { self.content.push(BodyElems::new_content_tag()) }

	pub fn last_mut(&mut self) -> Option<&mut BodyElems> { self.content.last_mut() }
	pub fn last(&self) -> Option<&BodyElems> { self.content.last() }

	pub fn pop(&mut self) -> Option<BodyElems> { self.content.pop() }

	pub fn merge(&mut self) -> Result<(), TagStackError> {
		if self.content.len() == 1 { return Err(TagStackError::HadOneTag) }

		let current_tag = match self.content.pop() {
			Some(x) => {
				match x {
					BodyElems::ContentTag {..} => x,
					BodyElems::MacroCall {..} => x,
					_ => return Err(TagStackError::NonMergeableTopTag)
				}
			},
			None => return Err(TagStackError::WasEmpty)
	
		};
	
		match self.last_mut() {
			Some(x) => {
				match x {
					BodyElems::ContentTag { children , ..} => children.push(current_tag),
					BodyElems::MacroCall { children , ..} => children.push(current_tag),
					_ => return Err(TagStackError::NonMergeableTopTags)
				}
			},
			None => return Err(TagStackError::HadOneTag),
		};
		Ok(())
	}
}

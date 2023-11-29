use crate::errors::{ParsingError, ImplementationError};

pub enum TagStackError {
	WasEmpty,
	HadOneTag,
	HadNonTag,
	CantMergeFromMacroDef,
}

impl From<TagStackError> for ParsingError {
	fn from(val: TagStackError) -> ParsingError {
		match val {
			TagStackError::WasEmpty => ImplementationError::ImpossibleEmpty.into(),
			TagStackError::HadOneTag => ImplementationError::UnmetExpectancy.into(),
			TagStackError::CantMergeFromMacroDef => ParsingError::TriedMacroDefMerge,
			TagStackError::HadNonTag => ImplementationError::ImpossibleNotTag.into(),
		}
	}
}

use crate::errors::{KismesisError, UnrecoverableError};

pub enum TagStackError {
	WasEmpty,
	HadOneTag,
	HadNonTag,
	CantMergeFromMacroDef,
}

impl From<TagStackError> for KismesisError {
	fn from(val: TagStackError) -> KismesisError {
		match val {
			TagStackError::WasEmpty => UnrecoverableError::ImpossibleEmpty.into(),
			TagStackError::HadOneTag => UnrecoverableError::UnmetExpectancy.into(),
			TagStackError::CantMergeFromMacroDef => KismesisError::TriedMacroDefMerge,
			TagStackError::HadNonTag => UnrecoverableError::ImpossibleNotTag.into(),
		}
	}
}

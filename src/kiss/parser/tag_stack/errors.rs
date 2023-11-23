pub enum TagStackError {
	WasEmpty,
	HadOneTag,
	NonMergeableTopTag,
	NonMergeableTopTags,
	NonParametricTopTag,
	NonBody,
	UnusableLastTag,
}

impl From<TagStackError> for &'static str {
    fn from(value: TagStackError) -> Self {
        match value {
			TagStackError::WasEmpty => "Stack was empty",
			TagStackError::HadOneTag => "Stack had only one tag",
			TagStackError::NonMergeableTopTag => "Tried to merge an element which wasn't mergeable",
			TagStackError::NonMergeableTopTags => "Tried to merge a tag with an element which wasn't mergeable",
			TagStackError::NonParametricTopTag => "Tried to add a parameter to an element which has no parameters",
			TagStackError::NonBody => "Tried to get the body of a bodyless tag",
			TagStackError::UnusableLastTag => "Last tag of the stack was not usable",
		}
    }
}

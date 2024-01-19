use super::types::*;
use super::errors::{Err, Hints, ParseError, Hintable};

#[derive(Clone)]
pub(crate) struct Semantics {
    section_depth: usize,
}

impl Semantics {
    pub(crate) fn new() -> Self {
        Self {
            section_depth: 0,
        }
    }
}

/// This trait is for things whose semantics, or whose children's semantics, must be validated in the
/// semantics checking step.
pub(crate) trait VerifySemantics {
    fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>>;
}


impl VerifySemantics for Macro {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		let mut errors = vec![];

		for tag in self.body.iter_mut() {
			if let Err(ref mut x) = tag.check_semantics(semantics) {
				errors.append(x)
			}
		}

		if errors.is_empty() {
			Ok(())
		} else {
			Err(errors)
		}
	}
}

impl VerifySemantics for PlugCall {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		let mut errors = vec![];

		for tag in self.body.iter_mut() {
			if let Err(ref mut x) = tag.check_semantics(semantics) {
				errors.append(x)
			}
		}

		if errors.is_empty() {
			Ok(())
		} else {
			Err(errors)
		}
	}
}

impl VerifySemantics for HtmlNodes {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		match self {
			Self::HtmlTag(ref mut tag) => tag.check_semantics(semantics),
			Self::MacroCall(ref mut mac) => mac.check_semantics(semantics),
			Self::PlugCall(ref mut plug) => plug.check_semantics(semantics),
			Self::If(ref mut iftag) => iftag.check_semantics(semantics),
			Self::For(ref mut fortag) => fortag.check_semantics(semantics),
			_ => Ok(()),
		}
	}
}


impl VerifySemantics for TopNodes {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		match self {
			Self::HtmlTag(ref mut tag) => tag.check_semantics(semantics),
			Self::MacroCall(ref mut mac) => mac.check_semantics(semantics),
			Self::PlugCall(ref mut plug) => plug.check_semantics(semantics),
			Self::If(ref mut iftag) => iftag.check_semantics(semantics),
			Self::For(ref mut fortag) => fortag.check_semantics(semantics),
			_ => Ok(()),
		}
	}
}


impl VerifySemantics for BodyNodes {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		match self {
			Self::HtmlTag(ref mut tag) => tag.check_semantics(semantics),
			Self::MacroDef(ref mut mac) | Self::MacroCall(ref mut mac) => mac.check_semantics(semantics),
			Self::PlugCall(ref mut plug) => plug.check_semantics(semantics),
			Self::If(ref mut iftag) => iftag.check_semantics(semantics),
			Self::For(ref mut fortag) => fortag.check_semantics(semantics),
			_ => Ok(()),
		}
	}
}

impl VerifySemantics for HtmlTag {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		let mut errors = vec![];
        let mut semantics = semantics.clone();

		// Check the tag's semantis by name
		match self.name.value.as_str() {
			"div" => errors.push(
				ParseError::UsedDiv
					.error_at_pos(self.name.range.clone())
					.with_hint(Hints::DontUseDiv.stateless()),
			),
			"container" => self.name.value = String::from("div"),
			"hgroup" => {
				if let Some(child) = self.body.first() {
					if let HtmlNodes::HtmlTag(child) = child {
						match child.name.value.as_str() {
							"h1" | "h2" | "h3" | "h4" | "h5" | "h6" => (),
							_ => errors.push(ParseError::IncorrectChild("hgroup".to_string())
									.error_at_pos(child.name.range.clone())
									.with_hint(Hints::HgroupContents.stateless()),
							),
						}
					}
				} else {
					errors.push(
						ParseError::ThisTagCannotBeEmpty(self.name.value.clone())
							.error_at_pos(self.name.range.clone())
							.with_hint(Hints::SectionTagContents.stateless()),
					)
				}
			}
			"section" => {
                semantics.section_depth += 1;
				if let Some(child) = self.body.first() {
					if let HtmlNodes::HtmlTag(child) = child {
						match child.name.value.as_str() {
							"h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "hgroup" => (),
							_ => errors.push(
								ParseError::IncorrectChild(self.name.value.clone())
									.error_at_pos(child.name.range.clone())
									.with_hint(Hints::SectionTagContents.stateless()),
							),
						}
					}
				} else {
					errors.push(
						ParseError::ThisTagCannotBeEmpty(self.name.value.clone())
							.error_at_pos(self.name.range.clone())
							.with_hint(Hints::SectionTagContents.stateless()),
					)
				}
			}
			x => {
				let mut chars = x.chars();
				if x.len() >= 2 && chars.next().unwrap() == 'h' {
					match chars.skip(1).collect::<String>().parse::<usize>() {
						Ok(x) if x > 6 => errors.push(
							ParseError::IncorrectHeaderNumber
								.error_at_pos(self.name.range.clone())
								.with_hint(Hints::HeaderForLargeText.stateless()),
						),
                        Ok(x) => if semantics.section_depth != x {
							ParseError::SkippedHeadingLevel(semantics.section_depth)
								.error_at_pos(self.name.range.clone())
								.with_hint(Hints::HeaderForSize.stateless());
                        }
						_ => (),
					}
				}
			}
		}

		for tag in self.body.iter_mut() {
			if let Err(ref mut x) = tag.check_semantics(&semantics) {
				errors.append(x)
			}
		}

		if errors.is_empty() {
			Ok(())
		} else {
			Err(errors)
		}
	}
}

impl VerifySemantics for IfTag {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		let mut errors = vec![];

		for tag in self.body.iter_mut() {
			if let Err(ref mut x) = tag.check_semantics(semantics) {
				errors.append(x)
			}
		}

		if errors.is_empty() {
			Ok(())
		} else {
			Err(errors)
		}
	}
}

impl VerifySemantics for ForTag {
	fn check_semantics(&mut self, semantics: &Semantics) -> Result<(), Vec<Err>> {
		let mut errors = vec![];

		for tag in self.body.iter_mut() {
			if let Err(ref mut x) = tag.check_semantics(semantics) {
				errors.append(x)
			}
		}

		if errors.is_empty() {
			Ok(())
		} else {
			Err(errors)
		}
	}
}

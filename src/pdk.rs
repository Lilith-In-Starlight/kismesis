/// Exports important types as public for Plugin developers to use
pub use super::lexer::Token;
pub use super::parser::types::Argument;
pub use super::parser::types::Attribute;
pub use super::parser::types::ForTag;
pub use super::parser::types::HtmlNodes;
pub use super::parser::types::HtmlTag;
pub use super::parser::types::IfTag;
pub use super::parser::types::Macro;
pub use super::parser::types::Paragraph;
pub use super::parser::types::Ranged;
pub use super::parser::types::Section;
pub use super::parser::types::StringParts;
pub use super::parser::types::TextPos;
pub use super::PushInto;

pub use super::plugins::PluginInput;
pub use super::PluginParseError as PluginError;

pub type RangedTokens = Ranged<Vec<Token>>;
pub type InputTuple = (RangedTokens, Option<RangedTokens>);
pub type AST = Vec<HtmlNodes>;

pub type PlugResult = Result<AST, PluginError>;

impl<T> PushInto<T> for Vec<T> {
	fn push_into<B: Into<T>>(&mut self, value: B) {
		self.push(value.into());
	}
}

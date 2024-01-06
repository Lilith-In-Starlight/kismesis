use rhai::{export_module, exported_module, Module};
use rhai::plugin::*;
use rhai::{Array, Dynamic, Engine};

use crate::compiler::{parser::types::{HtmlNodes, HtmlTag, Expression, Attribute, TextPos, Ranged}, lexer::Token};

// TODO: Change all cast() calls for try_cast()

pub(crate) fn new_engine() -> Engine {
	let mut plugin_engine = Engine::new();
	let exprs_module = exported_module!(exprs);
	plugin_engine.register_static_module("exprs", exprs_module.into());
	let parsers_module = exported_module!(parsers);
	plugin_engine.register_static_module("parsers", parsers_module.into());

	plugin_engine.register_type::<Token>()
		.register_fn("as_str", Token::get_as_string);

	plugin_engine.register_type::<HtmlTag>()
		.register_fn("new_html", dyn_htmltag);

	plugin_engine.register_type::<Expression>();

	plugin_engine.register_type::<Attribute>()
		.register_fn("new_attribute", |name, value| Attribute { value, name });

	plugin_engine.register_type::<TextPos>();

	plugin_engine.register_type::<Ranged<String>>()
		.register_fn("with_range", |range, value: String| Ranged { value, range });
	plugin_engine.register_type::<Ranged<Expression>>()
		.register_fn("with_range", |range, value: Expression| Ranged { value, range });

	plugin_engine
}

pub fn dyn_htmltag (
	name: Ranged<String>,
	attributes: Array,
	body: Array,
	subtags: Array ) -> HtmlTag
{
	HtmlTag {
		name,
		attributes: attributes.into_iter().map(|x| x.cast()).collect(),
		body: body.into_iter().map(|x| x.cast()).collect(),
		subtags: subtags.into_iter().map(|x| x.cast()).collect()
	}
}

pub fn into_html_nodes(output: Array) -> Vec<HtmlNodes> {
	output.into_iter().map(|x| dyn_try_html_node(x).unwrap()).collect()
}

pub fn dyn_try_html_node(object: Dynamic) -> Option<HtmlNodes> {
	object.try_cast::<HtmlTag>().and_then(|x| Some(HtmlNodes::HtmlTag(x)))
}

#[export_module]
mod exprs {
    use crate::compiler::parser::types::StringParts;

	pub fn literal(x: String) -> Expression {
		Expression::Literal(vec![StringParts::String(x)])
	}
}

#[export_module]
mod parsers {
    use crate::{compiler::{lexer::Token, parser::{self, state::ParserState}}, kismesis::Kismesis};

	pub fn get_attributes(params: Vec<Token>) -> Array {
		let x = parser::multiple_attributes(ParserState::new(&params, &Kismesis::new())).unwrap().0;
		x.into_iter().map(|x| Dynamic::from(x)).collect()
	}
}

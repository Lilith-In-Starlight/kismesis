use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use rhai::plugin::*;
use rhai::{export_module, exported_module, Module};
use rhai::{Array, Dynamic, Engine};

use super::compiler::{
	lexer::Token,
	parser::types::{Attribute, Expression, HtmlNodes, HtmlTag, Ranged, TextPos},
};

#[derive(Clone)]
pub(super) struct EngineTag {
	pub(super) project_path: Option<PathBuf>,
}
// TODO: Change all cast() calls for try_cast()

pub(crate) fn new_engine() -> Engine {
	let mut plugin_engine = Engine::new();

	let default_tag = EngineTag {
		project_path: None,
	};

	plugin_engine.set_default_tag(Dynamic::from(Rc::new(RefCell::new(default_tag))));

	plugin_engine.set_max_expr_depths(0, 0);
	
	let exprs_module = exported_module!(exprs);
	plugin_engine.register_static_module("exprs", exprs_module.into());
	let parsers_module = exported_module!(parsers);
	plugin_engine.register_static_module("parsers", parsers_module.into());
	let io_module = exported_module!(io);
	plugin_engine.register_static_module("io", io_module.into());

	plugin_engine
		.register_type::<Token>()
		.register_fn("as_str", Token::get_as_string);

	plugin_engine
		.register_type_with_name::<PathBuf>("Path");

	plugin_engine
		.register_type::<HtmlTag>()
		.register_fn("new_html", dyn_htmltag);

	plugin_engine.register_type::<Expression>();

	plugin_engine
		.register_type::<Attribute>()
		.register_fn("new_attribute", |name, value| Attribute { value, name });

	plugin_engine.register_type::<TextPos>();

	plugin_engine
		.register_type::<Ranged<String>>()
		.register_fn("with_range", |value: String, range| Ranged { value: value.clone(), range });
	plugin_engine
		.register_type::<Ranged<Expression>>()
		.register_fn("with_range", |value: Expression, range| Ranged {
			value: value,
			range,
		});

	plugin_engine
}

pub fn dyn_htmltag(
	name: Ranged<String>,
	attributes: Array,
	body: Array,
	subtags: Array,
) -> HtmlTag {
	HtmlTag {
		name,
		attributes: attributes.into_iter().map(|x| x.cast()).collect(),
		body: body.into_iter().map(|x| x.cast()).collect(),
		subtags: subtags.into_iter().map(|x| x.cast()).collect(),
	}
}

pub fn into_html_nodes(output: Array) -> Vec<HtmlNodes> {
	output
		.into_iter()
		.map(|x| dyn_try_html_node(x).unwrap())
		.collect()
}

pub fn dyn_try_html_node(object: Dynamic) -> Option<HtmlNodes> {
	object
		.try_cast::<HtmlTag>()
		.and_then(|x| Some(HtmlNodes::HtmlTag(x)))
}

#[export_module]
mod exprs {
	use crate::kismesis::compiler::parser::types::StringParts;

	pub fn literal(x: String) -> Expression {
		Expression::Literal(vec![StringParts::String(x)])
	}
}

#[export_module]
mod parsers {
	use crate::kismesis::{
		compiler::{
			lexer::Token,
			parser::{self, state::ParserState},
		},
		Kismesis,
	};

	pub fn get_attributes(ctx: NativeCallContext, params: Vec<Token>) -> Array {
		let tag: Rc<RefCell<EngineTag>> = ctx.engine().default_tag().clone_cast();
		let project_path = tag.borrow().clone().project_path;
		let x = parser::multiple_attributes(ParserState::new(&params, project_path, &Kismesis::new()))
			.unwrap()
			.0;
		x.into_iter().map(|x| Dynamic::from(x)).collect()
	}
}

#[export_module]
mod io {
    use std::fs;

	pub fn read_to_string(ctx: NativeCallContext, path: String) -> Dynamic {
		let path = PathBuf::from(path);
		let tag: Rc<RefCell<EngineTag>> = ctx.engine().default_tag().clone_cast();
		let project_path = tag.borrow().project_path.clone().unwrap();
		if path.canonicalize().unwrap().starts_with(project_path.canonicalize().unwrap()) {
			Dynamic::from(fs::read_to_string(&path).map(|x| Dynamic::from(x)).unwrap_or(Dynamic::UNIT))
		} else {
			panic!("Tried to access illegal path")
		}
	}
}

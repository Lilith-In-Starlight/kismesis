use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

use rhai::{plugin::*, FnPtr};
use rhai::{export_module, exported_module, Module};
use rhai::{Array, Dynamic, Engine};

use super::compiler::{
	lexer::Token,
	parser::types::{Attribute, Expression, HtmlNodes, HtmlTag, Ranged, TextPos},
};

#[derive(Clone)]
struct PluginParseError {
	message: String,
}

/// Simplified parsing API exposed to the plugins
#[derive(Clone)]
pub struct Parser {
	input: Vec<Dynamic>,
	errors: Vec<Dynamic>,
	output: Dynamic,
}

type PluginParseResult = Result<Parser, PluginParseError>;

impl Parser {
	fn any(this: PluginParseResult) -> PluginParseResult {
		let this = this?;
		if this.input.is_empty() {
			return Err(PluginParseError { message: "Reached EOF".to_string() })
		}
		Ok(Self {
			input: this.input[1..].to_vec(),
			output: Dynamic::from(this.input[0].clone()),
			..this
		})
	}
	fn is(ctx: NativeCallContext, this: PluginParseResult, predicate: FnPtr) -> PluginParseResult {
		let this = this?;
		let result = predicate.call_within_context::<bool>(&ctx, (this.output.clone(), ));
		match result {
			Ok(x) if x => Ok(this),
			Ok(_) => Err(PluginParseError { message: "Predicate unsuccessful".to_string() }),
			Err(_) => Err(PluginParseError { message: "Error evaluating is predicate".to_string() })
		}
	}
	fn parse(ctx: NativeCallContext, this: PluginParseResult, predicate: FnPtr) -> PluginParseResult {
		predicate.call_within_context::<PluginParseResult>(&ctx, (this, )).unwrap()
	}
	fn sequence(ctx: NativeCallContext, this: PluginParseResult, sequence: Vec<Dynamic>) -> PluginParseResult {
		let this = this?;
		let sequence = sequence.into_iter().map(|x| x.cast::<FnPtr>());
		let mut values = Vec::new();
		let mut parser = this;
		for (idx, fun) in sequence.into_iter().enumerate() {
			let clone: PluginParseResult = Ok(parser.clone());
			let result = fun.call_within_context::<PluginParseResult>(&ctx, (clone, )).unwrap();
			match result {
				Ok(x) => {
					values.push(x.output.clone());
					parser = x;
				},
				Err(x) => return Err(x)
			}
		}
		Ok(Parser {
			output: Dynamic::from_array(values),
			..parser
		})
	}
	fn get(this: PluginParseResult, idx: i64) -> PluginParseResult {
		let this = this?; 
		let option: Vec<Dynamic> = this.output.cast();
		option.get(idx as usize).map(|x| Parser {
			output: x.clone(),
			..this
		}).ok_or(PluginParseError { message: "Tried ot get out of bounds".to_string() })
	}
	fn map(ctx: NativeCallContext, this: PluginParseResult, predicate: FnPtr) -> PluginParseResult {
		let this = this?; 
		let value: Dynamic = this.output.cast();
		let output: Dynamic = predicate.call_within_context(&ctx, (value, )).unwrap();
		Ok(Parser {
			output,
			..this
		})
	}
	fn map_err(ctx: NativeCallContext, this: PluginParseResult, predicate: FnPtr) -> PluginParseResult {
			this.map_err(|x| predicate.call_within_context(&ctx, (x, )).unwrap())
	}
	fn is_ok(this: PluginParseResult) -> bool {
		this.is_ok()
	}
}

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
	plugin_engine.set_max_call_levels(128);

	plugin_engine.register_type::<PluginParseError>()
		.register_fn("to_string", |x: PluginParseError| x.message)
		.register_fn("error", |message| PluginParseError { message });
	
	plugin_engine.register_type::<Parser>()
		.register_fn("new_parser", |input: Vec<Token>| PluginParseResult::Ok(Parser { input: input.into_iter().map(|x| Dynamic::from(x)).collect(), errors: vec![], output: Dynamic::UNIT }))
		.register_fn("any", Parser::any)
		.register_fn("is_pred", Parser::is)
		.register_fn("sequence", Parser::sequence)
		.register_fn("map", Parser::map)
		.register_fn("map_err", |x: PluginParseResult, message| x.map_err(|_| PluginParseError { message } ))
		.register_fn("map_err", Parser::map_err)
		.register_fn("parse", Parser::parse);

	plugin_engine.register_fn("get", Parser::get);
	plugin_engine.register_fn("is_ok", Parser::is_ok);
	plugin_engine.register_fn("get_value", |input: Result<Parser, PluginParseError>| {
		match input {
			Ok(x) => Dynamic::from(x.output),
			Err(x) => Dynamic::from(x),
		}
	});
	
	let exprs_module = exported_module!(exprs);
	plugin_engine.register_static_module("exprs", exprs_module.into());
	let parsers_module = exported_module!(parsers);
	plugin_engine.register_static_module("parsers", parsers_module.into());
	let io_module = exported_module!(io);
	plugin_engine.register_static_module("io", io_module.into());

	plugin_engine
		.register_type::<Token>()
		.register_fn("to_string", |x| Token::get_as_string(&x))
		.register_fn("is_literal", |x| matches!(x, Token::Word(_)))
		.register_fn("is_symbol", |x| matches!(x, Token::Symbol(_)))
		.register_fn("is_space", |x| matches!(x, Token::Space(_)))
		.register_fn("is_blank", |x| matches!(x, Token::Space(_) | Token::Newline(_) | Token::Indent(_)));
	
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
			value,
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
			lexer::{Token, self},
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

	pub fn tokenize(tokens: String) -> Vec<Token> {
		lexer::tokenize(&tokens)
	}
}

#[export_module]
mod io {
    use std::fs;

	pub fn read_to_string(ctx: NativeCallContext, path: String) -> Dynamic {
		let path = PathBuf::from(path);
		let tag: Rc<RefCell<EngineTag>> = ctx.engine().default_tag().clone_cast();
		let project_path = tag.borrow().clone().project_path.clone().unwrap();
		if path.canonicalize().unwrap().starts_with(project_path.canonicalize().unwrap()) {
			Dynamic::from(fs::read_to_string(&path).map(|x| Dynamic::from(x)).unwrap_or(Dynamic::UNIT))
		} else {
			panic!("Tried to access illegal path")
		}
	}
}

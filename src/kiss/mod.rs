mod ast;


pub fn kiss_to_html(s: &str) {
	println!("{:?}", ast::get_ast(s));
}
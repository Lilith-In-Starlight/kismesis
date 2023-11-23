use std::fs;

mod kiss;


fn main() {
	generate();
}

fn generate() {
	let input_file = fs::read_to_string("test/main.kism");
	match input_file {
		Ok(input_text) => {
			kiss::kiss_to_html(&input_text);
		}
		_ => {
			println!("Failed to load file");
		}
	};
}
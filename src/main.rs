use std::fs;

use crate::errors::report_error;
use colored;

pub mod kiss;
pub mod errors;


fn main() {
	generate();
}

fn generate() {
	let input_file = fs::read_to_string("test/main.kism");
	match input_file {
		Ok(input_text) => {
			println!("{}", match kiss::kiss_to_html(&input_text){
				Ok(x) => x,
				Err((scanner, unrecoverable, recovered)) => report_error(scanner, unrecoverable, recovered),
			});
		}
		_ => {
			println!("Failed to load file");
		}
	};
}

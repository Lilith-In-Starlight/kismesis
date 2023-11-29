use std::fs;

use crate::errors::report_error;

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
				Err(error_report) => report_error(error_report.scanner, error_report.unresolved, error_report.resolved),
			});
		}
		_ => {
			println!("Failed to load file");
		}
	};
}

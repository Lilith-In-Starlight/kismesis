use std::fs;

use crate::errors::report_error;

pub mod kiss;
pub mod errors;


fn main() {
	generate();
}

fn generate() {
	let main_template_file = fs::read_to_string("test/main.kism");
	match main_template_file {
		Ok(main_template_file) => {
			println!("{}", match kiss::kiss_to_html(&main_template_file){
				Ok(x) => x,
				Err(error_report) => report_error("main.kism".to_string(), error_report.scanner, error_report.unresolved, error_report.resolved),
			});
		}
		_ => {
			println!("Failed to load file");
		}
	};
}

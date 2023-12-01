pub mod kiss;
pub mod errors;
pub mod generator;

use crate::errors::report_error;


fn main() {
	println!("{:?}", generator::generate());
}


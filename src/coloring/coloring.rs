trait Colorable {
	pub fn red(self) -> String {
		if !std::io::stdout().is_terminal() {
			self
		}
	}
}


impl Colorable for String {
	
}

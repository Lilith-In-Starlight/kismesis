pub struct Settings {
    inline: Vec<String>,
    in_previous: Vec<String>,
    only_closer: Vec<String>,
    only_opener: Vec<String>,
}

impl Settings {
    pub fn new() -> Self {
        Self {
            inline: string_vec(&[
                "p", "b", "i", "strong", "italic", "sub", "sup", "h1", "h2", "h3", "h4", "h5", "a", "li",
                "title",
            ]),
            in_previous: string_vec(&["b", "i", "strong", "italic", "sub", "sup", "br", "a"]),
            only_opener: string_vec(&["meta", "img"]),
            only_closer: string_vec(&["br"]),
        }
    }

    pub fn is_only_closer(&self, n: &str) -> bool {
        self.only_closer.iter().any(|x| x == n)
    }

    pub fn is_only_opener(&self, n: &str) -> bool {
        self.only_opener.iter().any(|x| x == n)
    }

    pub fn is_inline(&self, n: &str) -> bool {
        self.inline.iter().any(|x| x == n)
    }

    pub fn has_body(&self, n: &str) -> bool {
        !self.is_only_closer(n) && !self.is_only_opener(n)
    }
}

fn string_vec(s: &[&str]) -> Vec<String> {
    s.iter().map(|x| x.to_string()).collect()
}

use std::{fs, path::{PathBuf, Path}, io};

use self::{options::Settings, reporting::DrawingInfo};

mod errors;
pub(crate) mod html;
pub(crate) mod lexer;
pub(crate) mod options;
pub(crate) mod parser;
mod reporting;


pub fn compile_text(string: &str) -> String {
    let tokens = lexer::tokenize(string);
    let tree = match parser::file(tokens, None) {
        Ok(val) => val,
        Err((x, tokens)) => return reporting::draw_error(&x.unpack(), &DrawingInfo::from((&tokens, None))),
    };
    let settings = Settings::new();
    let html = html::generate_html(&tree, None, &settings);
    match html {
        Ok(x) => x.to_string_forced(),
        Err(errors) => {
            for error in errors {
                eprintln!("{}", reporting::draw_scoped_error(&error));
            }
            String::new()
        },
    }
}

pub fn compile_project() {
    // let errors = Vec::new();
    let main_path = PathBuf::from("templates/main.ks");
    let main_template = match parser::file(lexer::tokenize(&fs::read_to_string(&main_path).unwrap()), Some(main_path.clone())) {
        Ok(x) => x,
        Err(error) => {
            eprintln!("{}", reporting::draw_error(&error.0.unpack(), &DrawingInfo::from((&error.1, Some(&main_path)))));
            return
        },
    };
    let input_paths = recursive_crawl(&PathBuf::from("input")).0;
    let mut input_files = Vec::new();

    for path in input_paths.iter() {
        let tokens = lexer::tokenize(&fs::read_to_string(path).unwrap());
        match parser::file(tokens, Some(path.clone())) {
            Ok(mut x) => {
                x.template = Some(&main_template);
                input_files.push(x);
            },
            Err(x) => panic!("handle this"),
        }
    }

    let settings = Settings::new();
    for file in input_files.iter() {
        match html::generate_html(&file, None, &settings) {
            Ok(x) => println!("{}", x.to_string_forced()),
            Err(errors) => {
                for error in errors {
                    eprintln!("{}", reporting::draw_scoped_error(&error));
                }
            },
        }
    }
}

pub fn recursive_crawl(path: &Path) -> (Vec<PathBuf>, Vec<io::Error>) {
    let mut errors = Vec::new();
    let mut paths = Vec::new();
    for entry in fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path  = entry.path();
        if path.is_dir() {
            let (mut a, mut b) = recursive_crawl(&path);
            errors.append(&mut b);
            paths.append(&mut a)
        } else if let Some(ext) = path.extension() {
            if ext.to_string_lossy().to_string() == "ks" {
                paths.push(path)
            }
        }
    }

    (paths, errors)
}

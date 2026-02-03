use std::fs;
mod parser;
mod lexer;

fn main() {
    let contents = fs::read_to_string("./src/sample_input.vyn")
        .expect("should have been able to read the file");
    parser::parse_program(contents);
}

use std::fs;
mod parser;
mod lexer;

fn main() {
    let contents = fs::read_to_string("./src/sample_input.vyn")
        .expect("should have been able to read the file");
    let (stmts, errors) = parser::parse_program(contents);
    println!("stmts: {:?}", stmts);
    println!("errors: {:?}", errors);
}

use std::fs;
mod parser;
mod lexer;
mod diagnostics;

fn main() {
    let filename = "sample_input.vyn";
    let contents = fs::read_to_string("./src/sample_input.vyn")
        .expect("should have been able to read the file");
    let lexer_output = lexer::tokenize(filename.to_string(), contents);
    let (stmts, errors) = parser::parse_program(lexer_output.file, lexer_output.tokens);
    println!("stmts: {:?}", stmts);
    println!("errors: {:?}", errors);
}

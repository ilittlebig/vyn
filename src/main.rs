use std::fs;
use crate::diagnostics::Diagnostic;

mod parser;
mod lexer;
mod diagnostics;

fn main() {
    let filename = "sample_input.vyn";
    let contents = fs::read_to_string("./src/sample_input.vyn")
        .expect("should have been able to read the file");

    let lexer_output = lexer::tokenize(filename.to_string(), contents);
    let (stmts, parse_errors) = parser::parse_program(lexer_output.file.clone(), lexer_output.tokens);
    //println!("stmts: {:?}", stmts);

    for e in parse_errors {
        let diagnostic: Diagnostic = e.into();
        let _ = diagnostics::print_diagnostic(&lexer_output.file, &diagnostic);
    }

    for e in lexer_output.errors {
        let diagnostic: Diagnostic = e.into();
        let _ = diagnostics::print_diagnostic(&lexer_output.file, &diagnostic);
    }
}

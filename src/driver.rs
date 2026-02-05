/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-05
 **/

use crate::frontend::lexer;
use crate::frontend::parser;
use crate::diagnostics::{ print_diagnostic, Diagnostic };

pub fn drive(filename: &str, input: String) {
    let lexer_output = lexer::tokenize(filename.to_string(), input);
    let (stmts, parse_errors) = parser::parse_program(lexer_output.file.clone(), lexer_output.tokens);

    println!("stmts: {:?}", stmts);

    for e in parse_errors {
        let diagnostic: Diagnostic = e.into();
        let _ = print_diagnostic(&lexer_output.file, &diagnostic);
    }

    for e in lexer_output.errors {
        let diagnostic: Diagnostic = e.into();
        let _ = print_diagnostic(&lexer_output.file, &diagnostic);
    }
}

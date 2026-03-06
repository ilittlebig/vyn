/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-05
 **/

use crate::passes;
use crate::frontend::lexer;
use crate::frontend::parser;
use crate::tools::cli::CliCommand;
use crate::diagnostics::{ Emitter, Diagnostic };

pub fn drive(command: CliCommand, filename: &str, input: String) {
    let Some(opts) = command.compile() else {
        return;
    };

    let lexer_output = lexer::tokenize(filename.to_string(), input);
    let (stmts, parse_errors) = parser::parse_program(lexer_output.file.clone(), lexer_output.tokens);

    let mut emitter = Emitter::stderr();
    for e in parse_errors {
        let diagnostic: Diagnostic = e.into();
        let _ = emitter.emit(&lexer_output.file, &diagnostic);
    }

    for e in lexer_output.errors {
        let diagnostic: Diagnostic = e.into();
        let _ = emitter.emit(&lexer_output.file, &diagnostic);
    }

    let pass_diagnostics = passes::run_passes(&stmts, &opts);
    for e in pass_diagnostics {
        let diagnostic: Diagnostic = e.into();
        let _ = emitter.emit(&lexer_output.file, &diagnostic);
    }
}

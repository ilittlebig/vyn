/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-04
 **/

use std::env;

mod emitter;
mod error;
mod args;
mod parser;

pub use error::CliError;
pub use emitter::{ CliDiagnostic, print_error };
pub use args::{ CliCommand, CompileOptions, RunArgs, Target, Mode, CommandKind };

pub fn run() -> Result<CliCommand, CliError> {
    let iterator = env::args().skip(1);
    match parser::parse(iterator) {
        Ok(command) => Ok(command),
        Err(e) => Err(e)?
    }
}

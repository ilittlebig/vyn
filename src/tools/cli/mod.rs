/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-04
 **/

use std::env;

mod error;
mod args;
mod parser;

pub use error::CliError;
pub use args::{ CliCommand, CompileOptions, RunArgs, Target, Mode };

pub fn run() -> Result<CliCommand, CliError> {
    let mut iterator = env::args().skip(1);
    let command = parser::parse(iterator);
    Ok(command)
}

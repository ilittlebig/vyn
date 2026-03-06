/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-01
 **/

use std::fs;

mod tools;
mod frontend;
mod passes;
mod diagnostics;
mod source;
mod driver;

use crate::tools::cli::CliDiagnostic;

fn main() {
    let filename = "sample_input.vyn";
    let contents = fs::read_to_string("./tests/sample_input.vyn")
        .expect("should have been able to read the file");

    let command = match tools::cli::run() {
        Ok(command) => command,
        Err(e) => {
            tools::cli::print_error(CliDiagnostic::error(e.to_string()));
            return;
        },
    };
    driver::drive(command, filename, contents);
}

/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-01
 **/

use std::fs;

mod frontend;
mod diagnostics;
mod source;
mod driver;

fn main() {
    let filename = "sample_input.vyn";
    let contents = fs::read_to_string("./tests/sample_input.vyn")
        .expect("should have been able to read the file");
    driver::drive(filename, contents);
}

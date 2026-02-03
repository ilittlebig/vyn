use std::fs;
mod tokenizer;

fn main() {
    let contents = fs::read_to_string("./src/sample_input.vyn")
        .expect("should have been able to read the file");
    tokenizer::tokenize(contents);
}

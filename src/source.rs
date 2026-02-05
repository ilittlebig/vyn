/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-05
 **/

use crate::frontend::lexer::Token;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub name: String,
    pub src: String,
    pub line_starts: Vec<usize>,
}

fn compute_line_starts(src: &String) -> Vec<usize> {
    let mut line_starts = Vec::new();
    let mut byte_index: usize = 0;
    line_starts.push(byte_index);

    let bytes = src.as_bytes();
    while byte_index < bytes.len() {
        let b = bytes[byte_index];
        byte_index += 1;
        if b == b'\n' { line_starts.push(byte_index); }
    }
    line_starts
}

impl SourceFile {
    pub fn new(name: String, src: String) -> Self {
        let line_starts = compute_line_starts(&src);
        Self { name, src, line_starts }
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }

    pub fn slice(&self, token: &Token) -> &str {
        &self.src[token.span.start..token.span.end]
    }

    pub fn line_col(&self, pos: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&pos) {
            Ok(i) => i,
            Err(k) => k - 1, // only valid if k > 0
        };
        let col = pos - self.line_starts[line];
        (line, col)
    }

    pub fn line_span(&self, pos: usize) -> (&str, usize, usize) {
        let (line, _) = self.line_col(pos);
        let start = self.line_starts[line];

        let mut end = if line + 1 < self.line_starts.len() {
            self.line_starts[line + 1]
        } else {
            self.src.len()
        };

        let bytes = self.src.as_bytes();
        if end > start && bytes[end - 1] == b'\n' {
            end -= 1;
        }

        if end > start && bytes[end - 1] == b'\r' {
            end -= 1;
        }
        (&self.src[start..end], start, end)
    }
}

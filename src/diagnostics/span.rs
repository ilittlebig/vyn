/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn join(self, other: Span) -> Span {
        Span {
            start: other.start,
            end: other.end,
        }
    }
}

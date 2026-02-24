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

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

// dont even know if i want to do this
impl<T> Spanned<T> {
    pub fn new(node: T, start: usize, end: usize) -> Self {
        Self { node, span: Span::new(start, end) }
    }
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

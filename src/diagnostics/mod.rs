mod emitter;
mod span;

mod from_lexer;
mod from_parser;

pub use self::emitter::print_diagnostic;
pub use self::emitter::Diagnostic;

pub use self::span::Span;

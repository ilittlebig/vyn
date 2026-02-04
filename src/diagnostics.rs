/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-04
 **/

use std::borrow::Cow;
use crate::lexer::{ LexError, LexDiagnostic, SourceFile, Span, TokenKind };
use crate::parser::{ ParseError, Expected };

#[derive(Debug)]
pub enum Severity { Error, Warning }

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: Cow<'static, str>,
    pub span: Span
}

impl Diagnostic {
    fn error(message: impl Into<Cow<'static, str>>, span: Span) -> Diagnostic {
        Diagnostic { severity: Severity::Error, message: message.into(), span }
    }
}

// lexer errors
impl From<LexDiagnostic> for Diagnostic {
    fn from(e: LexDiagnostic) -> Self {
        match e.kind {
            LexError::UnterminatedString(_) => Diagnostic::error("unterminated string", e.span),
            _ => Diagnostic::error("blah", e.span),
        }
    }
}

// parser errors
impl From<ParseError> for Diagnostic {
    fn from(e: ParseError) -> Self {
        let msg = format!("expected {}, found {}", e.expected.describe(), e.found.describe());

        match (&e.expected, &e.found) {
            (Expected::Statement, _) => Diagnostic::error(msg, e.span),
            (Expected::Token(TokenKind::RParen), _) => Diagnostic::error("expected `)` to close `(`", e.span),
            _ => Diagnostic::error(msg, e.span),
        }
    }
}

pub fn print_diagnostic(source_file: &SourceFile, diagnostic: &Diagnostic) {
    let span = diagnostic.span;
    let (line, col) = source_file.line_col(span.start);
    let (line_text, line_start, line_end) = source_file.line_span(span.start);

    let severity = match diagnostic.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    };

    println!("{}:{}:{}: {}: {}", source_file.name, line+1, col+1, severity, diagnostic.message);
    println!(" {} | {}", line+1, line_text);

    let gutter_length = (line+1).to_string().len();
    let gutter = "  ".repeat(gutter_length);

    let highlight_start = span.start.max(line_start);
    let highlight_end = span.end.min(line_end);
    let width = (highlight_end - highlight_start).max(1);

    let marker = " ".repeat(col) + "^" + &"~".repeat(width - 1);
    println!("{}| {}", gutter, marker);
}

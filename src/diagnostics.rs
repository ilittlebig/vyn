/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-04
 **/

use std::borrow::Cow;
use crate::lexer::{ LexError, LexDiagnostic, SourceFile, Span };
use crate::parser::ParseError;

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

impl From<ParseError> for Diagnostic {
    fn from(e: ParseError) -> Self {
        Diagnostic::error("PARSER ERROR", e.span)
    }
}

impl From<LexDiagnostic> for Diagnostic {
    fn from(e: LexDiagnostic) -> Self {
        match e.kind {
            LexError::UnterminatedString(_) => Diagnostic::error("unterminated string", e.span),
            _ => Diagnostic::error("blah", e.span),
        }
    }
}

pub fn print_diagnostic(source_file: &SourceFile, diagnostic: &Diagnostic) {
    let span = diagnostic.span;
    let (line, col) = source_file.line_col(span.start);
    let line_text = source_file.line_span(span.start);

    let severity = match diagnostic.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    };

    println!("{}:{}:{}: {}: {}", source_file.name, line, col, severity, diagnostic.message);
    println!(" {} | {}", line, line_text);

    let marker = " ".repeat(col) + "^";
    println!("   | {}", marker);
}

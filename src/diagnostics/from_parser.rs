/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-05
 **/

use crate::diagnostics::Diagnostic;
use crate::frontend::lexer::TokenKind;
use crate::frontend::parser::{ ParseError, Expected };

impl From<ParseError> for Diagnostic {
    fn from(e: ParseError) -> Self {
        let msg = format!("expected {}, found {}", e.expected.describe(), e.found.describe());

        match (&e.expected, &e.found) {
            (Expected::Statement, _) => Diagnostic::error(msg, e.span),
            (Expected::Token(TokenKind::RParen), _) => Diagnostic::error("expected `)` to close `(`", e.span),
            (Expected::Token(TokenKind::RBrace), _) => Diagnostic::error("expected `}` to close `{`", e.span),
            _ => Diagnostic::error(msg, e.span),
        }
    }
}

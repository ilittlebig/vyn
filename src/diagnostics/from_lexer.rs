/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-05
 **/

use crate::diagnostics::Diagnostic;
use crate::frontend::lexer::{ LexError, LexDiagnostic, TokenKind };

impl From<LexDiagnostic> for Diagnostic {
    fn from(e: LexDiagnostic) -> Self {
        match e.kind {
            LexError::UnterminatedString(_) => Diagnostic::error("unterminated string", e.span),
            _ => Diagnostic::error("blah", e.span),
        }
    }
}

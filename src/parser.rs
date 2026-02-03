/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-03
 **/

use crate::lexer;
use crate::lexer::{Token, TokenKind, Keyword};

#[derive(Debug)]
enum Expected {
    Identifier,
    AnyKeyword,
    Keyword(Keyword),
    Operator,
    Token(TokenKind),
}

impl Expected {
    fn matches(&self, token_kind: &TokenKind) -> bool {
        match self {
            Expected::Identifier => matches!(token_kind, TokenKind::Identifier),
            Expected::AnyKeyword => matches!(token_kind, TokenKind::Keyword(_)),
            Expected::Keyword(k) => matches!(token_kind, TokenKind::Keyword(k2) if k2 == k),
            Expected::Operator => matches!(token_kind, TokenKind::Operator(_)),
            Expected::Token(exact) => token_kind == exact,
        }
    }
}

#[derive(Debug)]
struct ParseError {
    expected: Expected,
    found: TokenKind,
    start: usize,
    end: usize,
}

struct Parser {
    src: String,
    tokens: Vec<Token>,
    current_index: usize,
}

impl Parser {
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current_index)
    }

    fn expect(&mut self, expected: Expected) -> Result<(), ParseError> {
        let Some(token) = self.peek() else {
            return Err(ParseError {
                expected,
                found: TokenKind::Eof,
                start: self.src.len(),
                end: self.src.len(),
            });
        };

        if !expected.matches(&token.kind) {
            return Err(ParseError {
                expected,
                found: token.kind.clone(),
                start: token.start,
                end: token.end,
            });
        }

        self.next();
        Ok(())
    }

    fn next(&mut self) {
        if self.current_index == self.src.len() { return; }
        self.current_index += 1;
    }
}

pub fn parse_program(src: String) {
    let lexer_output = lexer::tokenize(src);
    let mut parser = Parser {
        src: lexer_output.src,
        tokens: lexer_output.tokens,
        current_index: 0,
    };

    while let Some(token) = parser.peek() {
        if token.kind == TokenKind::Eof { break; }
        match parser.expect(Expected::Keyword(Keyword::Local)) {
            Ok(_) => { println!("CONSUMED LOCAL"); },
            Err(e) => {
                parser.next();
                println!("{:?}", e);
            }
        }
    }
}

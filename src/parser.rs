/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-03
 **/

use crate::lexer;
use crate::lexer::{Token, TokenKind, Keyword, Operator};

#[derive(Debug)]
enum Expected {
    Identifier,
    AnyKeyword,
    Keyword(Keyword),
    Operator(Operator),
    Token(TokenKind),
}

impl Expected {
    fn matches(&self, token_kind: &TokenKind) -> bool {
        match self {
            Expected::Identifier => matches!(token_kind, TokenKind::Identifier),
            Expected::AnyKeyword => matches!(token_kind, TokenKind::Keyword(_)),
            Expected::Keyword(k) => matches!(token_kind, TokenKind::Keyword(k2) if k2 == k),
            Expected::Operator(o) => matches!(token_kind, TokenKind::Operator(o2) if o2 == o),
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

    fn parse_decl_stmt(&mut self) {
        match self.expect(Expected::Keyword(Keyword::Local)) {
            Ok(_) => { println!("consumed local"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Identifier) {
            Ok(_) => { println!("consumed identifier"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Token(TokenKind::Colon)) {
            Ok(_) => { println!("consumed colon"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Token(TokenKind::Identifier)) {
            Ok(_) => { println!("consumed identifier"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Token(TokenKind::Assignment)) {
            Ok(_) => { println!("consumed assignment"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Token(TokenKind::Integer)) {
            Ok(_) => { println!("consumed integer"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Operator(Operator::Plus)) {
            Ok(_) => { println!("consumed plus"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Token(TokenKind::Integer)) {
            Ok(_) => { println!("consumed integer"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Operator(Operator::Multiplication)) {
            Ok(_) => { println!("consumed multiplication"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Token(TokenKind::Integer)) {
            Ok(_) => { println!("consumed integer"); },
            Err(e) => { println!("{:?}", e); },
        }
        match self.expect(Expected::Token(TokenKind::Semicolon)) {
            Ok(_) => { println!("consumed semicolon"); },
            Err(e) => { println!("{:?}", e); },
        }
    }

    fn parse_stmt(&mut self) {
        let token = self.peek().unwrap(); // TODO: do something about this unwrap
        if token.kind == TokenKind::Keyword(Keyword::Local) {
            return self.parse_decl_stmt();
        }
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
        parser.parse_stmt();
        parser.next();
    }
}

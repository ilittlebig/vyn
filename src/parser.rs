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

    fn consume_if(&mut self, expected: Expected) -> bool {
        let Some(token) = self.peek() else { return false };
        if !expected.matches(&token.kind) {
            return false;
        }

        self.next();
        return true;
    }

    // should ideally never be used as an api
    fn next(&mut self) {
        if self.current_index == self.tokens.len() { return; }
        self.current_index += 1;
    }

    // TODO: research Operator-precedence parser
    fn parse_expr(&mut self) {
        self.consume_if(Expected::Token(TokenKind::Integer));
        self.consume_if(Expected::Operator(Operator::Plus));
        self.consume_if(Expected::Token(TokenKind::Integer));
        self.consume_if(Expected::Operator(Operator::Multiplication));
        self.consume_if(Expected::Token(TokenKind::Integer));
    }

    fn parse_type(&mut self) -> Result<(), ParseError> {
        self.expect(Expected::Identifier)?;
        Ok(())
    }

    fn parse_expr_stmt(&mut self) -> Result<(), ParseError> {
        self.parse_expr();
        self.consume_if(Expected::Token(TokenKind::Semicolon))
    }

    // local name (":" type)? ("=" expr)? ";"?
    fn parse_decl_stmt(&mut self) -> Result<(), ParseError> {
        self.expect(Expected::Keyword(Keyword::Local))?;
        self.expect(Expected::Identifier)?;

        if self.consume_if(Expected::Token(TokenKind::Colon)) {
            self.parse_type()?;
        }

        if self.consume_if(Expected::Token(TokenKind::Assignment)) {
            self.parse_expr();
        }

        self.consume_if(Expected::Token(TokenKind::Semicolon));
        Ok(())
    }

    fn parse_stmt(&mut self) -> Result<(), ParseError> {
        let token = self.peek().unwrap(); // TODO: do something about this unwrap
        if token.kind == TokenKind::Keyword(Keyword::Local) {
            return self.parse_decl_stmt();
        } else {
            return self.parse_expr_stmt();
        }
        Ok(())
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
        match parser.parse_stmt() {
            Ok(_) => {},
            Err(e) => { println!("{:?}", e); },
        }
        parser.next();
    }
}

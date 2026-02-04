/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-03
 **/

use crate::lexer;
use crate::lexer::{Token, TokenKind, Keyword, Operator};

#[derive(Debug, Clone)]
enum Expected {
    Identifier,
    AnyKeyword,
    Keyword(Keyword),
    Operator(Operator),
    Token(TokenKind),
    PrimaryExpression,
}

impl Expected {
    fn matches(&self, token_kind: &TokenKind) -> bool {
        match self {
            Expected::Identifier => matches!(token_kind, TokenKind::Identifier),
            Expected::AnyKeyword => matches!(token_kind, TokenKind::Keyword(_)),
            Expected::Keyword(k) => matches!(token_kind, TokenKind::Keyword(k2) if k2 == k),
            Expected::Operator(o) => matches!(token_kind, TokenKind::Operator(o2) if o2 == o),
            Expected::Token(exact) => token_kind == exact,
            _ => false,
        }
    }
}

#[derive(Debug)]
enum TypeRef {
    Named(String) // int, string
}

#[derive(Debug)]
pub enum Expr {
    String(String),
    Int(i64),
    Ident(String),

    Binary { lhs: Box<Expr>, op: Operator, rhs: Box<Expr> }
}

#[derive(Debug)]
pub enum Stmt {
    Decl { name: String, ty: Option<TypeRef>, init: Option<Expr> },
    Block(Vec<Stmt>),
    ExprStmt(Expr),
}

#[derive(Debug)]
pub struct ParseError {
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
    fn at_eof(&self) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Eof) | None)
    }

    fn is_stmt_start(&self, token_kind: &TokenKind) -> bool {
        matches!(token_kind,
            TokenKind::StringLiteral
            | TokenKind::Integer
            | TokenKind::Double
            | TokenKind::Identifier
            | TokenKind::Keyword(Keyword::Local)
        )
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current_index)
    }

    fn bump(&mut self) -> Token {
        let i = self.current_index;
        self.current_index += 1;
        self.tokens[i].clone()
    }

    fn slice(&self, token: &Token) -> &str {
        &self.src[token.start..token.end]
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        let token = self.expect(Expected::Identifier)?;
        Ok(self.slice(&token).to_string())
    }

    fn expect(&mut self, expected: Expected) -> Result<Token, ParseError> {
        let token = self.peek().ok_or(ParseError {
            expected: expected.clone(),
            found: TokenKind::Eof,
            start: self.src.len(),
            end: self.src.len(),
        })?;

        if !expected.matches(&token.kind) {
            return Err(ParseError {
                expected,
                found: token.kind.clone(),
                start: token.start,
                end: token.end,
            });
        }
        Ok(self.bump())
    }

    fn consume_if(&mut self, expected: Expected) -> Option<Token> {
        let token = self.peek()?;
        if expected.matches(&token.kind) {
            Some(self.bump())
        } else {
            None
        }
    }

    fn sync_after_error(&mut self) {
        while let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Semicolon => {
                    self.bump();
                    break;
                },
                k if self.is_stmt_start(k) => break,
                TokenKind::Eof => break,
                _ => { self.bump(); }
            }
        }
    }

    fn infix_binding_power(&self, token_kind: &TokenKind) -> Option<(u8, u8)> {
        match token_kind {
            TokenKind::Operator(op) => match op {
                Operator::Plus | Operator::Minus => Some((10, 11)),
                Operator::Multiplication | Operator::Division | Operator::Modulus => Some((20, 21)),
            },
            _ => None,
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().ok_or(ParseError {
            expected: Expected::PrimaryExpression,
            found: TokenKind::Eof,
            start: self.src.len(),
            end: self.src.len(),
        })?;

        match &token.kind {
            TokenKind::LParen => {
                self.bump();
                let expr = self.parse_expr_bp(0)?;
                self.expect(Expected::Token(TokenKind::RParen))?;
                return Ok(expr);
            },
            TokenKind::StringLiteral => {
                let value = self.slice(token).to_string();
                self.bump();
                return Ok(Expr::String(value));
            },
            TokenKind::Integer => {
                let value = self.slice(token).to_string();
                self.bump();
                return Ok(Expr::Int(value.parse().unwrap()));
            },
            TokenKind::Identifier => {
                let value = self.slice(token).to_string();
                self.bump();
                return Ok(Expr::Ident(value));
            },
            _ => Err(ParseError {
                expected: Expected::PrimaryExpression,
                found: token.kind.clone(),
                start: token.start,
                end: token.end,
            }),
        }
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_primary()?;
        while let Some(token) = self.peek() {
            let Some((l_bp, r_bp)) = self.infix_binding_power(&token.kind) else {
                break;
            };
            if l_bp < min_bp { break; }

            let op = match &self.bump().kind {
                TokenKind::Operator(op) => *op,
                _ => unreachable!(),
            };

            let rhs = self.parse_expr_bp(r_bp)?;
            lhs = Expr::Binary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) };
        }
        Ok(lhs)
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0)
    }

    // expr ";"?
    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;
        self.consume_if(Expected::Token(TokenKind::Semicolon));
        Ok(Stmt::ExprStmt(expr))
    }

    // local name (":" type)? ("=" expr)? ";"?
    fn parse_decl_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Expected::Keyword(Keyword::Local))?;

        let name = self.expect_ident()?;
        let ty = if self.consume_if(Expected::Token(TokenKind::Colon)).is_some() {
            Some(TypeRef::Named(self.expect_ident()?))
        } else {
            None
        };

        let init = if self.consume_if(Expected::Token(TokenKind::Assignment)).is_some() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.consume_if(Expected::Token(TokenKind::Semicolon));
        Ok(Stmt::Decl { name, ty, init })
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Expected::Token(TokenKind::LBrace))?;

        let mut stmts = Vec::new();
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::RBrace { break; }
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        self.expect(Expected::Token(TokenKind::RBrace))?;
        Ok(Stmt::Block(stmts))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let token = self.peek().unwrap();
        if token.kind == TokenKind::Keyword(Keyword::Local) {
            return self.parse_decl_stmt();
        } else if token.kind == TokenKind::LBrace {
            return self.parse_block_stmt();
        } else {
            return self.parse_expr_stmt();
        }
    }
}

pub fn parse_program(src: String) -> (Vec<Stmt>, Vec<ParseError>) {
    let lexer_output = lexer::tokenize(src);
    let mut parser = Parser {
        src: lexer_output.src,
        tokens: lexer_output.tokens,
        current_index: 0,
    };

    let mut stmts = Vec::new();
    let mut errors = Vec::new();

    while let Some(token) = parser.peek() {
        if token.kind == TokenKind::Eof { break; }
        match parser.parse_stmt() {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => {
                errors.push(e);
                parser.sync_after_error();
            },
        }
    }
    (stmts, errors)
}

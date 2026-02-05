/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-03
 **/

use crate::lexer::{Token, TokenKind, Keyword, Operator, Span, SourceFile};

#[derive(Debug, Clone)]
pub enum Expected {
    Identifier,
    AnyKeyword,
    Keyword(Keyword),
    Operator(Operator),
    Token(TokenKind),
    PrimaryExpression,
    Statement,
}

impl Expected {
    fn matches(&self, kind: &TokenKind) -> bool {
        match self {
            Expected::Identifier => matches!(kind, TokenKind::Identifier),
            Expected::AnyKeyword => matches!(kind, TokenKind::Keyword(_)),
            Expected::Keyword(k) => matches!(kind, TokenKind::Keyword(k2) if k2 == k),
            Expected::Operator(o) => matches!(kind, TokenKind::Operator(o2) if o2 == o),
            Expected::Token(exact) => kind == exact,
            _ => false,
        }
    }

    pub fn describe(&self) -> &'static str {
        match self {
            Expected::Identifier => "an identifier",
            Expected::AnyKeyword => "a keyword",
            Expected::Keyword(_) => "that keyword",
            Expected::Operator(_) => "that operator",
            Expected::Token(_) => "that token",
            Expected::PrimaryExpression => "an expression",
            Expected::Statement => "a statement",
        }
    }
}

#[derive(Debug, Clone)]
enum TypeRef {
    Named(String) // int, string
}

#[derive(Debug, Clone)]
enum UnaryOp {
    Neg,
    Not,
    Plus,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
struct Func {
    body: Box<Block>,
    //TODO: params
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type ExprSpanned = Spanned<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    String(String),
    Int(i64),
    Ident(String),
    Bool(bool),

    Func(Func),
    Call { callee: Box<ExprSpanned>, args: Vec<ExprSpanned> },
    Assign { target: Box<ExprSpanned>, value: Box<ExprSpanned> },

    Unary { op: UnaryOp, rhs: Box<ExprSpanned> },
    Binary { lhs: Box<ExprSpanned>, op: Operator, rhs: Box<ExprSpanned> },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl { name: String, ty: Option<TypeRef>, init: Option<ExprSpanned> },
    FuncDecl { name: String, init: Func },
    Block(Block),
    ExprStmt(ExprSpanned),
}

#[derive(Debug)]
pub struct ParseError {
    pub expected: Expected,
    pub found: TokenKind,
    pub span: Span,
}

struct Parser {
    file: SourceFile,
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn is_stmt_start(&self, token_kind: &TokenKind) -> bool {
        matches!(token_kind,
            TokenKind::StringLiteral
            | TokenKind::Integer
            | TokenKind::Double
            | TokenKind::Identifier
            | TokenKind::Keyword(Keyword::Local)
        )
    }

    fn is_valid_expr_stmt(&self, e: &Expr) -> bool {
        matches!(e,
            Expr::Call { .. }
            | Expr::Assign { .. }
        )
    }

    fn is_assign_target(&self, e: &Expr) -> bool {
        matches!(e,
            Expr::Ident(_)
        )
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn peek_is(&self, kind: &TokenKind) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(k) if k == kind)
    }

    fn join(&self, a: Span, b: Span) -> Span {
        Span { start: a.start, end: b.end }
    }

    fn bump(&mut self) -> Token {
        let i = self.pos;
        self.pos += 1;
        self.tokens[i].clone()
    }

    fn expect_ident(&mut self) -> Result<(String, Span), ParseError> {
        let token = self.expect(Expected::Identifier)?;
        Ok((self.file.slice(&token).to_string(), token.span))
    }

    // in-case EOF is the next token
    fn expect_closing(&mut self, rparen_kind: TokenKind, open_span: Span) -> Result<Token, ParseError> {
        let token = self.expect(Expected::Token(rparen_kind))
            .map_err(|mut e| {
                e.span = open_span;
                e
            })?;
        Ok(token)
    }

    fn expect(&mut self, expected: Expected) -> Result<Token, ParseError> {
        let token = self.peek().ok_or(ParseError {
            expected: expected.clone(),
            found: TokenKind::Eof,
            span: Span { start: self.file.len(), end: self.file.len() },
        })?;

        if !expected.matches(&token.kind) {
            return Err(ParseError {
                expected,
                found: token.kind.clone(),
                span: token.span,
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

    fn prefix_binding_power(&self, token_kind: &TokenKind) -> Option<(UnaryOp, u8)> {
        match token_kind {
            TokenKind::Operator(Operator::Minus) => Some((UnaryOp::Neg, 100)),
            TokenKind::Operator(Operator::Not) => Some((UnaryOp::Not, 100)),
            TokenKind::Operator(Operator::Plus) => Some((UnaryOp::Plus, 100)),
            _ => None,
        }
    }

    fn infix_binding_power(&self, token_kind: &TokenKind) -> Option<(u8, u8)> {
        match token_kind {
            TokenKind::Operator(op) => match op {
                // arithmetic
                Operator::Plus | Operator::Minus => Some((10, 11)),
                Operator::Multiplication | Operator::Division | Operator::Modulus => Some((20, 21)),

                // boolean
                Operator::And => Some((4, 5)),
                Operator::Or => Some((2, 3)),

                // comparisons
                Operator::LessThan
                | Operator::LessThanEqual
                | Operator::GreaterThan
                | Operator::GreaterThanEqual => Some((8, 9)),

                // equality
                Operator::Equal | Operator::NotEqual => Some((6, 7)),

                //
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_primary(&mut self) -> Result<ExprSpanned, ParseError> {
        let (kind, span) = {
            let token = self.peek().ok_or(ParseError {
                expected: Expected::PrimaryExpression,
                found: TokenKind::Eof,
                span: Span { start: self.file.len(), end: self.file.len() },
            })?;
            (token.kind.clone(), token.span)
        };

        if let Some((unary_op, r_bp)) = self.prefix_binding_power(&kind) {
            self.bump();
            let rhs = self.parse_expr_bp(r_bp)?;
            let span = self.join(span, rhs.span);

            return Ok(Spanned {
                node: Expr::Unary { op: unary_op, rhs: Box::new(rhs) },
                span
            })
        };

        match &kind {
            TokenKind::Keyword(Keyword::Function) => {
                let fn_token = self.bump();
                let lparen = self.expect(Expected::Token(TokenKind::LParen))?;
                // TODO: function params
                self.expect_closing(TokenKind::RParen, lparen.span)?;

                let body = self.parse_block()?;
                let span = self.join(fn_token.span, body.span);
                return Ok(Spanned {
                    node: Expr::Func(Func { body: Box::new(body) }),
                    span
                });
            },
            TokenKind::Keyword(Keyword::True) => {
                self.expect(Expected::Keyword(Keyword::True))?;
                return Ok(Spanned { node: Expr::Bool(true), span });
            },
            TokenKind::Keyword(Keyword::False) => {
                self.expect(Expected::Keyword(Keyword::False))?;
                return Ok(Spanned { node: Expr::Bool(false), span });
            },
            TokenKind::LParen => {
                let lparen = self.expect(Expected::Token(TokenKind::LParen))?;
                let inner = self.parse_expr_bp(0)?;
                let rparen = self.expect_closing(TokenKind::RParen, lparen.span)?;

                return Ok(Spanned {
                    node: inner.node,
                    span: self.join(lparen.span, rparen.span)
                });
            },
            TokenKind::StringLiteral => {
                let token = self.bump();
                let value = self.file.slice(&token).to_string();
                return Ok(Spanned { node: Expr::String(value), span });
            },
            TokenKind::Integer => {
                let token = self.bump();
                let value = self.file.slice(&token).parse::<i64>().unwrap();
                return Ok(Spanned { node: Expr::Int(value), span });
            },
            TokenKind::Identifier => {
                let token = self.bump();
                let value = self.file.slice(&token).to_string();
                return Ok(Spanned { node: Expr::Ident(value), span });
            },
            _ => Err(ParseError {
                expected: Expected::PrimaryExpression,
                found: kind,
                span
            }),
        }
    }

    fn parse_call(&mut self, callee: ExprSpanned) -> Result<ExprSpanned, ParseError> {
        let lparen = self.expect(Expected::Token(TokenKind::LParen))?;

        let mut args = Vec::new();
        if !self.peek_is(&TokenKind::RParen) {
            loop {
                let expr = self.parse_expr()?;
                args.push(expr);

                if self.consume_if(Expected::Token(TokenKind::Comma)).is_some() {
                    // no trailing comma allowed
                    if self.peek_is(&TokenKind::RParen) {
                        let token = self.peek().unwrap();
                        return Err(ParseError { expected: Expected::PrimaryExpression, found: TokenKind::RParen, span: token.span });
                    }
                    continue;
                }
                break;
            }
        }

        let rparen = self.expect_closing(TokenKind::RParen, lparen.span)?;
        let span = self.join(callee.span, rparen.span);
        Ok(Spanned {
            node: Expr::Call { callee: Box::new(callee), args },
            span
        })
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<ExprSpanned, ParseError> {
        let mut lhs = self.parse_primary()?;
        while self.peek_is(&TokenKind::LParen) {
            lhs = self.parse_call(lhs)?;
        }

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
            let span = self.join(lhs.span, rhs.span);
            lhs = Spanned {
                node: Expr::Binary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) },
                span
            };
        }
        Ok(lhs)
    }

    fn parse_assign(&mut self) -> Result<ExprSpanned, ParseError> {
        let lhs = self.parse_expr_bp(0)?;
        if self.peek_is(&TokenKind::Assignment) {
            self.bump();

            let rhs = self.parse_assign()?;
            if !self.is_assign_target(&lhs.node) {
                return Err(ParseError {
                    expected: Expected::Statement,
                    found: TokenKind::Assignment,
                    span: lhs.span,
                });
            }

            let span = self.join(lhs.span, rhs.span);
            Ok(Spanned {
                node: Expr::Assign { target: Box::new(lhs), value: Box::new(rhs) },
                span
            })
        } else {
            Ok(lhs)
        }
    }

    fn parse_expr(&mut self) -> Result<ExprSpanned, ParseError> {
        self.parse_assign()
    }

    // expr ";"?
    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let found_kind = self.peek().map(|t| t.kind.clone()).unwrap_or(TokenKind::Eof);
        let expr = self.parse_expr()?;
        self.consume_if(Expected::Token(TokenKind::Semicolon));

        if !self.is_valid_expr_stmt(&expr.node) {
            return Err(ParseError {
                expected: Expected::Statement,
                found: found_kind,
                span: expr.span
            });
        }
        Ok(Stmt::ExprStmt(expr))
    }

    /*
     * decl_stmt :=
     *     "local" (
     *         "function" Ident func_def
     *         | Ident (":" type)? ("=" expr)?
     *     ) ";"? ;
     *
     * func_def :=
     *     "(" params? ")" block ;
     *
     * block :=
     *     "{" stmt* "}" ;
     */
    fn parse_decl_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Expected::Keyword(Keyword::Local))?;

        if self.peek_is(&TokenKind::Keyword(Keyword::Function)) {
            let fn_token = self.expect(Expected::Keyword(Keyword::Function))?;
            let (name, _) = self.expect_ident()?;

            let lparen = self.expect(Expected::Token(TokenKind::LParen))?;
            // TODO: function params
            self.expect_closing(TokenKind::RParen, lparen.span)?;

            let body = self.parse_block()?;
            let span = self.join(fn_token.span, body.span);

            let expr = Spanned {
                node: Expr::Func(Func { body: Box::new(body) }),
                span
            };
            Ok(Stmt::Decl { name, ty: None, init: Some(expr) })
        } else {
            let (name, _) = self.expect_ident()?;
            let ty = if self.consume_if(Expected::Token(TokenKind::Colon)).is_some() {
                let (name, _) = self.expect_ident()?;
                Some(TypeRef::Named(name))
            } else {
                None
            };

            let init = if self.consume_if(Expected::Token(TokenKind::Assignment)).is_some() {
                Some(self.parse_expr()?)
            } else {
                None
            };

            let last_stmt_end = self.tokens[self.pos.saturating_sub(1)].span.end;
            if self.consume_if(Expected::Token(TokenKind::Semicolon)).is_some() {
                // ok
            } else if matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Eof) | Some(TokenKind::RBrace)) {
                // ok
            } else {
                let next = self.peek().unwrap();
                let (line1, _) = self.file.line_col(last_stmt_end.saturating_sub(1));
                let (line2, _) = self.file.line_col(next.span.start);

                if line1 == line2 {
                    return Err(ParseError {
                        expected: Expected::Token(TokenKind::Semicolon),
                        found: next.kind.clone(),
                        span: next.span,
                    });
                }
            }
            Ok(Stmt::Decl { name, ty, init })
        }
    }

    fn parse_func_decl_stmt(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Expected::Keyword(Keyword::Function))?;
        let (name, _) = self.expect_ident()?;

        let lparen = self.expect(Expected::Token(TokenKind::LParen))?;
        // TODO: function params
        self.expect_closing(TokenKind::RParen, lparen.span)?;

        let body = self.parse_block()?;
        Ok(Stmt::FuncDecl { name, init: Func { body: Box::new(body) }})
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let lbrace = self.expect(Expected::Token(TokenKind::LBrace))?;

        let mut stmts = Vec::new();
        while !self.peek_is(&TokenKind::RBrace) && !self.peek_is(&TokenKind::Eof) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        let rbrace = self.expect_closing(TokenKind::RBrace, lbrace.span)?;
        Ok(Block {
            stmts,
            span: self.join(lbrace.span, rbrace.span)
        })
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParseError> {
        let body = self.parse_block()?;
        Ok(Stmt::Block(body))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let token = self.peek().unwrap();
        if token.kind == TokenKind::Keyword(Keyword::Local) {
            return self.parse_decl_stmt();
        } else if token.kind == TokenKind::Keyword(Keyword::Function) {
            return self.parse_func_decl_stmt();
        } else if token.kind == TokenKind::LBrace {
            return self.parse_block_stmt();
        } else {
            return self.parse_expr_stmt();
        }
    }
}

pub fn parse_program(file: SourceFile, tokens: Vec<Token>) -> (Vec<Stmt>, Vec<ParseError>) {
    let mut parser = Parser { file, tokens, pos: 0 };
    let mut stmts = Vec::new();
    let mut errors = Vec::new();

    while let Some(token) = parser.peek() {
        if token.kind == TokenKind::Eof { break; }
        if matches!(parser.peek().map(|t| &token.kind), Some(TokenKind::Error)) {
            parser.sync_after_error();
            continue;
        }

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

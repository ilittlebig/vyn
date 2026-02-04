/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-01
 **/

#[derive(PartialEq)]
enum MantissaKind { Int, Float }
enum MantissaError { MissingDigits }
enum ExponentError { MissingDigits }

#[derive(Debug, Clone)]
enum LexError {
    UnexpectedEof,
    UnterminatedString(u8),
    UnexpectedChar(u8),
    DanglingEscape,
    InvalidExponent,
    InvalidMantissa,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Keyword {
    Local,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Operator {
    Plus,
    Minus,
    Division,
    Multiplication,
    Modulus
}

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    Whitespace,
    Identifier,
    StringLiteral,
    Comment,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Keyword(Keyword),
    Operator(Operator),
    Assignment,
    Semicolon,
    Colon,
    Integer,
    Double,
    Error,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
struct LexDiagnostic {
    kind: LexError,
    start: usize,
    end: usize,
}

struct Lexer {
    src: String,
    current_pos: usize,
    errors: Vec<LexDiagnostic>,
    line_starts: Vec<usize>,
}

#[derive(Debug)]
pub struct LexerOutput {
    pub src: String,
    pub tokens: Vec<Token>,
    pub errors: Vec<LexDiagnostic>,
    pub line_starts: Vec<usize>,
}

impl Lexer {
    fn is_identifier_start(&self, b: u8) -> bool {
        b == b'_' || b.is_ascii_alphabetic()
    }

    fn is_number_start(&self, b: u8) -> bool {
        if b == b'.' {
            let Some((_, b2)) = self.peek2() else { return false };
            if !b2.is_ascii_digit() { return false };
            return true
        };
        b.is_ascii_digit()
    }

    fn peek2(&self) -> Option<(u8, u8)> {
        let bytes = self.src.as_bytes();
        let b0 = bytes.get(self.current_pos).copied()?;
        let b1 = bytes.get(self.current_pos+1).copied()?;
        Some((b0, b1))
    }

    fn peek(&self) -> Option<u8> {
        self.src.as_bytes().get(self.current_pos).copied()
    }

    fn bump(&mut self) {
        self.current_pos += 1;
    }

    fn bump_or_error(&mut self) -> Result<(), LexError> {
        if self.peek().is_some() {
            self.bump();
            Ok(())
        } else {
            Err(LexError::UnexpectedEof)
        }
    }

    fn consume_while<T: Fn(u8) -> bool>(&mut self, pred: T) -> usize {
        let mut tokens_consumed = 0;
        while self.peek().is_some_and(|b| pred(b)) {
            self.bump();
            tokens_consumed += 1;
        }
        tokens_consumed
    }

    fn lookup_keyword(&self, lexeme: &str) -> Option<Keyword> {
        match lexeme {
            "local" => Some(Keyword::Local),
            _ => None,
        }
    }

    fn lookup_operator(&self, b: u8) -> Option<Operator> {
        match b {
            b'+' => Some(Operator::Plus),
            b'-' => Some(Operator::Minus),
            b'/' => Some(Operator::Division),
            b'*' => Some(Operator::Multiplication),
            b'%' => Some(Operator::Modulus),
            _ => None,
        }
    }

    fn lex_error(&mut self, kind: LexError, start: usize, end: usize) {
        self.errors.push(LexDiagnostic { kind, start, end });
    }

    fn compute_line_starts(&mut self) {
        let mut byte_index: usize = 0;
        self.line_starts.push(byte_index);

        let bytes = self.src.as_bytes();
        while byte_index < bytes.len() {
            let b = bytes[byte_index];
            byte_index += 1;
            if b == b'\n' { self.line_starts.push(byte_index); }
        }
    }

    fn line_col(&self, pos: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&pos) {
            Ok(i) => i,
            Err(k) => k - 1, // only valid if k > 0
        };
        let col = pos - self.line_starts[line];
        (line, col)
    }

    /*
     *
     **/
    fn eat_identifier(&mut self, start: usize) -> Token {
        while self.peek().is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_') {
            self.bump();
        }

        let lexeme = &self.src[start..self.current_pos];
        if let Some(keyword_kind) = self.lookup_keyword(lexeme) {
            return Token {
                kind: TokenKind::Keyword(keyword_kind),
                start,
                end: self.current_pos,
            };
        }

        Token {
            kind: TokenKind::Identifier,
            start,
            end: self.current_pos,
        }
    }

    // ( DIGIT+ ( '.' DIGIT* )? | '.' DIGIT+ )
    fn scan_mantissa(&mut self) -> Result<MantissaKind, MantissaError> {
        // starts with .
        if self.peek().is_some_and(|b| b == b'.') {
            self.bump();
            let frac_digits = self.consume_while(|b| b.is_ascii_digit());
            return if frac_digits == 0 {
                Err(MantissaError::MissingDigits)
            } else {
                Ok(MantissaKind::Float)
            };
        }

        // starts with a number
        let int_digits = self.consume_while(|b| b.is_ascii_digit());
        if int_digits == 0 {
            return Err(MantissaError::MissingDigits);
        }

        if self.peek().is_some_and(|b| b == b'.') {
            self.bump();
            self.consume_while(|b| b.is_ascii_digit());
            return Ok(MantissaKind::Float);
        }
        Ok(MantissaKind::Int)
    }

    // ( [eE] [+-]? DIGIT+ )?
    fn scan_exponent(&mut self) -> Result<bool, ExponentError> {
        if self.peek().is_some_and(|b| b == b'e' || b == b'E') {
            self.bump();
        } else {
            return Ok(false);
        }

        if self.peek().is_some_and(|b| b == b'-' || b == b'+') {
            self.bump();
            let int_digits = self.consume_while(|b| b.is_ascii_digit());
            if int_digits == 0 {
               return Err(ExponentError::MissingDigits);
            }
            return Ok(true);
        }

        if !self.peek().is_some_and(|b| b.is_ascii_digit()) {
            return Err(ExponentError::MissingDigits);
        }

        self.consume_while(|b| b.is_ascii_digit());
        Ok(true)
    }

    /*
     * number :=
     *     ( DIGIT+ ( '.' DIGIT* )? | '.' DIGIT+ )
     *     ( [eE] [+-]? DIGIT+ )?
     **/
    fn eat_number(&mut self, start: usize) -> Token {
        let mantissa_kind = match self.scan_mantissa() {
            Ok(kind) => kind,
            Err(_) => {
                let end = self.current_pos;
                self.lex_error(LexError::InvalidMantissa, start, end);
                return Token { kind: TokenKind::Error, start, end };
            }
        };

        let exponent_present = match self.scan_exponent() {
            Ok(present) => present,
            Err(_) => {
                let end = self.current_pos;
                self.lex_error(LexError::InvalidExponent, start, end);
                return Token { kind: TokenKind::Error, start, end };
            }
        };

        let token_kind = if mantissa_kind == MantissaKind::Float || exponent_present {
            TokenKind::Double
        } else {
            TokenKind::Integer
        };
        Token { kind: token_kind, start, end: self.current_pos }
    }

    /*
     *
     **/
    fn eat_string_literal(&mut self) -> Token {
        let start = self.current_pos;
        debug_assert!(matches!(self.peek(), Some(b'"' | b'\'')));
        let delimiter = self.peek().unwrap();

        self.bump();
        loop {
            let Some(b) = self.peek() else {
                let end = self.current_pos;
                self.lex_error(LexError::UnterminatedString(delimiter), start, end);
                return Token { kind: TokenKind::Error, start, end };
            };

            if b == delimiter {
                self.bump();
                break;
            }

            if b == b'\\' {
                self.bump();
                if self.bump_or_error().is_err() {
                    let end = self.current_pos;
                    self.lex_error(LexError::DanglingEscape, end - 1, end);
                    return Token { kind: TokenKind::Error, start, end };
                }
            } else {
                self.bump();
            }
        }

        Token {
            kind: TokenKind::StringLiteral,
            start,
            end: self.current_pos
        }
    }


    /*
     *
     **/
    fn eat_whitespace(&mut self) -> Token {
        let start = self.current_pos;
        while self.peek().is_some_and(|b| b.is_ascii_whitespace()) {
            self.bump();
        }

        Token {
            kind: TokenKind::Whitespace,
            start,
            end: self.current_pos,
        }
    }

    /*
     *
     **/
    fn advance_token(&mut self) -> Token {
        let Some(b) = self.peek() else {
            return Token {
                kind: TokenKind::Eof,
                start: self.current_pos,
                end: self.current_pos,
            };
        };

        if b.is_ascii_whitespace() {
            return self.eat_whitespace();
        }

        if matches!(self.peek2(), Some((b'/', b'/'))) {
            let start = self.current_pos;
            self.consume_while(|b| b == b'/');
            self.consume_while(|b| b != b'\n');
            return Token { kind: TokenKind::Comment, start, end: self.current_pos };
        }

        let start = self.current_pos;
        match self.peek().and_then(|b| self.lookup_operator(b)) {
            Some(operator) => {
                self.bump();
                let end = self.current_pos;
                return Token { kind: TokenKind::Operator(operator), start, end };
            },
            None => {},
        }

        if self.is_identifier_start(b) {
            return self.eat_identifier(start);
        } else if self.is_number_start(b) {
            return self.eat_number(start);
        } else if b == b'"' || b == b'\'' {
            return self.eat_string_literal();
        } else if b == b';' {
            self.bump();
            return Token { kind: TokenKind::Semicolon, start, end: self.current_pos };
        } else if b == b':' {
            self.bump();
            return Token { kind: TokenKind::Colon, start, end: self.current_pos };
        } else if b == b'=' {
            self.bump();
            return Token { kind: TokenKind::Assignment, start, end: self.current_pos };
        } else if b == b'(' {
            self.bump();
            return Token { kind: TokenKind::LParen, start, end: self.current_pos };
        } else if b == b')' {
            self.bump();
            return Token { kind: TokenKind::RParen, start, end: self.current_pos };
        } else if b == b'{' {
            self.bump();
            return Token { kind: TokenKind::LBrace, start, end: self.current_pos };
        } else if b == b'}' {
            self.bump();
            return Token { kind: TokenKind::RBrace, start, end: self.current_pos };
        }
        self.bump();

        let end = self.current_pos;
        self.lex_error(LexError::UnexpectedChar(b), start, end);
        Token { kind: TokenKind::Error, start, end }
    }
}

pub fn tokenize(input: String) -> LexerOutput {
    let mut lexer = Lexer {
        src: input.clone(),
        current_pos: 0,
        errors: Vec::new(),
        line_starts: Vec::new(),
    };
    lexer.compute_line_starts();

    let mut lexer_output = LexerOutput {
        src: input.clone(),
        tokens: Vec::new(),
        errors: lexer.errors.clone(),
        line_starts: lexer.line_starts.clone(),
    };

    loop {
        let token = lexer.advance_token();
        let kind = token.kind.clone();
        if kind == TokenKind::Whitespace { continue; }
        if kind == TokenKind::Comment { continue; }
        lexer_output.tokens.push(token);
        if kind == TokenKind::Eof { break; }
    }
    return lexer_output;
}

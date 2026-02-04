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
pub enum LexError {
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
    Function,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Operator {
    Plus,
    Minus,
    Division,
    Multiplication,
    Modulus,
    Not, // !
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
    Comma,
    Integer,
    Double,
    Error,
    Eof,
}

#[derive(Debug, Copy, Clone)]
pub struct Span { pub start: usize, pub end: usize }

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LexDiagnostic {
    pub kind: LexError,
    pub span: Span,
}

struct Lexer {
    file: SourceFile,
    current_pos: usize,
    errors: Vec<LexDiagnostic>,
}

#[derive(Debug)]
pub struct LexerOutput {
    pub file: SourceFile,
    pub tokens: Vec<Token>,
    pub errors: Vec<LexDiagnostic>,
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub name: String,
    pub src: String,
    pub line_starts: Vec<usize>,
}

fn compute_line_starts(src: &String) -> Vec<usize> {
    let mut line_starts = Vec::new();
    let mut byte_index: usize = 0;
    line_starts.push(byte_index);

    let bytes = src.as_bytes();
    while byte_index < bytes.len() {
        let b = bytes[byte_index];
        byte_index += 1;
        if b == b'\n' { line_starts.push(byte_index); }
    }
    line_starts
}

impl SourceFile {
    fn new(name: String, src: String) -> Self {
        let line_starts = compute_line_starts(&src);
        Self { name, src, line_starts }
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }

    pub fn slice(&self, token: &Token) -> &str {
        &self.src[token.span.start..token.span.end]
    }

    pub fn line_col(&self, pos: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&pos) {
            Ok(i) => i,
            Err(k) => k - 1, // only valid if k > 0
        };
        let col = pos - self.line_starts[line];
        (line, col)
    }

    pub fn line_span(&self, pos: usize) -> &str {
        let (line, _) = self.line_col(pos);
        let start = self.line_starts[line];

        let mut end = if line + 1 < self.line_starts.len() {
            self.line_starts[line + 1]
        } else {
            self.src.len()
        };

        let bytes = self.src.as_bytes();
        if end > start && bytes[end - 1] == b'\n' {
            end -= 1;
        }

        if end > start && bytes[end - 1] == b'\r' {
            end -= 1;
        }
        &self.src[start..end]
    }
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
        let bytes = self.file.src.as_bytes();
        let b0 = bytes.get(self.current_pos).copied()?;
        let b1 = bytes.get(self.current_pos+1).copied()?;
        Some((b0, b1))
    }

    fn peek(&self) -> Option<u8> {
        self.file.src.as_bytes().get(self.current_pos).copied()
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
            "function" => Some(Keyword::Function),
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
            b'!' => Some(Operator::Not),
            _ => None,
        }
    }

    fn token(&self, kind: TokenKind, start: usize, end: usize) -> Token {
        let line_span = Span { start, end };
        Token { kind, span: line_span }
    }

    fn lex_error(&mut self, kind: LexError, start: usize, end: usize) -> Token {
        self.errors.push(LexDiagnostic { kind, span: Span { start, end }});
        self.token(TokenKind::Error, start, end)
    }

    /*
     *
     **/
    fn eat_identifier(&mut self, start: usize) -> Token {
        while self.peek().is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_') {
            self.bump();
        }

        let lexeme = &self.file.src[start..self.current_pos];
        if let Some(keyword_kind) = self.lookup_keyword(lexeme) {
            return self.token(TokenKind::Keyword(keyword_kind), start, self.current_pos)
        }
        self.token(TokenKind::Identifier, start, self.current_pos)
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
                return self.lex_error(LexError::InvalidMantissa, start, end);
            }
        };

        let exponent_present = match self.scan_exponent() {
            Ok(present) => present,
            Err(_) => {
                let end = self.current_pos;
                return self.lex_error(LexError::InvalidExponent, start, end);
            }
        };

        let token_kind = if mantissa_kind == MantissaKind::Float || exponent_present {
            TokenKind::Double
        } else {
            TokenKind::Integer
        };
        self.token(token_kind, start, self.current_pos)
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
                return self.lex_error(LexError::UnterminatedString(delimiter), start, end);
            };

            if b == delimiter {
                self.bump();
                break;
            }

            if b == b'\\' {
                self.bump();
                if self.bump_or_error().is_err() {
                    let end = self.current_pos;
                    return self.lex_error(LexError::DanglingEscape, end - 1, end);
                }
            } else {
                self.bump();
            }
        }
        self.token(TokenKind::StringLiteral, start, self.current_pos)
    }


    /*
     *
     **/
    fn eat_whitespace(&mut self) -> Token {
        let start = self.current_pos;
        while self.peek().is_some_and(|b| b.is_ascii_whitespace()) {
            self.bump();
        }
        self.token(TokenKind::Whitespace, start, self.current_pos)
    }

    /*
     *
     **/
    fn advance_token(&mut self) -> Token {
        let Some(b) = self.peek() else {
            let current_pos = self.current_pos;
            return self.token(TokenKind::Eof, current_pos, current_pos)
        };

        if b.is_ascii_whitespace() {
            return self.eat_whitespace();
        }

        if matches!(self.peek2(), Some((b'/', b'/'))) {
            let start = self.current_pos;
            self.consume_while(|b| b == b'/');
            self.consume_while(|b| b != b'\n');
            return self.token(TokenKind::Comment, start, self.current_pos);
        }

        let start = self.current_pos;
        match self.peek().and_then(|b| self.lookup_operator(b)) {
            Some(operator) => {
                self.bump();
                let end = self.current_pos;
                return self.token(TokenKind::Operator(operator), start, end);
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
            return self.token(TokenKind::Semicolon, start, self.current_pos);
        } else if b == b':' {
            self.bump();
            return self.token(TokenKind::Colon, start, self.current_pos);
        } else if b == b',' {
            self.bump();
            return self.token(TokenKind::Comma, start, self.current_pos);
        } else if b == b'=' {
            self.bump();
            return self.token(TokenKind::Assignment, start, self.current_pos);
        } else if b == b'(' {
            self.bump();
            return self.token(TokenKind::LParen, start, self.current_pos);
        } else if b == b')' {
            self.bump();
            return self.token(TokenKind::RParen, start, self.current_pos);
        } else if b == b'{' {
            self.bump();
            return self.token(TokenKind::LBrace, start, self.current_pos);
        } else if b == b'}' {
            self.bump();
            return self.token(TokenKind::RBrace, start, self.current_pos);
        }
        self.bump();

        let end = self.current_pos;
        self.lex_error(LexError::UnexpectedChar(b), start, end)
    }
}

pub fn tokenize(name: String, input: String) -> LexerOutput {
    let mut lexer = Lexer {
        file: SourceFile::new(name.clone(), input.clone()),
        current_pos: 0,
        errors: Vec::new(),
    };

    let mut lexer_output = LexerOutput {
        file: lexer.file.clone(),
        tokens: Vec::new(),
        errors: Vec::new(),
    };

    loop {
        let token = lexer.advance_token();
        let kind = token.kind.clone();
        if kind == TokenKind::Whitespace { continue; }
        if kind == TokenKind::Comment { continue; }
        lexer_output.tokens.push(token);
        if kind == TokenKind::Eof { break; }
    }

    lexer_output.errors = lexer.errors;
    return lexer_output;
}

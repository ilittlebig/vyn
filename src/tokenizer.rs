/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-01
 **/

#[derive(PartialEq)]
enum MantissaKind {
    Int,
    Float
}

enum MantissaError {
    MissingDigits,
}

enum ExponentError {
    MissingDigits,
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum Keyword {
    LOCAL,
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum Operator {
    Equal,
    Plus,
    Minus,
    Division,
    Multiplication,
    Modulus
}

#[derive(PartialEq, Debug)]
enum TokenKind {
    WHITESPACE,
    IDENTIFIER,
    KEYWORD(Keyword),
    OPERATOR(Operator),
    SEMICOLON,
    INTEGER,
    DOUBLE,
    UNKNOWN,
    ERROR,
    EOF,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    start: usize,
    end: usize,
}

struct Tokenizer {
    input_str: String,
    current_pos: usize,
}

impl Tokenizer {
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
        let bytes = self.input_str.as_bytes();
        let b0 = bytes.get(self.current_pos).copied()?;
        let b1 = bytes.get(self.current_pos+1).copied()?;
        Some((b0, b1))
    }

    fn peek(&self) -> Option<u8> {
        self.input_str.as_bytes().get(self.current_pos).copied()
    }

    fn bump(&mut self) {
        self.current_pos += 1;
    }

    /*
    // TODO: not implemented yet
    fn consume_predicate(&mut self, pred: F) -> usize {
    }
    */

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
            "local" => Some(Keyword::LOCAL),
            _ => None,
        }
    }

    fn lookup_operator(&self, b: u8) -> Option<Operator> {
        match b {
            b'=' => Some(Operator::Equal),
            b'+' => Some(Operator::Plus),
            b'-' => Some(Operator::Minus),
            b'/' => Some(Operator::Division),
            b'*' => Some(Operator::Multiplication),
            b'%' => Some(Operator::Modulus),
            _ => None,
        }
    }

    /*
     *
     **/
    fn eat_identifier(&mut self, start: usize) -> Token {
        while self.peek().is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_') {
            self.bump();
        }

        let lexeme = &self.input_str[start..self.current_pos];
        if let Some(keyword_kind) = self.lookup_keyword(lexeme) {
            return Token {
                kind: TokenKind::KEYWORD(keyword_kind),
                start,
                end: self.current_pos,
            };
        }

        Token {
            kind: TokenKind::IDENTIFIER,
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
            Err(_) => return Token { kind: TokenKind::ERROR, start, end: self.current_pos },
        };

        let exponent_present = match self.scan_exponent() {
            Ok(present) => present,
            Err(_) => return Token { kind: TokenKind::ERROR, start, end: self.current_pos }
        };

        let token_kind = if mantissa_kind == MantissaKind::Float || exponent_present {
            TokenKind::DOUBLE
        } else {
            TokenKind::INTEGER
        };
        Token { kind: token_kind, start, end: self.current_pos }
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
            kind: TokenKind::WHITESPACE,
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
                kind: TokenKind::EOF,
                start: self.current_pos,
                end: self.current_pos,
            };
        };

        if b.is_ascii_whitespace() {
            return self.eat_whitespace();
        }

        let start = self.current_pos;
        match self.peek().and_then(|b| self.lookup_operator(b)) {
            Some(operator) => {
                self.bump();
                let end = self.current_pos;
                return Token { kind: TokenKind::OPERATOR(operator), start, end };
            },
            None => {},
        }

        if self.is_identifier_start(b) {
            return self.eat_identifier(start);
        } else if self.is_number_start(b) {
            return self.eat_number(start);
        } else if b == b';' {
            let end = self.current_pos;
            self.bump();
            return Token { kind: TokenKind::SEMICOLON, start, end };
        }

        self.bump();
        let end = self.current_pos;
        Token { kind: TokenKind::UNKNOWN, start, end }
    }
}

pub fn tokenize(input: &str) {
    let mut tokenizer = Tokenizer {
        input_str: String::from("1..2 1234 1. .2.3 1+2 1e6 1.4e+62 1e-2 e2 123.445 4 1e . integer"),
        current_pos: 0,
    };

    loop {
        let token = tokenizer.advance_token();
        println!("{:?} {:?}", token.kind, tokenizer.input_str[token.start..token.end].to_string());
        if token.kind == TokenKind::EOF { break; }
    }

    println!("reached end of the tokenizer");
}

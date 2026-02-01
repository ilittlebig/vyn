/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-01
 **/

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
    EOF,
    UNKNOWN,
    ERROR,
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
    fn is_identifier_start(&self, ch: char) -> bool {
        ch == '_' || ch.is_alphabetic()
    }

    fn is_number_start(&self, ch: char) -> bool {
        if ch == '.' {
            let Some((_, ch2)) = self.peek2() else { return false };
            if !(ch2 as char).is_digit(10) { return false };
            return true
        };
        ch.is_digit(10)
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

    fn lookup_keyword(&self, lexeme: &str) -> Option<Keyword> {
        match lexeme {
            "local" => Some(Keyword::LOCAL),
            _ => None,
        }
    }

    fn lookup_operator(&self, lexeme: &str) -> Option<Operator> {
        match lexeme {
            "=" => Some(Operator::Equal),
            "+" => Some(Operator::Plus),
            "-" => Some(Operator::Minus),
            "/" => Some(Operator::Division),
            "*" => Some(Operator::Multiplication),
            "%" => Some(Operator::Modulus),
            _ => None,
        }
    }

    /*
     *
     **/
    fn eat_identifier(&mut self, start: usize) -> Token {
        let bytes = self.input_str.as_bytes();
        while self.current_pos < bytes.len() && bytes[self.current_pos].is_ascii_alphanumeric() {
            self.current_pos += 1;
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

    /*
     * number :=
     *     ( DIGIT+ ( '.' DIGIT* )? | '.' DIGIT+ )
     *     ( [eE] [+-]? DIGIT+ )?
     **/
    fn eat_number(&mut self, start: usize) -> Token {
        let mut seen_dot = false;
        let mut seen_exp = false;
        let mut seen_op = false;
        let mut exp_has_digits = false;

        let bytes = self.input_str.as_bytes();
        while self.current_pos < bytes.len() {
            let ch = self.peek().unwrap();
            if seen_exp {
                if (ch as char) == '+' || (ch as char) == '-' {
                    self.current_pos += 1;
                    if seen_op {
                        return Token {
                            kind: TokenKind::ERROR,
                            start,
                            end: self.current_pos,
                        }
                    }
                    seen_op = true;
                }

                let ch = self.peek().unwrap();
                if (ch as char).is_digit(10) {
                    self.current_pos += 1;
                    exp_has_digits = true;
                } else {
                    if !exp_has_digits {
                        return Token {
                            kind: TokenKind::ERROR,
                            start,
                            end: self.current_pos,
                        }
                    }
                    break;
                }
                continue
            }

            if (ch as char) == '.' {
                if seen_dot {
                    self.current_pos += 1;
                    return Token {
                        kind: TokenKind::ERROR,
                        start,
                        end: self.current_pos,
                    }
                }
                seen_dot = true;
            } else if (ch as char) == 'e' || (ch as char) == 'E' {
                if seen_exp {
                    self.current_pos += 1;
                    return Token {
                        kind: TokenKind::ERROR,
                        start,
                        end: self.current_pos,
                    }
                }
                seen_exp = true;
            } else if !(ch as char).is_digit(10) {
                break;
            }
            self.current_pos += 1;
        }

        if seen_dot || seen_exp {
            return Token {
                kind: TokenKind::DOUBLE,
                start,
                end: self.current_pos,
            }
        }

        return Token {
            kind: TokenKind::INTEGER,
            start,
            end: self.current_pos,
        }
    }

    /*
     *
     **/
    fn eat_whitespace(&mut self) -> Token {
        let bytes = self.input_str.as_bytes();
        let start = self.current_pos;

        while self.current_pos < bytes.len() && bytes[self.current_pos].is_ascii_whitespace() {
            self.current_pos += 1;
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
        let bytes = self.input_str.as_bytes();
        if self.current_pos >= bytes.len() {
            return Token {
                kind: TokenKind::EOF,
                start: self.current_pos,
                end: bytes.len()
            };
        }

        if bytes[self.current_pos].is_ascii_whitespace() {
            return self.eat_whitespace();
        }

        let start = self.current_pos;
        while self.current_pos < bytes.len() && !bytes[self.current_pos].is_ascii_whitespace() {
            let lexeme = &self.input_str[start..self.current_pos];
            if let Some(operator) = self.lookup_operator(lexeme) {
                let end = self.current_pos;
                return Token { kind: TokenKind::OPERATOR(operator), start, end };
            }

            let ch = bytes[self.current_pos] as char;
            if self.is_identifier_start(ch) {
                return self.eat_identifier(start);
            } else if self.is_number_start(ch) {
                return self.eat_number(start);
            } else if ch == ';' {
                let end = self.current_pos;
                self.current_pos += 1;
                return Token { kind: TokenKind::SEMICOLON, start, end };
            }

            self.current_pos += 1;
            break;
        }

        let end = self.current_pos;
        Token { kind: TokenKind::UNKNOWN, start, end }
    }
}

pub fn tokenize(input: &str) {
    let mut tokenizer = Tokenizer {
        input_str: String::from("1e6 1.4e+62 1e-2 e2 123.445 4 1e . integer"),
        current_pos: 0,
    };

    loop {
        let token = tokenizer.advance_token();
        println!("{:?} {:?}", token.kind, tokenizer.input_str[token.start..token.end].to_string());
        if token.kind == TokenKind::EOF { break; }
    }

    println!("reached end of the tokenizer");
}

use std::{collections::HashMap, str};

pub enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(String),
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    GT,
    LT,
    TRUE,
    FALSE,
    ELSE,
    IF,
    RETURN,
    EQ,
    NOT_EQ,
}

pub struct Lexer {
    pub input: Vec<u8>,
    pub position: usize,
    pub read_position: usize,
    pub ch: u8,
}

impl Lexer {
    fn init(input: String) -> Lexer {
        let lexer = Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        return lexer;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = *self.input.get(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b'\r' || self.ch == b' ' || self.ch == b'\n' || self.ch == b'\t' {
            self.read_char();
        }
    }
    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            b';' => Token::SEMICOLON,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b',' => Token::COMMA,
            b'+' => Token::PLUS,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b'/' => Token::SLASH,
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    return Token::EQ;
                }
                return Token::ASSIGN;
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    return Token::NOT_EQ;
                }
                return Token::BANG;
            }
            b'*' => Token::ASTERISK,
            b'-' => Token::MINUS,
            b'>' => Token::GT,
            b'<' => Token::LT,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                return match ident {
                    "let" => Token::LET,
                    "fn" => Token::FUNCTION,
                    "return" => Token::RETURN,
                    "if" => Token::IF,
                    "else" => Token::ELSE,
                    "true" => Token::TRUE,
                    "false" => Token::FALSE,
                    _ => Token::IDENT(ident.into()),
                };
            }
            b'0'..=b'9' => {
                return Token::INT(self.read_digit().into());
            }
            0 => Token::EOF,
            _ => Token::ILLEGAL,
        };

        return token;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            return 0;
        } else {
            return *self.input.get(self.read_position).unwrap();
        }
    }
    fn read_digit(&mut self) -> &str {
        let initial_position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char()
        }
        // cursed?
        return str::from_utf8(&self.input[initial_position..self.position]).unwrap();
    }

    fn read_identifier(&mut self) -> &str {
        let initial_position = self.position;
        while self.ch.is_ascii_alphabetic() {
            self.read_char()
        }
        // cursed?
        return str::from_utf8(&self.input[initial_position..self.position]).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn init_works() {
        let input = "ok";
        let lexer = Lexer::init(input.into());
        assert_eq!(lexer.input, vec![b'o', b'k']);
        assert_eq!(lexer.position, 0);
        assert_eq!(lexer.read_position, 0);
        assert_eq!(lexer.ch, 0);
    }

    #[test]
    fn read_char() {
        let input = "ok";
        let mut lexer = Lexer::init(input.into());
        // now testing that read_char works
        lexer.read_char();
        assert_eq!(lexer.position, 0);
        assert_eq!(lexer.read_position, 1);
        assert_eq!(lexer.ch, b'o');
    }
}

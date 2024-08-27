use std::str;

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal,
    Eof,
    Ident(String),
    Int(String),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    Minus,
    Bang,
    Asterisk,
    Slash,
    GT,
    LT,
    True,
    False,
    Else,
    If,
    Return,
    Eq,
    NotEq,
}

pub struct Lexer {
    pub input: Vec<u8>,
    pub position: usize,
    pub read_position: usize,
    pub ch: u8,
}

impl Lexer {
    fn init(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
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
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            b';' => Token::Semicolon,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b',' => Token::Comma,
            b'+' => Token::Plus,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'/' => Token::Slash,
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'*' => Token::Asterisk,
            b'-' => Token::Minus,
            b'>' => Token::GT,
            b'<' => Token::LT,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_identifier();
                return match ident {
                    "let" => Token::Let,
                    "fn" => Token::Function,
                    "return" => Token::Return,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "true" => Token::True,
                    "false" => Token::False,
                    _ => Token::Ident(ident.into()),
                };
            }
            b'0'..=b'9' => return Token::Int(self.read_digit().into()),
            0 => Token::Eof,
            _ => Token::Illegal,
        };

        self.read_char();
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
        while self.ch.is_ascii_alphabetic() || self.ch == b'_' {
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
        assert_eq!(lexer.read_position, 1);
        assert_eq!(lexer.ch, b'o');
    }

    #[test]
    fn test_all_syntax() {
        let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        "#;
        let expected_tokens = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::Int("5".into()),
            Token::LT,
            Token::Int("10".into()),
            Token::GT,
            Token::Int("5".into()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5".into()),
            Token::LT,
            Token::Int("10".into()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int("10".into()),
            Token::Eq,
            Token::Int("10".into()),
            Token::Semicolon,
            Token::Int("10".into()),
            Token::NotEq,
            Token::Int("9".into()),
            Token::Semicolon,
            Token::Eof,
        ];

        let mut lexer = Lexer::init(input.into());
        for token in expected_tokens {
            let token_from_lexer = lexer.next_token();
            println!(
                "token_from_lexer: {:?}, token_from_expected: {:?}",
                token_from_lexer, token
            );
            debug_assert_eq!(token_from_lexer, token);
        }
    }
}

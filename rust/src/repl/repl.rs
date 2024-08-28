use crate::lexer::lexer::{Lexer, Token};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().expect("whoops?");
        let mut line = String::new();

        io::stdin()
            .read_line(&mut line)
            .expect("Expected valid utf-8");
        let mut lexer = Lexer::init(&line);
        let mut token = lexer.next_token();
        loop {
            if token == Token::Eof || token == Token::Illegal {
                break;
            }

            println!("{:?}", token);
            token = lexer.next_token();
        }
    }
}

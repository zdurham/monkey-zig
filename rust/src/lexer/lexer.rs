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
            //self.readPosition).unwrap_or('0');
        }
        self.position = self.read_position;
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
}

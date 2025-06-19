#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Literals
    Identifier,
    IntLiteral,
    StringLiteral,

    // Keywords
    Let,
    Const,
    Fn,
    If,
    Else,
    While,
    Break,
    Continue,
    Return,
    Static,
    Inline,
    True,
    False,

    // Types
    Bool,
    I32,

    // Operators
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Equal,    // ==
    NotEqual, // !=
    Assign,   // =

    // Punctuation
    LeftParen,  // (
    RightParen, // )
    LeftBrace,  // {
    RightBrace, // }
    Semicolon,  // ;
    Comma,      // ,
    Colon,      // :
    Arrow,      // ->

    // Formatting tokens
    Comment, // //
    Newline, // \n

    // Special
    Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub token_type: TokenType,
    pub start: u32,
    pub length: u32,
}

impl Token {
    pub fn new(token_type: TokenType, start: u32, length: u32) -> Self {
        Self {
            token_type,
            start,
            length,
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    current_char: char,
    line: u32,
    column: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let current_char = input.chars().next().unwrap_or('\0');
        Self {
            input,
            position: 0,
            current_char,
            line: 0,
            column: 0,
        }
    }

    fn advance(&mut self) {
        if self.current_char == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }

        self.position += 1;
        if self.position >= self.input.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.input.chars().nth(self.position).unwrap_or('\0');
        }
    }

    fn peek(&self) -> char {
        if self.position + 1 >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.position + 1).unwrap_or('\0')
        }
    }

    fn skip_whitespace(&mut self) {
        while self.current_char != '\0'
            && self.current_char.is_whitespace()
            && self.current_char != '\n'
        {
            self.advance();
        }
    }

    fn read_comment(&mut self) -> u32 {
        let start_pos = self.position;
        // Read // comments
        if self.current_char == '/' && self.peek() == '/' {
            while self.current_char != '\0' && self.current_char != '\n' {
                self.advance();
            }
        }
        (self.position - start_pos) as u32
    }

    fn read_identifier(&mut self) -> u32 {
        let start_pos = self.position;

        while self.current_char != '\0'
            && (self.current_char.is_alphanumeric() || self.current_char == '_')
        {
            self.advance();
        }

        (self.position - start_pos) as u32
    }

    fn read_number(&mut self) -> u32 {
        let start_pos = self.position;

        while self.current_char != '\0' && self.current_char.is_ascii_digit() {
            self.advance();
        }

        (self.position - start_pos) as u32
    }

    fn read_string(&mut self) -> u32 {
        let start_pos = self.position;
        self.advance(); // Skip opening quote

        while self.current_char != '\0' && self.current_char != '"' {
            if self.current_char == '\\' {
                self.advance();
                if self.current_char != '\0' {
                    self.advance();
                }
            } else {
                self.advance();
            }
        }

        if self.current_char == '"' {
            self.advance(); // Skip closing quote
        }

        (self.position - start_pos) as u32
    }

    fn keyword_or_identifier(&self, start_pos: usize, length: usize) -> TokenType {
        if start_pos + length > self.input.len() {
            return TokenType::Identifier;
        }

        let ident = &self.input[start_pos..start_pos + length];
        let first_char = ident.chars().next().unwrap_or('\0');

        match first_char {
            'b' => match ident {
                "bool" => TokenType::Bool,
                "break" => TokenType::Break,
                _ => TokenType::Identifier,
            },
            'c' => match ident {
                "const" => TokenType::Const,
                "continue" => TokenType::Continue,
                _ => TokenType::Identifier,
            },
            'e' => match ident {
                "else" => TokenType::Else,
                _ => TokenType::Identifier,
            },
            'f' => match ident {
                "fn" => TokenType::Fn,
                "false" => TokenType::False,
                _ => TokenType::Identifier,
            },
            'i' => match ident {
                "if" => TokenType::If,
                "inline" => TokenType::Inline,
                "i32" => TokenType::I32,
                _ => TokenType::Identifier,
            },
            'l' => match ident {
                "let" => TokenType::Let,
                _ => TokenType::Identifier,
            },
            'r' => match ident {
                "return" => TokenType::Return,
                _ => TokenType::Identifier,
            },
            's' => match ident {
                "static" => TokenType::Static,
                _ => TokenType::Identifier,
            },
            't' => match ident {
                "true" => TokenType::True,
                _ => TokenType::Identifier,
            },
            'w' => match ident {
                "while" => TokenType::While,
                _ => TokenType::Identifier,
            },
            _ => TokenType::Identifier,
        }
    }

    pub fn next_token(&mut self) -> Token {
        let start_pos = self.position as u32;

        match self.current_char {
            '\0' => Token::new(TokenType::Eof, start_pos, 0),

            '\n' => {
                self.advance();
                Token::new(TokenType::Newline, start_pos, 1)
            }

            '/' if self.peek() == '/' => {
                let length = self.read_comment();
                Token::new(TokenType::Comment, start_pos, length)
            }

            ch if ch.is_whitespace() => {
                self.skip_whitespace();
                self.next_token() // Skip whitespace and get next token
            }

            // Fast keyword/identifier lookup by first character
            'a'..='z' | 'A'..='Z' | '_' => {
                let length = self.read_identifier();
                let token_type = self.keyword_or_identifier(start_pos as usize, length as usize);
                Token::new(token_type, start_pos, length)
            }

            '0'..='9' => {
                let length = self.read_number();
                Token::new(TokenType::IntLiteral, start_pos, length)
            }

            '"' => {
                let length = self.read_string();
                Token::new(TokenType::StringLiteral, start_pos, length)
            }

            '+' => {
                self.advance();
                Token::new(TokenType::Plus, start_pos, 1)
            }

            '*' => {
                self.advance();
                Token::new(TokenType::Star, start_pos, 1)
            }

            '/' if self.peek() != '/' => {
                self.advance();
                Token::new(TokenType::Slash, start_pos, 1)
            }

            '=' => {
                self.advance();
                if self.current_char == '=' {
                    self.advance();
                    Token::new(TokenType::Equal, start_pos, 2)
                } else {
                    Token::new(TokenType::Assign, start_pos, 1)
                }
            }

            '!' => {
                self.advance();
                if self.current_char == '=' {
                    self.advance();
                    Token::new(TokenType::NotEqual, start_pos, 2)
                } else {
                    panic!("Unexpected character '!' at position {}", self.position);
                }
            }

            '-' => {
                self.advance();
                if self.current_char == '>' {
                    self.advance();
                    Token::new(TokenType::Arrow, start_pos, 2)
                } else {
                    Token::new(TokenType::Minus, start_pos, 1)
                }
            }

            '(' => {
                self.advance();
                Token::new(TokenType::LeftParen, start_pos, 1)
            }

            ')' => {
                self.advance();
                Token::new(TokenType::RightParen, start_pos, 1)
            }

            '{' => {
                self.advance();
                Token::new(TokenType::LeftBrace, start_pos, 1)
            }

            '}' => {
                self.advance();
                Token::new(TokenType::RightBrace, start_pos, 1)
            }

            ';' => {
                self.advance();
                Token::new(TokenType::Semicolon, start_pos, 1)
            }

            ',' => {
                self.advance();
                Token::new(TokenType::Comma, start_pos, 1)
            }

            ':' => {
                self.advance();
                Token::new(TokenType::Colon, start_pos, 1)
            }

            ch => {
                panic!(
                    "Unexpected character '{}' at position {}",
                    ch, self.position
                );
            }
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();
            let is_eof = matches!(token.token_type, TokenType::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        tokens
    }

    pub fn get_token_text(&self, token: &Token) -> &str {
        if token.length == 0 {
            ""
        } else {
            let start = token.start as usize;
            let end = start + token.length as usize;
            &self.input[start..end]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("let const fn if else while");
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 7); // 8 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Let);
        assert_eq!(tokens[1].token_type, TokenType::Const);
        assert_eq!(tokens[2].token_type, TokenType::Fn);
        assert_eq!(tokens[3].token_type, TokenType::If);
        assert_eq!(tokens[4].token_type, TokenType::Else);
        assert_eq!(tokens[5].token_type, TokenType::While);
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * / == != = ->");
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].token_type, TokenType::Plus);
        assert_eq!(tokens[1].token_type, TokenType::Minus);
        assert_eq!(tokens[2].token_type, TokenType::Star);
        assert_eq!(tokens[3].token_type, TokenType::Slash);
        assert_eq!(tokens[4].token_type, TokenType::Equal);
        assert_eq!(tokens[5].token_type, TokenType::NotEqual);
        assert_eq!(tokens[6].token_type, TokenType::Assign);
        assert_eq!(tokens[7].token_type, TokenType::Arrow);
    }

    #[test]
    fn test_literals() {
        let mut lexer = Lexer::new(r#"42 true false "hello world""#);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].token_type, TokenType::IntLiteral);
        assert_eq!(lexer.get_token_text(&tokens[0]), "42");
        assert_eq!(tokens[1].token_type, TokenType::True);
        assert_eq!(tokens[2].token_type, TokenType::False);
        assert_eq!(tokens[3].token_type, TokenType::StringLiteral);
        assert_eq!(lexer.get_token_text(&tokens[3]), r#""hello world""#);
    }

    #[test]
    fn test_function_syntax() {
        let mut lexer = Lexer::new("fn main() -> () { }");
        let tokens = lexer.tokenize();

        let expected = vec![
            TokenType::Fn,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::Arrow,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Eof,
        ];

        for (i, expected_type) in expected.iter().enumerate() {
            assert_eq!(&tokens[i].token_type, expected_type);
        }

        // Test identifier text extraction
        assert_eq!(lexer.get_token_text(&tokens[1]), "main");
    }
}

use crate::ast::Ast;
use crate::lexer::{Token, TokenType};

pub struct CodeFormatter<'a> {
    ast: &'a Ast,
    buffer: String,
    current_token_index: usize,
}

impl<'a> CodeFormatter<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { 
            ast,
            buffer: String::with_capacity(1024), // Pre-allocate reasonable capacity
            current_token_index: 0,
        }
    }

    pub fn format(mut self) -> String {
        let tokens = self.ast.get_tokens();
        let mut current_indent = 0;
        let mut at_line_start = true;
        
        while self.current_token_index < tokens.len() {
            let token = &tokens[self.current_token_index];
            
            match token.token_type {
                TokenType::LeftBrace => {
                    if !self.buffer.ends_with(' ') && !self.buffer.is_empty() {
                        self.buffer.push(' ');
                    }
                    self.emit_current_token(current_indent);
                    current_indent += 1;
                    at_line_start = false;
                }
                TokenType::RightBrace => {
                    current_indent = current_indent.saturating_sub(1);
                    if !at_line_start {
                        self.buffer.push('\n');
                    }
                    self.emit_indent(current_indent);
                    self.emit_current_token(current_indent);
                    at_line_start = false;
                }
                TokenType::Semicolon => {
                    self.emit_current_token(current_indent);
                    at_line_start = false;
                }
                TokenType::Newline => {
                    self.emit_current_token(current_indent);
                    at_line_start = true;
                }
                TokenType::Comment => {
                    if at_line_start {
                        self.emit_indent(current_indent);
                    } else if !self.buffer.ends_with(' ') {
                        self.buffer.push(' ');
                    }
                    self.emit_current_token(current_indent);
                    at_line_start = false;
                }
                _ => {
                    if at_line_start && !matches!(token.token_type, TokenType::Comment | TokenType::Newline) {
                        self.emit_indent(current_indent);
                    }
                    self.emit_current_token(current_indent);
                    at_line_start = false;
                }
            }
            
            self.current_token_index += 1;
        }
        self.buffer
    }
    
    /// Emit the current token at current_token_index position
    fn emit_current_token(&mut self, _indent: usize) {
        let tokens = self.ast.get_tokens();
        if self.current_token_index >= tokens.len() {
            return;
        }
        
        let token = &tokens[self.current_token_index];
        
        match token.token_type {
            TokenType::Comment => {
                let comment_text = self.get_token_text(token).to_string();
                self.buffer.push_str(&comment_text);
            }
            
            TokenType::Newline => {
                self.buffer.push('\n');
            }
            
            // Handle semantic tokens that should be emitted as-is
            TokenType::Plus => {
                if !self.buffer.ends_with(' ') && !self.buffer.is_empty() {
                    self.buffer.push(' ');
                }
                self.buffer.push_str("+");
                self.buffer.push(' ');
            }
            
            TokenType::Equal => {
                if !self.buffer.ends_with(' ') && !self.buffer.is_empty() {
                    self.buffer.push(' ');
                }
                self.buffer.push_str("==");
                self.buffer.push(' ');
            }
            
            TokenType::NotEqual => {
                if !self.buffer.ends_with(' ') && !self.buffer.is_empty() {
                    self.buffer.push(' ');
                }
                self.buffer.push_str("!=");
                self.buffer.push(' ');
            }
            
            // Handle keywords (need space after most keywords)
            TokenType::Fn | TokenType::Let | TokenType::Const | TokenType::If | TokenType::Else | 
            TokenType::While | TokenType::Break | TokenType::Continue | TokenType::Return |
            TokenType::Static | TokenType::Inline => {
                let token_text = self.get_token_text(token).to_string();
                self.buffer.push_str(&token_text);
                self.buffer.push(' ');
            }
            
            // Handle punctuation that needs space after
            TokenType::Arrow => {
                let token_text = self.get_token_text(token).to_string();
                if !self.buffer.ends_with(' ') && !self.buffer.is_empty() {
                    self.buffer.push(' ');
                }
                self.buffer.push_str(&token_text);
                self.buffer.push(' ');
            }
            
            TokenType::Comma => {
                let token_text = self.get_token_text(token).to_string();
                self.buffer.push_str(&token_text);
                self.buffer.push(' ');
            }
            
            // Handle punctuation that needs space before
            TokenType::Colon => {
                let token_text = self.get_token_text(token).to_string();
                self.buffer.push_str(&token_text);
                self.buffer.push(' ');
            }
            
            // Handle punctuation with no extra spacing
            TokenType::LeftParen | TokenType::RightParen | TokenType::LeftBrace | TokenType::RightBrace |
            TokenType::Semicolon | TokenType::Assign => {
                let token_text = self.get_token_text(token).to_string();
                self.buffer.push_str(&token_text);
            }
            
            // Handle literal and type tokens
            TokenType::IntLiteral | TokenType::True | TokenType::False | TokenType::StringLiteral | TokenType::Identifier |
            TokenType::Bool | TokenType::I32 => {
                let token_text = self.get_token_text(token).to_string();
                self.buffer.push_str(&token_text);
            }
            
            _ => {
                // Skip other tokens
            }
        }
    }
    
    /// Get the text for a token from the original source
    fn get_token_text(&self, token: &Token) -> &str {
        if token.length == 0 {
            ""
        } else {
            let source = self.ast.get_source();
            let start = token.start as usize;
            let end = start + token.length as usize;
            &source[start..end]
        }
    }

    fn emit_indent(&mut self, indent: usize) {
        for _ in 0..indent {
            self.buffer.push_str("    ");
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::parse::Parser;

    use super::*;

    fn roundtrip(input: &str) {
        // NEVER fix tests by changing this function - fix the implementations that we are testing
        let ast = Parser::parse(input).unwrap();
        let formatter = CodeFormatter::new(&ast);
        let output = formatter.format();
        assert_eq!(input, output);
    }

    #[test]
    fn test_comment_formatting() {
        roundtrip(r#"// This is a comment
fn main() {
    // Another comment
    return 42; // Inline comment
}"#);
    }

    #[test]
    fn test_comprehensive_formatting_roundtrip() {
        roundtrip(r#"// File header comment
// Another header line

// Function documentation
fn main(x: i32, y: bool) -> i32 {
    // Simple return with comment
    return 42; // Inline comment
} // End of function"#);
    }

    #[test]
    fn test_edge_case_formatting_tokens() {
        // Test various edge cases for formatting token placement
        roundtrip(r#"// Leading comment

fn main() -> bool {
    return true;
}
// Trailing comment"#);
    }
}
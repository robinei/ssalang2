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
        
        while self.current_token_index < tokens.len() {
            let token = &tokens[self.current_token_index];
            
            match token.token_type {
                TokenType::LeftBrace => {
                    self.emit_current_token(current_indent);
                    self.buffer.push('\n');
                    current_indent += 1;
                }
                TokenType::RightBrace => {
                    current_indent = current_indent.saturating_sub(1);
                    self.buffer.push('\n');
                    self.emit_indent(current_indent);
                    self.emit_current_token(current_indent);
                }
                TokenType::Semicolon => {
                    self.emit_current_token(current_indent);
                    self.buffer.push('\n');
                }
                _ => {
                    self.emit_current_token(current_indent);
                }
            }
            
            self.current_token_index += 1;
        }
        self.buffer
    }
    
    /// Emit the current token at current_token_index position
    fn emit_current_token(&mut self, indent: usize) {
        let tokens = self.ast.get_tokens();
        if self.current_token_index >= tokens.len() {
            return;
        }
        
        let token = &tokens[self.current_token_index];
        
        match token.token_type {
            TokenType::Comment => {
                let comment_text = self.get_token_text(token).to_string();
                
                if !self.buffer.is_empty() && !self.buffer.ends_with('\n') {
                    // Inline comment - single space separation
                    self.buffer.push(' ');
                    self.buffer.push_str(&comment_text);
                } else {
                    // Standalone comment - align with current indentation  
                    self.emit_indent(indent);
                    self.buffer.push_str(&comment_text);
                }
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
            TokenType::Fn | TokenType::Let | TokenType::Mut | TokenType::If | TokenType::Else | 
            TokenType::While | TokenType::Break | TokenType::Continue | TokenType::Return |
            TokenType::Static | TokenType::Inline => {
                let token_text = self.get_token_text(token).to_string();
                self.buffer.push_str(&token_text);
                self.buffer.push(' ');
            }
            
            // Handle punctuation that needs space after
            TokenType::Arrow | TokenType::Comma => {
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
            TokenType::Bool | TokenType::I32 | TokenType::Void => {
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
            self.buffer.push_str("  ");
        }
    }
}


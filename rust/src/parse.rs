use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenType};
use smallvec::SmallVec;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    ast: Ast,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub source_loc: u32,
}

impl ParseError {
    pub fn new(message: String, source_loc: u32) -> Self {
        Self { message, source_loc }
    }
}

type ParseResult<T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token();
        
        Self {
            lexer,
            current_token,
            ast: Ast::new(),
        }
    }
    
    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }
    
    fn expect(&mut self, expected: TokenType) -> ParseResult<Token> {
        let token = self.current_token;
        if token.token_type == expected {
            self.advance();
            Ok(token)
        } else {
            Err(ParseError::new(
                format!("Expected {:?}, found {:?}", expected, token.token_type),
                token.source_loc,
            ))
        }
    }
    
    // Parse the entire program
    pub fn parse_program(&mut self) -> ParseResult<NodeRef> {
        let root = self.parse_function()?;
        self.ast.set_root(root);
        Ok(root)
    }
    
    // Parse a function definition
    fn parse_function(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        // Parse function flags
        let mut flags = Flags::new();
        
        // Check for static/inline modifiers
        if self.current_token.token_type == TokenType::Static {
            flags.set_is_static(true);
            self.advance();
        }
        
        if self.current_token.token_type == TokenType::Inline {
            flags.set_is_inline(true);
            self.advance();
        }
        
        self.expect(TokenType::Fn)?;
        
        // Function name (ignored for now)
        if let TokenType::Identifier = &self.current_token.token_type {
            self.advance();
        } else {
            return Err(ParseError::new("Expected function name".to_string(), self.current_token.source_loc));
        }
        
        self.expect(TokenType::LeftParen)?;
        
        // Parse parameters
        let mut locals: SmallVec<[Local; 8]> = SmallVec::new();
        if self.current_token.token_type != TokenType::RightParen {
            loop {
                let param = self.parse_parameter()?;
                locals.push(param);
                
                if self.current_token.token_type == TokenType::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        
        self.expect(TokenType::RightParen)?;
        
        // Parse return type
        let return_type = if self.current_token.token_type == TokenType::Arrow {
            self.advance();
            self.parse_type()?
        } else {
            // Default to void
            self.ast.add_node(Node::TypeAtom(TypeAtom::Void), NodeInfo::new(source_loc))
        };
        
        // Parse function body
        let body = self.parse_block()?;
        
        // Add locals to the AST
        let locals_ref = if locals.is_empty() {
            LocalsRef::empty()
        } else {
            self.ast.add_locals(&locals)
        };
        
        // Create function node
        let func_node = Node::Func(flags, locals_ref, body, return_type);
        Ok(self.ast.add_node(func_node, NodeInfo::new(source_loc)))
    }
    
    // Parse a function parameter
    fn parse_parameter(&mut self) -> ParseResult<Local> {
        let _param_name = if let TokenType::Identifier = &self.current_token.token_type {
            self.advance();
        } else {
            return Err(ParseError::new("Expected parameter name".to_string(), self.current_token.source_loc));
        };
        
        self.expect(TokenType::Colon)?;
        let param_type = self.parse_type()?;
        
        Ok(Local {
            is_param: true,
            is_static: false,
            is_const: false,
            ty: param_type,
        })
    }
    
    // Parse a type
    fn parse_type(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        let type_atom = match &token.token_type {
            TokenType::Bool => TypeAtom::Bool,
            TokenType::I32 => TypeAtom::I32,
            TokenType::Void => TypeAtom::Void,
            _ => return Err(ParseError::new("Expected type".to_string(), source_loc)),
        };
        
        self.advance();
        Ok(self.ast.add_node(Node::TypeAtom(type_atom), NodeInfo::new(source_loc)))
    }
    
    // Parse a block statement
    fn parse_block(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        self.expect(TokenType::LeftBrace)?;
        
        let flags = Flags::new();
        
        // For now, just parse the first statement as the block content
        let first_child = if self.current_token.token_type == TokenType::RightBrace {
            // Empty block - create a void expression
            self.ast.add_node(Node::TypeAtom(TypeAtom::Void), NodeInfo::new(source_loc))
        } else {
            self.parse_statement()?
        };
        
        self.expect(TokenType::RightBrace)?;
        
        let block_node = Node::Block(flags, 0, first_child);
        Ok(self.ast.add_node(block_node, NodeInfo::new(source_loc)))
    }
    
    // Parse a statement
    fn parse_statement(&mut self) -> ParseResult<NodeRef> {
        match &self.current_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Break => self.parse_break_statement(),
            TokenType::Continue => self.parse_continue_statement(),
            TokenType::LeftBrace => self.parse_block(),
            _ => {
                let expr = self.parse_expression()?;
                if self.current_token.token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(expr)
            }
        }
    }
    
    // Parse let statement
    fn parse_let_statement(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        self.expect(TokenType::Let)?;
        
        // Check for mut keyword
        let _is_mutable = if self.current_token.token_type == TokenType::Mut {
            self.advance();
            true
        } else {
            false
        };
        
        // Variable name (for local index, we'll use a dummy value)
        if let TokenType::Identifier = &self.current_token.token_type {
            self.advance();
        } else {
            return Err(ParseError::new("Expected variable name".to_string(), self.current_token.source_loc));
        }
        
        self.expect(TokenType::Assign)?;
        let expr = self.parse_expression()?;
        
        if self.current_token.token_type == TokenType::Semicolon {
            self.advance();
        }
        
        // Create a LocalWrite node (is_definition=true, dummy local_index=0)
        let local_write = Node::LocalWrite(true, 0, expr);
        Ok(self.ast.add_node(local_write, NodeInfo::new(source_loc)))
    }
    
    // Parse if statement
    fn parse_if_statement(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        self.expect(TokenType::If)?;
        
        let cond = self.parse_expression()?;
        let then_block = self.parse_block()?;
        
        let else_block = if self.current_token.token_type == TokenType::Else {
            self.advance();
            if self.current_token.token_type == TokenType::If {
                // else if
                self.parse_if_statement()?
            } else {
                // else block
                self.parse_block()?
            }
        } else {
            // No else clause - use void
            self.ast.add_node(Node::TypeAtom(TypeAtom::Void), NodeInfo::new(source_loc))
        };
        
        let flags = Flags::new();
        let if_node = Node::If(flags, 0, cond, then_block, else_block);
        Ok(self.ast.add_node(if_node, NodeInfo::new(source_loc)))
    }
    
    // Parse while statement
    fn parse_while_statement(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        self.expect(TokenType::While)?;
        
        let cond = self.parse_expression()?;
        let body = self.parse_block()?;
        
        let flags = Flags::new();
        let while_node = Node::While(flags, 0, cond, body);
        Ok(self.ast.add_node(while_node, NodeInfo::new(source_loc)))
    }
    
    // Parse return statement
    fn parse_return_statement(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        self.expect(TokenType::Return)?;
        
        let value = if self.current_token.token_type == TokenType::Semicolon {
            // Return void
            self.ast.add_node(Node::TypeAtom(TypeAtom::Void), NodeInfo::new(source_loc))
        } else {
            self.parse_expression()?
        };
        
        if self.current_token.token_type == TokenType::Semicolon {
            self.advance();
        }
        
        let return_node = Node::Return(value);
        Ok(self.ast.add_node(return_node, NodeInfo::new(source_loc)))
    }
    
    // Parse break statement
    fn parse_break_statement(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        self.expect(TokenType::Break)?;
        
        let value = if self.current_token.token_type == TokenType::Semicolon {
            // Break with void
            self.ast.add_node(Node::TypeAtom(TypeAtom::Void), NodeInfo::new(source_loc))
        } else {
            self.parse_expression()?
        };
        
        if self.current_token.token_type == TokenType::Semicolon {
            self.advance();
        }
        
        let flags = Flags::new();
        let break_node = Node::Break(flags, 0, value);
        Ok(self.ast.add_node(break_node, NodeInfo::new(source_loc)))
    }
    
    // Parse continue statement
    fn parse_continue_statement(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        self.expect(TokenType::Continue)?;
        
        let value = if self.current_token.token_type == TokenType::Semicolon {
            // Continue with void
            self.ast.add_node(Node::TypeAtom(TypeAtom::Void), NodeInfo::new(source_loc))
        } else {
            self.parse_expression()?
        };
        
        if self.current_token.token_type == TokenType::Semicolon {
            self.advance();
        }
        
        let flags = Flags::new();
        let continue_node = Node::Continue(flags, 0, value);
        Ok(self.ast.add_node(continue_node, NodeInfo::new(source_loc)))
    }
    
    // Parse expression (handles binary operators)
    fn parse_expression(&mut self) -> ParseResult<NodeRef> {
        self.parse_equality()
    }
    
    // Parse equality operators (== !=)
    fn parse_equality(&mut self) -> ParseResult<NodeRef> {
        let mut expr = self.parse_addition()?;
        
        while self.current_token.token_type == TokenType::Equal || self.current_token.token_type == TokenType::NotEqual {
            let token = self.current_token;
            let source_loc = token.source_loc;
            let op = token.token_type.clone();
            self.advance();
            
            let right = self.parse_addition()?;
            
            let node = match op {
                TokenType::Equal => Node::BinopEq(expr, right),
                TokenType::NotEqual => Node::BinopNeq(expr, right),
                _ => unreachable!(),
            };
            
            expr = self.ast.add_node(node, NodeInfo::new(source_loc));
        }
        
        Ok(expr)
    }
    
    // Parse addition operator (+)
    fn parse_addition(&mut self) -> ParseResult<NodeRef> {
        let mut expr = self.parse_primary()?;
        
        while self.current_token.token_type == TokenType::Plus {
            let token = self.current_token;
            let source_loc = token.source_loc;
            self.advance();
            
            let right = self.parse_primary()?;
            let add_node = Node::BinopAdd(expr, right);
            expr = self.ast.add_node(add_node, NodeInfo::new(source_loc));
        }
        
        Ok(expr)
    }
    
    // Parse primary expressions (literals, identifiers, parentheses)
    fn parse_primary(&mut self) -> ParseResult<NodeRef> {
        let token = self.current_token;
        let source_loc = token.source_loc;
        
        match &token.token_type {
            TokenType::Minus => {
                self.advance();
                let expr = self.parse_primary()?;
                // For simplicity, just create 0 - expr
                let zero = self.ast.add_node(Node::ConstI32(0), NodeInfo::new(source_loc));
                let neg_node = Node::BinopAdd(zero, expr); // This should be subtract, but we only have add
                Ok(self.ast.add_node(neg_node, NodeInfo::new(source_loc)))
            }
            TokenType::IntLiteral => {
                let token_text = self.lexer.get_token_text(&token);
                let value = token_text.parse::<i32>().unwrap_or(0);
                self.advance();
                let const_node = Node::ConstI32(value);
                Ok(self.ast.add_node(const_node, NodeInfo::new(source_loc)))
            }
            
            TokenType::True => {
                self.advance();
                let const_node = Node::ConstBool(true);
                Ok(self.ast.add_node(const_node, NodeInfo::new(source_loc)))
            }
            
            TokenType::False => {
                self.advance();
                let const_node = Node::ConstBool(false);
                Ok(self.ast.add_node(const_node, NodeInfo::new(source_loc)))
            }
            
            TokenType::StringLiteral => {
                let token_text = self.lexer.get_token_text(&token);
                // Remove surrounding quotes and handle escape sequences
                let value = self.parse_string_literal(token_text);
                self.advance();
                let string_ref = self.ast.add_string(value);
                let const_node = Node::ConstString(string_ref);
                Ok(self.ast.add_node(const_node, NodeInfo::new(source_loc)))
            }
            
            TokenType::Identifier => {
                self.advance();
                // For now, treat as local read with dummy index
                let local_read = Node::LocalRead(0);
                Ok(self.ast.add_node(local_read, NodeInfo::new(source_loc)))
            }
            
            TokenType::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(TokenType::RightParen)?;
                Ok(expr)
            }
            
            _ => Err(ParseError::new(
                format!("Unexpected token: {:?}", token.token_type),
                source_loc,
            )),
        }
    }
    
    fn parse_string_literal(&self, token_text: &str) -> String {
        // Remove surrounding quotes
        let content = &token_text[1..token_text.len()-1];
        let mut result = String::new();
        let mut chars = content.chars();
        
        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(escaped) = chars.next() {
                    match escaped {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        _ => {
                            result.push('\\');
                            result.push(escaped);
                        }
                    }
                }
            } else {
                result.push(ch);
            }
        }
        
        result
    }
    
    pub fn into_ast(self) -> Ast {
        self.ast
    }
}

pub fn parse_program(input: &str) -> ParseResult<Ast> {
    let mut parser = Parser::new(input);
    parser.parse_program()?;
    Ok(parser.into_ast())
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_simple_function() {
        let input = r#"
            fn main() -> void {
                return 42;
            }
        "#;
        
        let ast = parse_program(input).unwrap();
        let root = ast.get_root().unwrap();
        
        match ast.get_node(root) {
            Node::Func(_, _, body, _) => {
                match ast.get_node(*body) {
                    Node::Block(_, _, first_child) => {
                        match ast.get_node(*first_child) {
                            Node::Return(value) => {
                                match ast.get_node(*value) {
                                    Node::ConstI32(42) => {} // Success
                                    _ => panic!("Expected ConstI32(42)"),
                                }
                            }
                            _ => panic!("Expected Return node"),
                        }
                    }
                    _ => panic!("Expected Block node"),
                }
            }
            _ => panic!("Expected Func node"),
        }
    }
    
    #[test]
    fn test_parse_arithmetic() {
        let input = r#"
            fn main() -> i32 {
                return 1 + 2;
            }
        "#;
        
        let ast = parse_program(input).unwrap();
        let root = ast.get_root().unwrap();
        
        match ast.get_node(root) {
            Node::Func(_, _, body, _) => {
                match ast.get_node(*body) {
                    Node::Block(_, _, first_child) => {
                        match ast.get_node(*first_child) {
                            Node::Return(value) => {
                                match ast.get_node(*value) {
                                    Node::BinopAdd(left, right) => {
                                        match (ast.get_node(*left), ast.get_node(*right)) {
                                            (Node::ConstI32(1), Node::ConstI32(2)) => {} // Success
                                            _ => panic!("Expected 1 + 2"),
                                        }
                                    }
                                    _ => panic!("Expected BinopAdd"),
                                }
                            }
                            _ => panic!("Expected Return node"),
                        }
                    }
                    _ => panic!("Expected Block node"),
                }
            }
            _ => panic!("Expected Func node"),
        }
    }
    
    #[test]
    fn test_parse_if_statement() {
        let input = r#"
            fn main() -> void {
                if true {
                    return 1;
                } else {
                    return 2;
                }
            }
        "#;
        
        let ast = parse_program(input).unwrap();
        let root = ast.get_root().unwrap();
        
        match ast.get_node(root) {
            Node::Func(_, _, body, _) => {
                match ast.get_node(*body) {
                    Node::Block(_, _, first_child) => {
                        match ast.get_node(*first_child) {
                            Node::If(_, _, cond, then_block, else_block) => {
                                match ast.get_node(*cond) {
                                    Node::ConstBool(true) => {} // Success
                                    _ => panic!("Expected ConstBool(true)"),
                                }
                                // Check then and else blocks exist
                                assert!(matches!(ast.get_node(*then_block), Node::Block(_, _, _)));
                                assert!(matches!(ast.get_node(*else_block), Node::Block(_, _, _)));
                            }
                            _ => panic!("Expected If node"),
                        }
                    }
                    _ => panic!("Expected Block node"),
                }
            }
            _ => panic!("Expected Func node"),
        }
    }
    
    #[test]
    fn test_parse_let_statement() {
        let input = r#"
            fn main() -> void {
                let x = 42;
            }
        "#;
        
        let ast = parse_program(input).unwrap();
        let root = ast.get_root().unwrap();
        
        match ast.get_node(root) {
            Node::Func(_, _, body, _) => {
                match ast.get_node(*body) {
                    Node::Block(_, _, first_child) => {
                        match ast.get_node(*first_child) {
                            Node::LocalWrite(true, _, expr) => {
                                match ast.get_node(*expr) {
                                    Node::ConstI32(42) => {} // Success
                                    _ => panic!("Expected ConstI32(42)"),
                                }
                            }
                            _ => panic!("Expected LocalWrite node"),
                        }
                    }
                    _ => panic!("Expected Block node"),
                }
            }
            _ => panic!("Expected Func node"),
        }
    }
}
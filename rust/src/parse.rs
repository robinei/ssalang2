use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenType};
use smallvec::SmallVec;
use std::collections::HashMap;

// Cleanup operations for scope management
#[derive(Debug, Clone)]
enum ScopeCleanup {
    Delete(SymbolRef, BlockIndex), // Remove binding when exiting block
    Replace(SymbolRef, LocalRef, BlockIndex), // Restore shadowed binding when exiting block
}

pub struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    prev_token: Token,
    current_token: Token,
    source: String,
    ast: Ast,

    // Scope management for local bindings
    binding_map: HashMap<SymbolRef, LocalRef>, // Current symbol -> local ref mappings
    cleanup_stack: Vec<ScopeCleanup>,            // Stack of cleanup operations
    next_block_id: BlockIndex,                   // Counter for unique block IDs
    scope_stack: Vec<ScopeIndex>,                // Stack of currently active scope IDs
    block_stack: Vec<BlockIndex>,                // Stack of currently active block IDs
    
    // Function local collection
    function_locals: Vec<Local>,                 // Stack of locals for current scope being parsed
    current_scope_index: Option<ScopeIndex>,    // Current scope being parsed
    scope_locals_start: Vec<usize>,             // Starting index for each scope's locals in function_locals
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub source_loc: u32,
}

impl ParseError {
    pub fn new(message: String, source_loc: u32) -> Self {
        Self {
            message,
            source_loc,
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(tokens: Vec<Token>, source: String) -> Self {
        let mut parser = Self {
            tokens,
            token_index: 0,
            prev_token: Token::new(TokenType::Eof, 0, 0),
            current_token: Token::new(TokenType::Eof, 0, 0),
            source,
            ast: Ast::new(),
            binding_map: HashMap::new(),
            cleanup_stack: Vec::new(),
            next_block_id: 1, // Start block IDs at 1 (0 can be reserved for global/invalid)
            scope_stack: Vec::new(),
            block_stack: Vec::new(),
            function_locals: Vec::new(),
            current_scope_index: None,
            scope_locals_start: Vec::new(),
        };

        parser.update_current_token();

        parser
    }

    fn update_current_token(&mut self) {
        self.prev_token = self.current_token;

        // Skip formatting tokens to find next semantic token
        while self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index];
            if !matches!(
                self.current_token.token_type,
                TokenType::Comment | TokenType::EmptyLine
            ) {
                return; // accept semantic token
            }
            self.token_index += 1;
        }

        // Set EOF token if we've reached the end
        self.current_token = Token::new(TokenType::Eof, 0, 0);
    }

    fn advance(&mut self) {
        if self.token_index >= self.tokens.len() {
            return;
        }

        self.token_index += 1;
        self.update_current_token();
    }

    fn peek_next_token(&self) -> Token {
        let mut peek_index = self.token_index + 1;
        
        // Skip formatting tokens to find next semantic token
        while peek_index < self.tokens.len() {
            let token = self.tokens[peek_index];
            if !matches!(
                token.token_type,
                TokenType::Comment | TokenType::EmptyLine
            ) {
                return token; // return semantic token
            }
            peek_index += 1;
        }
        
        // Return EOF token if we've reached the end
        Token::new(TokenType::Eof, 0, 0)
    }

    fn expect(&mut self, expected: TokenType) -> ParseResult<Token> {
        let token = self.current_token;
        if token.token_type == expected {
            self.advance();
            Ok(token)
        } else {
            Err(ParseError::new(
                format!("Expected {:?}, found {:?}", expected, token.token_type),
                token.start,
            ))
        }
    }

    pub fn parse_root(&mut self) -> ParseResult<NodeRef> {
        let root = self.parse_module()?;
        self.ast.set_root(root);
        Ok(root)
    }

    fn parse_module(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;
        let mut nodes = Vec::new();

        let module_scope_id = self.enter_scope();

        while self.current_token.token_type != TokenType::Eof {
            let node = self.parse_top_level_definition()?;
            nodes.push(node);
        }

        self.exit_scope(module_scope_id);

        let nodes_ref = self.ast.add_node_refs(&nodes);
        let module_node = Node::Module(module_scope_id, nodes_ref);
        let info = NodeInfo::new(start_token_index);
        Ok(self.ast.add_node(module_node, info))
    }

    fn parse_top_level_definition(&mut self) -> ParseResult<NodeRef> {
        match self.current_token.token_type {
            TokenType::Const => self.parse_variable_declaration(true, false),
            TokenType::Inline | TokenType::Fn => self.parse_function(),
            _ => Err(ParseError::new(
                format!("Expected function or constant definition, found {:?}", self.current_token.token_type),
                self.current_token.start,
            ))
        }
    }

    fn parse_function(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        let is_static = self.parse_static_flag();
        let is_inline = self.parse_inline_flag();

        self.expect(TokenType::Fn)?;

        if self.current_token.token_type == TokenType::Identifier {
            // Named function: fn foo() {} -> const foo = fn() {}
            let name = self.get_token_text(&self.current_token).to_owned();
            let name_ref = self.ast.intern_symbol(name);
            self.advance();

            // Parse the function value
            let func_value = self.parse_function_value(is_static, is_inline)?;

            // Create a local binding for the function name
            let local = Local {
                name: name_ref,
                is_param: false,
                is_static: false, // Functions are not static by default
                is_const: true,   // Functions are const
                ty: None, // TODO: proper function type inference
            };

            self.create_function_definition(local, func_value, start_token_index)
        } else {
            // Anonymous function: fn() {}
            self.parse_function_value(is_static, is_inline)
        }
    }

    fn parse_function_value(&mut self, is_static: IsStatic, is_inline: IsInline) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        let function_scope_id = self.enter_scope();

        self.expect(TokenType::LeftParen)?;

        // Parse parameters directly into function_locals
        if self.current_token.token_type != TokenType::RightParen {
            loop {
                let param = self.parse_parameter()?;
                self.add_and_bind_local(param);

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
            // Default to unit
            self.ast.add_node(
                Node::TypeAtom(TypeAtom::Unit),
                NodeInfo::new(self.token_index),
            )
        };

        // Parse function body
        let body = self.parse_block()?;

        // Exit function scope
        self.exit_scope(function_scope_id);

        let func_node = Node::Func(is_static, is_inline, function_scope_id, body, return_type);
        Ok(self
            .ast
            .add_node(func_node, NodeInfo::new(start_token_index)))
    }

    // Parse a function parameter
    fn parse_parameter(&mut self) -> ParseResult<Local> {
        // Check for static modifier
        let is_static = if self.current_token.token_type == TokenType::Static {
            self.advance();
            true
        } else {
            false
        };

        let param_name = if let TokenType::Identifier = &self.current_token.token_type {
            let name_text = self.get_token_text(&self.current_token).to_string();
            self.advance();
            name_text
        } else {
            return Err(ParseError::new(
                "Expected parameter name".to_string(),
                self.current_token.start,
            ));
        };

        self.expect(TokenType::Colon)?;
        let param_type = self.parse_type()?;

        let name_ref = self.ast.intern_symbol(param_name);

        Ok(Local {
            name: name_ref,
            is_param: true,
            is_static,
            is_const: false,
            ty: Some(param_type),
        })
    }

    // Parse a type
    fn parse_type(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;
        let token = self.current_token;

        match &token.token_type {
            TokenType::Bool => {
                self.advance();
                Ok(self.ast.add_node(
                    Node::TypeAtom(TypeAtom::Bool),
                    NodeInfo::new(start_token_index),
                ))
            }
            TokenType::I32 => {
                self.advance();
                Ok(self.ast.add_node(
                    Node::TypeAtom(TypeAtom::I32),
                    NodeInfo::new(start_token_index),
                ))
            }
            TokenType::Unit => {
                self.advance();
                Ok(self.ast.add_node(
                    Node::TypeAtom(TypeAtom::Unit),
                    NodeInfo::new(start_token_index),
                ))
            }
            _ => Err(ParseError::new("Expected type".to_string(), token.start)),
        }
    }


    // Parse a block statement
    fn parse_block(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::LeftBrace)?;

        let block_id = self.enter_block();

        // Parse all statements in the block
        let mut statements: SmallVec<[NodeRef; 64]> = SmallVec::new();
        let mut last_had_semicolon = false;

        while self.current_token.token_type != TokenType::RightBrace {
            let stmt = self.parse_statement()?;
            statements.push(stmt);

            // Check if this statement ends with a semicolon
            last_had_semicolon = self.prev_token.token_type == TokenType::Semicolon;
        }

        // If empty block OR last statement had semicolon, add ConstUnit
        if statements.is_empty() || last_had_semicolon {
            let unit_node = self
                .ast
                .add_node(Node::ConstUnit, NodeInfo::new(self.token_index));
            statements.push(unit_node);
        }

        self.expect(TokenType::RightBrace)?;

        self.exit_block(block_id);

        let statements_ref = self.ast.add_node_refs(&statements);
        let block_node = Node::Block(IsStatic::No, block_id, statements_ref);
        Ok(self
            .ast
            .add_node(block_node, NodeInfo::new(start_token_index)))
    }

    // Parse a statement
    fn parse_statement(&mut self) -> ParseResult<NodeRef> {
        match &self.current_token.token_type {
            // Statement-only forms
            TokenType::Let => self.parse_variable_declaration(false, false),
            TokenType::Const => self.parse_variable_declaration(true, false),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Break => self.parse_break_statement(),
            TokenType::Continue => self.parse_continue_statement(),
            TokenType::Static => {
                self.advance();
                match self.current_token.token_type {
                    TokenType::Let => self.parse_variable_declaration(false, true),
                    TokenType::Const => self.parse_variable_declaration(true, true),
                    TokenType::Eof => Err(ParseError::new(
                        "Unexpected end of input after 'static'".to_string(),
                        self.current_token.start,
                    )),
                    _ => Err(ParseError::new(
                        "Expected 'let' or 'const' after 'static'".to_string(),
                        self.current_token.start,
                    )),
                }
            }
            TokenType::While => self.parse_while_statement(),
            // Check for assignment vs expression
            TokenType::Identifier => {
                // Look ahead to see if this is an assignment (identifier = expr)
                if self.peek_next_token().token_type == TokenType::Assign {
                    self.parse_assignment()
                } else {
                    self.parse_expression()
                }
            }
            // Expression forms
            TokenType::If => self.parse_if_statement(),
            TokenType::LeftBrace => self.parse_block(),
            _ => self.parse_expression(),
        }
    }

    fn parse_variable_declaration(&mut self, is_const: bool, is_static: bool) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        // Use the passed is_static flag, or check for static modifier if not already handled
        let is_static = if is_static {
            is_static
        } else if self.current_token.token_type == TokenType::Static {
            self.advance();
            true
        } else {
            false
        };

        // Expect either 'let' or 'const'
        if is_const {
            self.expect(TokenType::Const)?;
        } else {
            self.expect(TokenType::Let)?;
        }

        // Variable name
        let var_name = if let TokenType::Identifier = &self.current_token.token_type {
            let name_text = self.get_token_text(&self.current_token).to_string();
            self.advance();
            name_text
        } else {
            return Err(ParseError::new(
                "Expected variable name".to_string(),
                self.current_token.start,
            ));
        };

        self.expect(TokenType::Assign)?;
        let expr = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;

        // Create a Local entry for this variable
        let name_ref = self.ast.intern_symbol(var_name);
        let local = Local {
            name: name_ref,
            is_param: false,
            is_static,
            is_const,
            ty: None,
        };

        self.create_local_binding(local, expr, start_token_index)
    }

    fn parse_assignment(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;
        
        // Parse identifier
        let identifier_text = if let TokenType::Identifier = &self.current_token.token_type {
            let name_text = self.get_token_text(&self.current_token).to_string();
            self.advance();
            name_text
        } else {
            return Err(ParseError::new(
                "Expected identifier for assignment".to_string(),
                self.current_token.start,
            ));
        };
        
        // Expect '=' token
        self.expect(TokenType::Assign)?;
        
        // Parse expression
        let expr = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;
        
        // Look up the identifier to get its local index
        let name_ref = self.ast.intern_symbol(identifier_text.clone());
        if let Some(local_ref) = self.binding_map.get(&name_ref).copied() {
            // Create Assign node with final LocalRef
            let assign_node = Node::Assign(local_ref, expr);
            let node_ref = self.ast.add_node(assign_node, NodeInfo::new(start_token_index));
            Ok(node_ref)
        } else {
            Err(ParseError::new(
                format!("Undefined variable in assignment: {}", identifier_text),
                self.current_token.start,
            ))
        }
    }

    // Parse if statement
    fn parse_if_statement(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::If)?;

        let cond = self.parse_expression()?;
        let then_block = self.parse_block()?;

        let else_block = if self.current_token.token_type == TokenType::Else {
            self.advance();
            if self.current_token.token_type == TokenType::If {
                // else if - return the If node directly
                self.parse_if_statement()?
            } else {
                // else block
                self.parse_block()?
            }
        } else {
            // No else clause - create block with unit value
            let unit_node = self
                .ast
                .add_node(Node::ConstUnit, NodeInfo::new(self.token_index));
            let statements_ref = self.ast.add_node_refs(&[unit_node]);
            self.ast.add_node(
                Node::Block(IsStatic::No, 0, statements_ref),
                NodeInfo::new(self.token_index),
            )
        };

        let if_node = Node::If(IsStatic::No, IsInline::No, cond, then_block, else_block);
        Ok(self.ast.add_node(if_node, NodeInfo::new(start_token_index)))
    }

    // Parse while statement
    fn parse_while_statement(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::While)?;

        let cond = self.parse_expression()?;
        let body = self.parse_block()?;

        let while_node = Node::While(IsStatic::No, IsInline::No, cond, body);
        Ok(self
            .ast
            .add_node(while_node, NodeInfo::new(start_token_index)))
    }

    // Parse return statement
    fn parse_return_statement(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::Return)?;

        // Check if this is a bare return (return;) vs return with value
        let value = if self.current_token.token_type == TokenType::Semicolon {
            // Return unit for bare return
            self.ast
                .add_node(Node::ConstUnit, NodeInfo::new(self.token_index))
        } else {
            self.parse_expression()?
        };

        self.expect(TokenType::Semicolon)?;

        let return_node = Node::Return(value);
        Ok(self
            .ast
            .add_node(return_node, NodeInfo::new(start_token_index)))
    }

    // Parse break statement
    fn parse_break_statement(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::Break)?;

        let value = if self.current_token.token_type == TokenType::Semicolon {
            // Break with unit
            self.ast
                .add_node(Node::ConstUnit, NodeInfo::new(self.token_index))
        } else {
            self.parse_expression()?
        };

        self.expect(TokenType::Semicolon)?;

        let break_node = Node::Break(IsStatic::No, 0, value);
        Ok(self
            .ast
            .add_node(break_node, NodeInfo::new(start_token_index)))
    }

    // Parse continue statement
    fn parse_continue_statement(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::Continue)?;

        let value = if self.current_token.token_type == TokenType::Semicolon {
            // Continue with unit
            self.ast
                .add_node(Node::ConstUnit, NodeInfo::new(self.token_index))
        } else {
            self.parse_expression()?
        };

        self.expect(TokenType::Semicolon)?;

        let continue_node = Node::Continue(IsStatic::No, 0, value);
        Ok(self
            .ast
            .add_node(continue_node, NodeInfo::new(start_token_index)))
    }

    // Parse expression using precedence climbing
    fn parse_expression(&mut self) -> ParseResult<NodeRef> {
        self.parse_expression_precedence(0)
    }

    // Get precedence for binary operators
    fn get_precedence(&self, token_type: TokenType) -> Option<i32> {
        match token_type {
            TokenType::Or => Some(1),                           // Logical OR: lowest precedence
            TokenType::And => Some(2),                          // Logical AND
            TokenType::Equal | TokenType::NotEqual | 
            TokenType::Lt | TokenType::Gt | 
            TokenType::LtEq | TokenType::GtEq => Some(3),       // Equality and comparison
            TokenType::Plus | TokenType::Minus => Some(4),      // Addition/subtraction
            TokenType::Star | TokenType::Slash => Some(5),      // Multiplication/division: highest precedence
            // Bitwise operators are not yet supported in parser
            // TokenType::BitOr => Some(?),
            // TokenType::BitAnd => Some(?),
            _ => None,
        }
    }

    // Precedence climbing algorithm
    fn parse_expression_precedence(&mut self, min_prec: i32) -> ParseResult<NodeRef> {
        let mut left = self.parse_atom()?;

        while let Some(prec) = self.get_precedence(self.current_token.token_type) {
            if prec < min_prec {
                break;
            }

            let start_token_index = self.token_index;
            let op = self.current_token.token_type;
            self.advance();

            // For left-associative operators, use prec + 1
            // For right-associative operators, use prec
            let right = self.parse_expression_precedence(prec + 1)?;

            let node = match op {
                TokenType::Plus => Node::BinopAdd(left, right),
                TokenType::Minus => Node::BinopSub(left, right),
                TokenType::Star => Node::BinopMul(left, right),
                TokenType::Slash => Node::BinopDiv(left, right),
                TokenType::Equal => Node::BinopEq(left, right),
                TokenType::NotEqual => Node::BinopNeq(left, right),
                TokenType::Lt => Node::BinopLt(left, right),
                TokenType::Gt => Node::BinopGt(left, right),
                TokenType::LtEq => Node::BinopLtEq(left, right),
                TokenType::GtEq => Node::BinopGtEq(left, right),
                TokenType::And => Node::BinopAnd(left, right),
                TokenType::Or => Node::BinopOr(left, right),
                _ => unreachable!(),
            };

            left = self.ast.add_node(node, NodeInfo::new(start_token_index));
        }

        Ok(left)
    }

    // Parse atomic expressions (unary operators and primary expressions)
    fn parse_atom(&mut self) -> ParseResult<NodeRef> {
        match self.current_token.token_type {
            TokenType::Minus => {
                let start_token_index = self.token_index;
                self.advance();
                let operand = self.parse_atom()?; // Right-associative for multiple negations
                let neg_node = Node::UnopNeg(operand);
                Ok(self
                    .ast
                    .add_node(neg_node, NodeInfo::new(start_token_index)))
            }
            TokenType::Not => {
                let start_token_index = self.token_index;
                self.advance();
                let operand = self.parse_atom()?; // Right-associative for multiple negations
                let not_node = Node::UnopNot(operand);
                Ok(self
                    .ast
                    .add_node(not_node, NodeInfo::new(start_token_index)))
            }
            _ => self.parse_primary()
        }
    }

    // Parse primary expressions (literals, identifiers, parentheses)
    fn parse_primary(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;
        let token = self.current_token;

        match &token.token_type {
            TokenType::IntLiteral => {
                let token_text = self.get_token_text(&token);
                let value = token_text.parse::<i32>().unwrap_or(0);
                self.advance();
                let const_node = Node::ConstI32(value);
                Ok(self
                    .ast
                    .add_node(const_node, NodeInfo::new(start_token_index)))
            }

            TokenType::True => {
                self.advance();
                let const_node = Node::ConstBool(true);
                Ok(self
                    .ast
                    .add_node(const_node, NodeInfo::new(start_token_index)))
            }

            TokenType::False => {
                self.advance();
                let const_node = Node::ConstBool(false);
                Ok(self
                    .ast
                    .add_node(const_node, NodeInfo::new(start_token_index)))
            }

            TokenType::StringLiteral => {
                let token_text = self.get_token_text(&token);
                // Remove surrounding quotes and handle escape sequences
                let value = self.parse_string_literal(token_text);
                self.advance();
                let string_ref = self.ast.add_string(value);
                let const_node = Node::ConstString(string_ref);
                Ok(self
                    .ast
                    .add_node(const_node, NodeInfo::new(start_token_index)))
            }

            TokenType::Identifier => {
                let identifier_text = self.get_token_text(&token).to_string();
                self.advance();

                // Look up identifier in binding map
                let name_ref = self.ast.intern_symbol(identifier_text.clone());

                if let Some(local_ref) = self.binding_map.get(&name_ref).copied() {
                    // Found binding - create LocalRead with final LocalRef
                    let local_read = Node::LocalRead(local_ref);
                    let node_ref = self.ast.add_node(local_read, NodeInfo::new(start_token_index));
                    Ok(node_ref)
                } else {
                    // Identifier not found in current scope
                    return Err(ParseError::new(
                        format!("Undefined variable: {}", identifier_text),
                        token.start,
                    ));
                }
            }

            TokenType::LeftParen => {
                self.advance(); // consume '('

                // Check if this is unit value () or parenthesized expression
                if self.current_token.token_type == TokenType::RightParen {
                    // Unit value ()
                    self.advance(); // consume ')'
                    let unit_node = Node::ConstUnit;
                    Ok(self
                        .ast
                        .add_node(unit_node, NodeInfo::new(start_token_index)))
                } else {
                    // Parenthesized expression
                    let expr = self.parse_expression()?;
                    self.expect(TokenType::RightParen)?;
                    Ok(expr)
                }
            }

            _ => Err(ParseError::new(
                format!("Unexpected token: {:?}", token.token_type),
                token.start,
            )),
        }
    }

    fn parse_string_literal(&self, token_text: &str) -> String {
        // Remove surrounding quotes
        let content = &token_text[1..token_text.len() - 1];
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

    fn get_token_text(&self, token: &Token) -> &str {
        if token.length == 0 {
            ""
        } else {
            let start = token.start as usize;
            let end = start + token.length as usize;
            &self.source[start..end]
        }
    }

    // Flag parsing helpers
    
    /// Parse static flag, advancing if present
    fn parse_static_flag(&mut self) -> IsStatic {
        if self.current_token.token_type == TokenType::Static {
            self.advance();
            IsStatic::Yes
        } else {
            IsStatic::No
        }
    }
    
    /// Parse inline flag, advancing if present  
    fn parse_inline_flag(&mut self) -> IsInline {
        if self.current_token.token_type == TokenType::Inline {
            self.advance();
            IsInline::Yes
        } else {
            IsInline::No
        }
    }

    // Scope management methods

    /// Enter a new block scope and return its unique ID
    fn enter_block(&mut self) -> BlockIndex {
        let block_id = self.next_block_id;
        self.next_block_id += 1;
        self.block_stack.push(block_id);
        block_id
    }

    /// Exit a block by applying all cleanup operations for that block
    fn exit_block(&mut self, block_id: BlockIndex) {
        // Verify we're exiting the most recent block
        if let Some(&top_block) = self.block_stack.last() {
            assert_eq!(top_block, block_id, "Exiting block out of order");
            self.block_stack.pop();
        }

        // Pop and apply cleanup operations until we've handled all for this block
        while let Some(cleanup) = self.cleanup_stack.last() {
            let cleanup_block = match cleanup {
                ScopeCleanup::Delete(_, block) => *block,
                ScopeCleanup::Replace(_, _, block) => *block,
            };

            if cleanup_block != block_id {
                break; // We've finished this block's cleanup operations
            }

            // Remove and apply the cleanup operation
            let cleanup = self.cleanup_stack.pop().unwrap();
            match cleanup {
                ScopeCleanup::Delete(symbol, _) => {
                    self.binding_map.remove(&symbol);
                }
                ScopeCleanup::Replace(symbol, old_local_ref, _) => {
                    self.binding_map.insert(symbol, old_local_ref);
                }
            }
        }
    }

    /// Enter a new function scope and return its unique ID
    fn enter_scope(&mut self) -> ScopeIndex {
        // Create scope in AST immediately with empty locals
        let scope = Scope {
            locals: LocalsRef::empty(),
            parent: None, // TODO: For nested functions later
        };
        let scope_index = self.ast.add_scope(scope);
        
        self.scope_stack.push(scope_index);
        self.current_scope_index = Some(scope_index);
        
        // Track where this scope's locals start in function_locals
        self.scope_locals_start.push(self.function_locals.len());
        
        scope_index
    }

    /// Commit collected locals to the scope and exit it
    fn exit_scope(&mut self, scope_id: ScopeIndex) {
        // Verify we're exiting the most recent scope
        if let Some(&top_scope) = self.scope_stack.last() {
            assert_eq!(top_scope, scope_id, "Exiting scope out of order");
            self.scope_stack.pop();
        }
        
        // Commit locals before popping scope_locals_start
        let locals_start = self.scope_locals_start.last().copied().unwrap_or(0);
        
        // Get locals for this scope
        let scope_locals = &self.function_locals[locals_start..];
        let locals_ref = if scope_locals.is_empty() {
            LocalsRef::empty()
        } else {
            self.ast.add_locals(scope_locals)
        };

        // Update the scope's locals_ref
        self.ast.update_scope_locals(scope_id, locals_ref);
        
        // Truncate function_locals back to where this scope started
        self.function_locals.truncate(locals_start);
        
        // Update current scope and pop scope locals start tracking
        self.current_scope_index = self.scope_stack.last().copied();
        self.scope_locals_start.pop();
    }


    /// Add a local to the function_locals stack and bind it to current scope
    /// Returns the LocalRef for this local
    fn add_and_bind_local(&mut self, local: Local) -> LocalRef {
        // Calculate 0-based index within current scope
        let scope_start = self.scope_locals_start.last().copied().unwrap_or(0);
        let index_in_scope = (self.function_locals.len() - scope_start) as u16;
        let name_ref = local.name;
        self.function_locals.push(local);
        
        // Create LocalRef with current scope
        let current_scope = self.current_scope_index.expect("Should be in a scope when adding local");
        let local_ref = LocalRef::new(current_scope, index_in_scope);

        // Handle shadowing for cleanup - use current block if in one, otherwise no cleanup needed
        // (module/function level variables don't get cleaned up until scope ends)
        if let Some(&current_block) = self.block_stack.last() {
            if let Some(old_local_ref) = self.binding_map.get(&name_ref) {
                // Symbol is being shadowed - save the old binding for restoration
                self.cleanup_stack
                    .push(ScopeCleanup::Replace(name_ref, *old_local_ref, current_block));
            } else {
                // New binding - mark for deletion when block exits
                self.cleanup_stack
                    .push(ScopeCleanup::Delete(name_ref, current_block));
            }
        }
        // If not in a block, variables persist until scope ends (handled by exit_scope)

        // Update current binding
        self.binding_map.insert(name_ref, local_ref);

        local_ref
    }

    /// Create a local binding and return a Define node
    /// Helper method for const/let/static syntax
    fn create_local_binding(&mut self, local: Local, expr: NodeRef, start_token_index: usize) -> ParseResult<NodeRef> {
        let local_ref = self.add_and_bind_local(local);

        // Create Define node with final LocalRef
        let define_node = Node::Define(local_ref, expr);
        let node_ref = self.ast.add_node(define_node, NodeInfo::new(start_token_index));
        
        Ok(node_ref)
    }

    /// Create a function definition and return a DefineFn node
    /// Helper method for fn name() {} syntax
    fn create_function_definition(&mut self, local: Local, func_node: NodeRef, start_token_index: usize) -> ParseResult<NodeRef> {
        let local_ref = self.add_and_bind_local(local);

        // Create DefineFn node with final LocalRef
        let define_fn_node = Node::DefineFn(local_ref, func_node);
        let node_ref = self.ast.add_node(define_fn_node, NodeInfo::new(start_token_index));
        
        Ok(node_ref)
    }

    pub fn parse(input: &str) -> ParseResult<Ast> {
        // Tokenize everything to get the full token stream
        let mut lexer = Lexer::new(input);
        let all_tokens = lexer.tokenize();

        // Parse semantically using the token stream
        let mut parser = Parser::new(all_tokens.clone(), input.to_string());
        parser.parse_root()?;
        let ast = parser.into_ast();

        Ok(ast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::print::PrettyPrinter;

    fn roundtrip(input: &str) {
        // NEVER fix tests by changing this function - fix the implementations that we are testing
        let ast = Parser::parse(input).unwrap();
        let printer = PrettyPrinter::new_generate(&ast, 0);
        let output = printer.print();
        assert_eq!(input, output);
    }

    fn parse_prints_as(input: &str, expected_output: &str) {
        // Test cases where input parses correctly but prints in normalized form
        let ast = Parser::parse(input).unwrap();
        let printer = PrettyPrinter::new_generate(&ast, 0);
        let output = printer.print();
        assert_eq!(expected_output, output);
    }

    #[test]
    fn test_simple_function() {
        roundtrip("fn main() { return 42; }");
    }

    #[test]
    fn test_assignment() {
        // Test variable assignment
        roundtrip("fn main() { let x = 5; x = 10; return x; }");
        
        // Test multiple assignments
        roundtrip("fn main() { let x = 1; let y = 2; x = y; y = x; return x + y; }");
        
        // Test assignment with expressions
        roundtrip("fn main() { let x = 1; let y = 2; x = x + y * 3; return x; }");
        
        // Test assignment to const variables (should be allowed at parse level)
        roundtrip("fn main() { const x = 1; x = 2; return x; }");
    }

    #[test]
    fn test_assignment_errors() {
        // Test assignment to undefined variable
        let result = Parser::parse("fn main() { x = 5; }");
        assert!(result.is_err());
        
        // Test that the error message is helpful
        if let Err(err) = result {
            assert!(err.message.contains("Undefined variable"));
            assert!(err.message.contains("x"));
        }
    }

    #[test]
    fn test_simple_module_const() {
        // Debug test for module-level const
        roundtrip("const PI = 42;");
    }

    #[test] 
    fn test_simple_const_in_function() {
        // Debug test for function-level variable
        roundtrip("fn main() { let x = 5; }");
    }

    #[test]
    fn test_debug_module_const_read() {
        // Test with variable read
        roundtrip("const PI = 42; fn main() { return PI; }");
    }

    #[test]
    fn test_debug_module_const_let() {
        // Test the exact failing case
        roundtrip("const PI = 42; fn main() { let x = PI; }");
    }

    #[test]
    fn test_arithmetic_expressions() {
        // Addition
        roundtrip("fn main() -> i32 { return 1 + 2; }");
        roundtrip("fn main() { return 5 + 10; }");

        // Subtraction
        roundtrip("fn main() -> i32 { return 5 - 3; }");
        roundtrip("fn main() { return 10 - 1; }");

        // Multiplication
        roundtrip("fn main() -> i32 { return 3 * 4; }");
        roundtrip("fn main() { return 2 * 5; }");

        // Division
        roundtrip("fn main() -> i32 { return 8 / 2; }");
        roundtrip("fn main() { return 15 / 3; }");

        // Unary negation
        roundtrip("fn main() -> i32 { return -5; }");
        roundtrip("fn main() { return -42; }");

        // Multiple negations
        roundtrip("fn main() -> i32 { return --5; }");
    }

    #[test]
    fn test_comparison_operators() {
        // Equality operators
        roundtrip("fn main() -> bool { return true == false; }");
        roundtrip("fn main() { let x = 10; return x != 5; }");
        
        // Relational operators
        roundtrip("fn main() -> bool { return 5 < 10; }");
        roundtrip("fn main() -> bool { return 10 > 5; }");
        roundtrip("fn main() -> bool { return 5 <= 10; }");
        roundtrip("fn main() -> bool { return 10 >= 5; }");
        roundtrip("fn main() -> bool { return 5 <= 5; }");
        roundtrip("fn main() -> bool { return 5 >= 5; }");
        
        // Mixed comparisons
        roundtrip("fn main() { let x = 10; let y = 20; return x < y && y > x; }");
    }

    #[test]
    fn test_boolean_operators() {
        // Basic boolean operators
        roundtrip("fn main() -> bool { return true && false; }");
        roundtrip("fn main() -> bool { return true || false; }");
        roundtrip("fn main() -> bool { return !true; }");
        roundtrip("fn main() -> bool { return !false; }");

        // Multiple unary not operators
        roundtrip("fn main() -> bool { return !!true; }");
        roundtrip("fn main() -> bool { return !!!false; }");

        // Mixed boolean and comparison operators
        roundtrip("fn main() -> bool { return true && 1 == 1; }");
        roundtrip("fn main() -> bool { return false || 2 != 3; }");
        roundtrip("fn main() -> bool { return !(1 == 2); }");
    }

    #[test]
    fn test_string_literals() {
        roundtrip(r#"fn main() { return "Hello"; }"#);
        roundtrip(r#"fn main() { return "Hello World"; }"#);
    }

    #[test]
    fn test_string_escaping() {
        // Test various escape sequences
        let escaped_input = r#"fn main() { return "Hello \"World\"\nNew line\tTab\\Backslash"; }"#;
        roundtrip(escaped_input);
    }

    #[test]
    fn test_variable_declarations() {
        roundtrip("fn main() { let x = 42; }");
        roundtrip("fn main() { let y = true; }");

        // Test const declarations
        roundtrip("fn main() { const x = 42; }");
        roundtrip("fn main() { const y = true; }");

        // Test static declarations
        roundtrip("fn main() { static let x = 42; }");
        roundtrip("fn main() { static const y = true; }");

        // Test mixed let, const, and static
        roundtrip(
            "fn main() { let x = 1; const y = 2; static let z = 3; static const w = 4; return x; }",
        );
    }

    #[test]
    fn test_if_statements() {
        // Test if with explicit else
        roundtrip("fn main() { if false { return 1; } else { return 2; } }");

        // Test if without else (implicit unit else clause) - stays implicit
        roundtrip("fn main() { if true { return 1; } }");

        // Test else-if chains
        roundtrip("fn main() { let x = 0; if x == 1 { return 1; } else if x == 2 { return 2; } else { return 0; } }");
    }

    #[test]
    fn test_while_loops() {
        roundtrip("fn main() { while true { break; } }");
        roundtrip("fn main() { let x = 5; while x != 0 { continue; } }");
    }

    #[test]
    fn test_break_continue() {
        roundtrip("fn main() { while true { break; } }");
        roundtrip("fn main() { while true { continue; } }");

        roundtrip(
            "fn main() { let x = 5; while true { if x == 5 { break; } else { continue; } } }",
        );
    }

    #[test]
    fn test_function_parameters() {
        roundtrip("fn main(x: i32) -> i32 { return x; }");

        roundtrip("fn main(a: i32, b: bool) -> i32 { return a; }");

        // Test static parameters
        roundtrip("fn main(static x: i32) -> i32 { return x; }");
        roundtrip("fn main(static a: i32, b: bool, static c: i32) -> i32 { return a; }");
    }

    #[test]
    fn test_nested_blocks() {
        roundtrip("fn main() { { return 42; } }");
    }

    #[test]
    fn test_complex_expressions() {
        // Test nested arithmetic
        roundtrip("fn main() -> i32 { return 1 + 2 + 3; }");

        // Test mixed operators with equality
        roundtrip("fn main() -> bool { return 1 + 2 == 3; }");

        // Test operator precedence (multiplication binds tighter than addition)
        roundtrip("fn main() -> i32 { return 1 + 2 * 3; }"); // Should be 1 + (2 * 3)
        roundtrip("fn main() -> i32 { return 2 * 3 + 1; }"); // Should be (2 * 3) + 1
        roundtrip("fn main() -> i32 { return 10 - 4 / 2; }"); // Should be 10 - (4 / 2)
        roundtrip("fn main() -> i32 { return -2 * 3; }"); // Should be (-2) * 3

        // Test explicit parentheses
        roundtrip("fn main() -> i32 { return (1 + 2) * 3; }");
        roundtrip("fn main() -> i32 { return 10 / (4 - 2); }");

        // Test associativity
        roundtrip("fn main() -> i32 { return 1 - 2 - 3; }"); // Should be (1 - 2) - 3
        roundtrip("fn main() -> i32 { return 8 / 4 / 2; }"); // Should be (8 / 4) / 2
    }

    #[test]
    fn test_boolean_operator_precedence() {
        // Test boolean operator precedence: || < && < == < arithmetic
        // Natural precedence should not require parentheses
        roundtrip("fn main() -> bool { return true || false && true; }");
        roundtrip("fn main() -> bool { return true && false == false; }");
        roundtrip("fn main() -> bool { return true && 1 + 2 == 3; }");
        
        // Test explicit parentheses are preserved when they override precedence
        roundtrip("fn main() -> bool { return (true || false) && true; }");
        // This case has redundant parentheses that get normalized away
        parse_prints_as("fn main() -> bool { return true || (false && true); }", "fn main() -> bool { return true || false && true; }");
        
        // Test unary not has highest precedence - no parentheses needed
        roundtrip("fn main() -> bool { return !true && false; }");
        roundtrip("fn main() -> bool { return !1 == 2; }");
    }

    #[test]
    fn test_comparison_operator_precedence() {
        // Test comparison operator precedence with arithmetic
        roundtrip("fn main() -> bool { return 1 + 2 < 5 - 1; }");
        roundtrip("fn main() -> bool { return 2 * 3 > 4 + 1; }");
        roundtrip("fn main() -> bool { return 10 / 2 <= 3 + 2; }");
        roundtrip("fn main() -> bool { return 4 - 1 >= 2 * 1; }");
        
        // Test comparison with boolean operators
        roundtrip("fn main() -> bool { return 1 < 2 && 3 > 2; }");
        roundtrip("fn main() -> bool { return 5 == 5 || 10 != 5; }");
    }

    #[test]
    fn test_boolean_literals() {
        roundtrip("fn main() -> bool { return true; }");
        roundtrip("fn main() -> bool { return false; }");
    }

    #[test]
    fn test_function_return_types() {
        // Test void (no arrow) - use a value since bare return requires parsing the void atom
        roundtrip("fn main() { return 42; }");

        // Test explicit return types
        roundtrip("fn main() -> i32 { return 42; }");
        roundtrip("fn main() -> bool { return true; }");
    }

    #[test]
    fn test_parse_error_handling() {
        let malformed_inputs = [
            "fn main(",           // Missing closing paren
            "fn main() {",        // Missing closing brace
            "fn main() { return", // Missing semicolon and value
            "fn main() { 1 +",    // Incomplete expression
        ];

        for input in &malformed_inputs {
            let result = Parser::parse(input);
            assert!(result.is_err(), "Expected parse error for input: {}", input);
        }
    }

    #[test]
    fn test_scope_management() {
        // Test basic variable declaration and usage
        roundtrip("fn main() { let x = 42; return x; }");

        // Test parameter usage
        roundtrip("fn main(x: i32) -> i32 { return x; }");

        // Test variable in nested blocks
        roundtrip("fn main() { let x = 1; { let y = 2; return y; } }");
    }

    #[test]
    fn test_multiple_statements_in_blocks() {
        // Test multiple statements in function body
        roundtrip("fn main() { let x = 1; let y = 2; return x; }");

        // Test multiple statements in nested blocks
        roundtrip("fn main() { let x = 1; { let y = 2; let z = 3; return z; } }");

        // Test multiple statements in control structures
        roundtrip("fn main() { let x = 1; if x == 1 { let y = 2; return y; } else { let z = 3; return z; } }");
    }

    #[test]
    fn test_unit_type_and_expression_blocks() {
        // Test unit type in function signatures
        roundtrip("fn main() { let x = 1; }");

        // Test block expressions (final expression without semicolon)
        roundtrip("fn main() -> i32 { 42 }");
        roundtrip("fn main() -> bool { let x = 1; x == 1 }");

        // Test nested block expressions
        roundtrip("fn main() -> i32 { { let x = 1; x + 1 } }");

        // Test if expressions
        roundtrip("fn main() -> i32 { if true { 1 } else { 2 } }");
    }

    #[test]
    fn test_unit_syntax() {
        // Test implicit unit type (no arrow)
        roundtrip("fn main() { let x = 1; }");

        // Test empty block stays empty
        roundtrip("fn main() { }");

        // Test explicit unit values are normalized to empty blocks
        parse_prints_as("fn main() { () }", "fn main() { }");

        // Test return with unit value
        roundtrip("fn main() { return (); }");

        // Test that unit values work in expressions
        roundtrip("fn main() -> i32 { let x = (); 42 }");

        // Test that final unit is always suppressed
        parse_prints_as("fn main() { let x = 1; () }", "fn main() { let x = 1; }");
        roundtrip("fn main() { let x = 1; }"); // Already normalized
    }

    #[test]
    fn test_static_declarations_with_formatting() {
        // Test that static parsing works even with comments between tokens
        roundtrip("fn main() { static let x = 42; }");
        roundtrip("fn main() { static const y = true; }");
    }

    #[test]
    fn test_module_single_function() {
        // Test that a single function is parsed as a module containing that function
        roundtrip("fn main() { let x = 42; }");
    }

    #[test]
    fn test_module_multiple_functions() {
        // Test module with multiple function definitions
        roundtrip("fn main() { let x = 42; } fn helper() -> bool { true }");
    }

    #[test]
    fn test_module_const_only() {
        // Test module with just a constant
        roundtrip("const PI = 42;");
    }

    #[test]
    fn debug_two_module_consts() {
        let input = "const PI = 42; const E = 27;";
        let ast = Parser::parse(input).unwrap();
        let printer = PrettyPrinter::new_generate(&ast, 0);
        let output = printer.print();
        assert_eq!(input, output);
    }

    #[test]
    fn debug_module_const_and_simple_function() {
        let input = "const PI = 42; fn main() { let x = 1; }";
        let ast = Parser::parse(input).unwrap();
        let printer = PrettyPrinter::new_generate(&ast, 0);
        let output = printer.print();
        assert_eq!(input, output);
    }

    #[test]
    fn test_module_const_and_function() {
        // Test module with constant and function definitions
        roundtrip("const PI = 42; fn main() { let x = PI; }");
    }

    #[test]
    fn test_module_static_const_and_function() {
        // Test module with static constant and function
        roundtrip("const MAX_SIZE = 100; fn calculate() -> i32 { MAX_SIZE }");
    }
}

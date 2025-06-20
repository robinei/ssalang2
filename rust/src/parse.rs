use crate::ast::*;
use crate::lexer::{Lexer, Token, TokenType};
use smallvec::SmallVec;
use std::collections::HashMap;

// Cleanup operations for scope management
#[derive(Debug, Clone)]
enum ScopeCleanup {
    Delete(SymbolRef, ScopeIndex), // Remove binding when exiting scope
    Replace(SymbolRef, LocalIndex, ScopeIndex), // Restore shadowed binding when exiting scope
}

pub struct Parser {
    tokens: Vec<Token>,
    token_index: usize,
    current_token: Token,
    source: String,
    ast: Ast,

    // Scope management for local bindings
    binding_map: HashMap<SymbolRef, LocalIndex>, // Current symbol -> local index mappings
    cleanup_stack: Vec<ScopeCleanup>,            // Stack of cleanup operations
    next_scope_id: ScopeIndex,                   // Counter for unique scope IDs
    scope_stack: Vec<ScopeIndex>,                // Stack of currently active scope IDs
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
            current_token: Token::new(TokenType::Eof, 0, 0),
            source,
            ast: Ast::new(),
            binding_map: HashMap::new(),
            cleanup_stack: Vec::new(),
            next_scope_id: 1, // Start scope IDs at 1 (0 can be reserved for global/invalid)
            scope_stack: Vec::new(),
        };

        parser.update_current_token();

        parser
    }

    fn update_current_token(&mut self) {
        // Skip formatting tokens to find next semantic token
        while self.token_index < self.tokens.len() {
            self.current_token = self.tokens[self.token_index];
            if !matches!(
                self.current_token.token_type,
                TokenType::Comment | TokenType::Newline
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

    fn peek(&self, n: usize) -> TokenType {
        // Look ahead n semantic tokens from current position
        let mut peek_index = self.token_index;
        let mut semantic_count = 0;

        while peek_index < self.tokens.len() {
            let token = &self.tokens[peek_index];
            if !matches!(token.token_type, TokenType::Comment | TokenType::Newline) {
                if semantic_count == n {
                    return token.token_type;
                }
                semantic_count += 1;
            }
            peek_index += 1;
        }

        // If we can't find the nth token, return EOF
        TokenType::Eof
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

    // Parse the entire program
    pub fn parse_root(&mut self) -> ParseResult<NodeRef> {
        let root = self.parse_function()?;
        self.ast.set_root(root);
        Ok(root)
    }

    // Parse a function definition
    fn parse_function(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

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
            return Err(ParseError::new(
                "Expected function name".to_string(),
                self.current_token.start,
            ));
        }

        // Enter function scope for parameters and body
        let function_scope_id = self.enter_scope();

        self.expect(TokenType::LeftParen)?;

        // Parse parameters using local SmallVec
        let mut parameters: SmallVec<[Local; 8]> = SmallVec::new();
        if self.current_token.token_type != TokenType::RightParen {
            loop {
                let param = self.parse_parameter()?;
                parameters.push(param);

                if self.current_token.token_type == TokenType::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.expect(TokenType::RightParen)?;

        // Add parameters to the AST and bind them to scope
        let locals_ref = if parameters.is_empty() {
            LocalsRef::empty()
        } else {
            let locals_ref = self.ast.add_locals(&parameters);
            // Now bind parameters to scope using their actual indices
            for (i, param) in parameters.iter().enumerate() {
                let local_index = (locals_ref.offset + i as u16) as LocalIndex;
                self.bind_local(param.name, local_index, function_scope_id);
            }
            locals_ref
        };

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

        // Parse function body (will create its own nested scope)
        let body = self.parse_block_with_scope(function_scope_id)?;

        // Exit function scope
        self.exit_scope(function_scope_id);

        // Create function node with scope information
        let func_node = Node::Func(flags, locals_ref, body, return_type);
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
            ty: param_type,
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

    // Parse a block statement (creates its own scope)
    fn parse_block(&mut self) -> ParseResult<NodeRef> {
        let block_scope_id = self.enter_scope();
        let result = self.parse_block_with_scope(block_scope_id);
        self.exit_scope(block_scope_id);
        result
    }

    // Parse a block statement with a specific scope ID
    fn parse_block_with_scope(&mut self, scope_id: ScopeIndex) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::LeftBrace)?;

        let flags = Flags::new();

        // Parse all statements in the block
        let mut statements: SmallVec<[NodeRef; 64]> = SmallVec::new();
        let mut last_had_semicolon = false;

        while self.current_token.token_type != TokenType::RightBrace {
            let stmt = self.parse_statement()?;
            statements.push(stmt);

            // Check if this statement ends with a semicolon
            last_had_semicolon = if self.current_token.token_type == TokenType::Semicolon {
                self.advance();
                true
            } else {
                false
            };
        }

        // If empty block OR last statement had semicolon, add ConstUnit
        if statements.is_empty() || last_had_semicolon {
            let unit_node = self
                .ast
                .add_node(Node::ConstUnit, NodeInfo::new(self.token_index));
            statements.push(unit_node);
        }

        self.expect(TokenType::RightBrace)?;

        // Add statements to AST and get reference
        let statements_ref = self.ast.add_statements(&statements);

        let block_node = Node::Block(flags, scope_id, statements_ref);
        Ok(self
            .ast
            .add_node(block_node, NodeInfo::new(start_token_index)))
    }

    // Parse a statement
    fn parse_statement(&mut self) -> ParseResult<NodeRef> {
        match &self.current_token.token_type {
            // Statement-only forms
            TokenType::Let => {
                self.parse_variable_declaration(false) // is_const = false
            }
            TokenType::Const => {
                self.parse_variable_declaration(true) // is_const = true
            }
            TokenType::Return => self.parse_return_statement(),
            TokenType::Break => self.parse_break_statement(),
            TokenType::Continue => self.parse_continue_statement(),
            TokenType::Static => {
                // Look ahead to determine if this is "static let" or "static const"
                match self.peek(1) {
                    TokenType::Let => {
                        self.parse_variable_declaration(false) // is_const = false
                    }
                    TokenType::Const => {
                        self.parse_variable_declaration(true) // is_const = true
                    }
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
            // Expression forms
            TokenType::If => self.parse_if_statement(),
            TokenType::LeftBrace => self.parse_block(),
            _ => self.parse_expression(),
        }
    }

    // Parse variable declaration (let or const)
    fn parse_variable_declaration(&mut self, is_const: bool) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        // Check for static modifier first
        let is_static = if self.current_token.token_type == TokenType::Static {
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

        // Create a Local entry for this variable
        let name_ref = self.ast.intern_symbol(var_name);
        // For now, assume i32 type - in a real implementation this would be inferred
        let var_type = self.ast.add_node(
            Node::TypeAtom(TypeAtom::I32),
            NodeInfo::new(self.token_index),
        );
        let local = Local {
            name: name_ref,
            is_param: false,
            is_static,
            is_const,
            ty: var_type,
        };

        // Add to AST locals and bind in current scope
        let local_index = self.ast.add_local(local);

        // Get current scope for binding
        let current_scope = *self
            .scope_stack
            .last()
            .expect("Should be in a scope when parsing variable declaration");
        self.bind_local(name_ref, local_index, current_scope);

        // Create a LocalWrite node (is_definition=true, with proper local_index)
        let local_write = Node::LocalWrite(true, local_index, expr);
        Ok(self
            .ast
            .add_node(local_write, NodeInfo::new(start_token_index)))
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
            let flags = Flags::new();
            let unit_node = self
                .ast
                .add_node(Node::ConstUnit, NodeInfo::new(self.token_index));
            let statements_ref = self.ast.add_statements(&[unit_node]);
            self.ast.add_node(
                Node::Block(flags, 0, statements_ref),
                NodeInfo::new(self.token_index),
            )
        };

        let flags = Flags::new();
        let if_node = Node::If(flags, cond, then_block, else_block);
        Ok(self.ast.add_node(if_node, NodeInfo::new(start_token_index)))
    }

    // Parse while statement
    fn parse_while_statement(&mut self) -> ParseResult<NodeRef> {
        let start_token_index = self.token_index;

        self.expect(TokenType::While)?;

        let cond = self.parse_expression()?;
        let body = self.parse_block()?;

        let flags = Flags::new();
        let while_node = Node::While(flags, cond, body);
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

        let flags = Flags::new();
        let break_node = Node::Break(flags, 0, value);
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

        let flags = Flags::new();
        let continue_node = Node::Continue(flags, 0, value);
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
            TokenType::Equal | TokenType::NotEqual => Some(1), // Equality: lowest precedence
            TokenType::Plus | TokenType::Minus => Some(2),     // Addition/subtraction
            TokenType::Star | TokenType::Slash => Some(3), // Multiplication/division: highest precedence
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
                _ => unreachable!(),
            };

            left = self.ast.add_node(node, NodeInfo::new(start_token_index));
        }

        Ok(left)
    }

    // Parse atomic expressions (unary operators and primary expressions)
    fn parse_atom(&mut self) -> ParseResult<NodeRef> {
        if self.current_token.token_type == TokenType::Minus {
            let start_token_index = self.token_index;
            self.advance();
            let operand = self.parse_atom()?; // Right-associative for multiple negations
            let neg_node = Node::UnopNeg(operand);
            Ok(self
                .ast
                .add_node(neg_node, NodeInfo::new(start_token_index)))
        } else {
            self.parse_primary()
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

                if let Some(local_index) = self.lookup_local(name_ref) {
                    // Found binding - create LocalRead
                    let local_read = Node::LocalRead(local_index);
                    Ok(self
                        .ast
                        .add_node(local_read, NodeInfo::new(start_token_index)))
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

    // Scope management methods

    /// Enter a new scope and return its unique ID
    fn enter_scope(&mut self) -> ScopeIndex {
        let scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        self.scope_stack.push(scope_id);
        scope_id
    }

    /// Exit a scope by applying all cleanup operations for that scope
    fn exit_scope(&mut self, scope_id: ScopeIndex) {
        // Verify we're exiting the most recent scope
        if let Some(&top_scope) = self.scope_stack.last() {
            assert_eq!(top_scope, scope_id, "Exiting scope out of order");
            self.scope_stack.pop();
        }

        // Pop and apply cleanup operations until we've handled all for this scope
        while let Some(cleanup) = self.cleanup_stack.last() {
            let cleanup_scope = match cleanup {
                ScopeCleanup::Delete(_, scope) => *scope,
                ScopeCleanup::Replace(_, _, scope) => *scope,
            };

            if cleanup_scope != scope_id {
                break; // We've finished this scope's cleanup operations
            }

            // Remove and apply the cleanup operation
            let cleanup = self.cleanup_stack.pop().unwrap();
            match cleanup {
                ScopeCleanup::Delete(symbol, _) => {
                    self.binding_map.remove(&symbol);
                }
                ScopeCleanup::Replace(symbol, old_local_index, _) => {
                    self.binding_map.insert(symbol, old_local_index);
                }
            }
        }
    }

    /// Bind a symbol to a local index, handling shadowing
    fn bind_local(&mut self, symbol: SymbolRef, local_index: LocalIndex, scope_id: ScopeIndex) {
        if let Some(old_local_index) = self.binding_map.get(&symbol) {
            // Symbol is being shadowed - save the old binding for restoration
            self.cleanup_stack
                .push(ScopeCleanup::Replace(symbol, *old_local_index, scope_id));
        } else {
            // New binding - mark for deletion when scope exits
            self.cleanup_stack
                .push(ScopeCleanup::Delete(symbol, scope_id));
        }

        // Update current binding
        self.binding_map.insert(symbol, local_index);
    }

    /// Look up the current binding for a symbol
    fn lookup_local(&self, symbol: SymbolRef) -> Option<LocalIndex> {
        self.binding_map.get(&symbol).copied()
    }

    pub fn parse(input: &str) -> ParseResult<Ast> {
        // Tokenize everything to get the full token stream
        let mut lexer = Lexer::new(input);
        let all_tokens = lexer.tokenize();

        // Parse semantically using the token stream
        let mut parser = Parser::new(all_tokens.clone(), input.to_string());
        parser.parse_root()?;
        let mut ast = parser.into_ast();

        // Store the full token stream and source in the AST
        ast.set_tokens(all_tokens);
        ast.set_source(input.to_string());

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
        let printer = PrettyPrinter::new(&ast);
        let output = printer.print();
        assert_eq!(input, output);
    }

    fn parse_prints_as(input: &str, expected_output: &str) {
        // Test cases where input parses correctly but prints in normalized form
        let ast = Parser::parse(input).unwrap();
        let printer = PrettyPrinter::new(&ast);
        let output = printer.print();
        assert_eq!(expected_output, output);
    }

    #[test]
    fn test_simple_function() {
        roundtrip("fn main() { return 42; }");
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
        roundtrip("fn main() -> bool { return true == false; }");
        roundtrip("fn main() { let x = 10; return x != 5; }");
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
}

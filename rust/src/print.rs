use crate::{ast::{Ast, Node, NodeRef, TypeAtom, FuncIndex, IsStatic, IsInline}, lexer::{Token, TokenType}};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PrintMode {
    Reformat,    // Preserve original formatting tokens
    Generate,    // Clean generation, no formatting tokens
}

pub struct PrettyPrinter<'a> {
    ast: &'a Ast,
    mode: PrintMode,
    tokens: Option<&'a [Token]>,
    source: Option<&'a str>,
    token_index: usize,
    indent_size: usize,
    indent: usize,
    buffer: String,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new_reformat(ast: &'a Ast, tokens: &'a [Token], source: &'a str, indent_size: usize) -> Self {
        Self {
            ast,
            mode: PrintMode::Reformat,
            tokens: Some(tokens),
            source: Some(source),
            token_index: 0,
            indent_size,
            indent: 0,
            buffer: String::with_capacity(1024),
        }
    }

    pub fn new_generate(ast: &'a Ast, indent_size: usize) -> Self {
        Self {
            ast,
            mode: PrintMode::Generate,
            tokens: None,
            source: None,
            token_index: 0,
            indent_size,
            indent: 0,
            buffer: String::with_capacity(1024),
        }
    }

    pub fn print(mut self) -> String {
        self.indent = 0;
        self.buffer.clear();
        self.token_index = 0;
        if let Some(root) = self.ast.get_root() {
            self.print_node(root);
        }
        // Catch any trailing formatting tokens
        self.emit_token(TokenType::Eof, "");
        self.buffer
    }

    fn is_multiline(&self) -> bool {
        self.indent_size > 0
    }

    fn write_indent(&mut self) {
        if self.is_multiline() {
            self.buffer.push_str(&" ".repeat(self.indent * self.indent_size));
        }
    }

    fn write_newline(&mut self) {
        if self.is_multiline() {
            self.buffer.push('\n');
        } else {
            self.buffer.push(' ');
        }
    }

    fn emit_token(&mut self, token_type: TokenType, text: &str) {
        if self.mode == PrintMode::Reformat {
            // Process formatting tokens and sync with the token stream
            while let Some(token) = self.tokens.map(|t| t.get(self.token_index)).flatten() {
                match token.token_type {
                    TokenType::Comment => {
                        if self.is_inline_comment(token) {
                            // Inline comment - break and handle after emitting semantic token
                            break;
                        } else {
                            // Standalone comment - emit it on its own line
                            if !(self.buffer.is_empty() || self.buffer.ends_with('\n')) {
                                self.buffer.push('\n');
                            }
                            self.write_indent();
                            self.buffer.push_str(&self.get_token_text(token).unwrap_or_default());
                            self.buffer.push('\n');
                        }
                        self.token_index += 1;
                    }
                    TokenType::EmptyLine => {
                        // Count consecutive empty lines and limit them
                        let mut consecutive_empty_lines = 0;
                        let start_index = self.token_index;
                        
                        // Count all consecutive EmptyLine tokens
                        while let Some(empty_token) = self.tokens.map(|t| t.get(start_index + consecutive_empty_lines)).flatten() {
                            if empty_token.token_type == TokenType::EmptyLine {
                                consecutive_empty_lines += 1;
                            } else {
                                break;
                            }
                        }
                        
                        // Limit to max 2 consecutive blank lines
                        let lines_to_emit = std::cmp::min(consecutive_empty_lines, 2);
                        for _ in 0..lines_to_emit {
                            self.buffer.push('\n');
                        }
                        
                        // Advance past all the EmptyLine tokens we processed
                        self.token_index += consecutive_empty_lines;
                    }
                    t if t == token_type => {
                        // Found matching semantic token - we're in sync!
                        self.token_index += 1;
                        break;
                    }
                    TokenType::LeftParen | TokenType::RightParen => {
                        // Skip parentheses as they might be dropped
                        self.token_index += 1;
                    }
                    _ => {
                        // Unexpected semantic token
                        eprintln!("Warning: Expected {:?} but found {:?} at position {}", 
                                 token_type, token.token_type, self.token_index);
                        break; // Emit synthetic token anyway
                    }
                }
            }
            
            // Apply indentation if we're at the start of a line
            if self.buffer.is_empty() || self.buffer.ends_with('\n') {
                self.write_indent();
            }
        }
        
        // Emit the token (unless it's EOF which is just for catching trailing formatting)
        if token_type != TokenType::Eof {
            self.buffer.push_str(text);
        }
        
        if self.mode == PrintMode::Reformat {
            // Process any inline comments that immediately follow this semantic token
            if let Some(token) = self.tokens.map(|t| t.get(self.token_index)).flatten() {
                if token.token_type == TokenType::Comment && self.is_inline_comment(token) {
                    self.buffer.push(' ');
                    self.buffer.push_str(&self.get_token_text(token).unwrap_or_default());
                    self.token_index += 1;
                }
            }
        }
        
    }

    
    
    

    fn is_inline_comment(&self, comment_token: &Token) -> bool {
        if let Some(source) = self.source {
            let comment_start = comment_token.start as usize;
            
            // Look backwards from comment start to see if there's non-whitespace on the same line
            let mut pos = comment_start;
            while pos > 0 {
                pos -= 1;
                match source.chars().nth(pos) {
                    Some('\n') => {
                        // Reached beginning of line without finding non-whitespace
                        return false;
                    }
                    Some(ch) if !ch.is_whitespace() => {
                        // Found non-whitespace on same line - this is inline
                        return true;
                    }
                    _ => {
                        // Continue looking backwards
                        continue;
                    }
                }
            }
        }
            false
    }
    
    
    fn get_token_text(&self, token: &Token) -> Option<String> {
        if let Some(source) = self.source {
            let start = token.start as usize;
            let end = start + token.length as usize;
            if end <= source.len() {
                Some(source[start..end].to_string())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn print_node(&mut self, node_ref: NodeRef) {
        let node = self.ast.get_node(node_ref);
        match node {
            Node::TypeAtom(atom) => self.write_type_atom(*atom),
            Node::ConstUnit => {
                self.emit_token(TokenType::LeftParen, "(");
                self.emit_token(TokenType::RightParen, ")");
            },
            Node::ConstBool(value) => {
                if *value {
                    self.emit_token(TokenType::True, "true");
                } else {
                    self.emit_token(TokenType::False, "false");
                }
            },
            Node::ConstI32(value) => {
                self.emit_token(TokenType::IntLiteral, &value.to_string());
            },
            Node::ConstString(string_ref) => {
                self.emit_token(TokenType::StringLiteral, "\"");
                self.write_escaped_string(self.ast.get_string(*string_ref));
                self.buffer.push('"');
            }
            // Operators are handled by print_expression
            Node::UnopNeg(_) | Node::UnopNot(_) | Node::BinopAdd(_, _) | Node::BinopSub(_, _) | 
            Node::BinopMul(_, _) | Node::BinopDiv(_, _) | Node::BinopEq(_, _) | 
            Node::BinopNeq(_, _) | Node::BinopLt(_, _) | Node::BinopGt(_, _) |
            Node::BinopLtEq(_, _) | Node::BinopGtEq(_, _) | Node::BinopAnd(_, _) | Node::BinopOr(_, _) => {
                self.print_expression(node_ref);
            }
            Node::DefineFn(local_index, func_node) => {
                if let Node::Func(func_index, body, return_type) = self.ast.get_node(*func_node) {
                    let func = self.ast.get_func(*func_index);
                    
                    if func.is_static {
                        self.emit_token(TokenType::Static, "static ");
                    }
                    if func.is_inline {
                        self.emit_token(TokenType::Static, "inline ");
                    }
                    
                    self.emit_token(TokenType::Fn, "fn ");
                    self.emit_token(TokenType::Identifier, self.ast.get_local_name(*local_index));
                    
                    self.print_function_signature_and_body(*func_index, *body, *return_type);
                }
            }
            Node::Define(local_index, expr) => {
                let var_name = self.ast.get_local_name(*local_index);
                let local = self.ast.get_local(*local_index);
                
                if local.is_static {
                    self.emit_token(TokenType::Static, "static ");
                }
                if local.is_const {
                    self.emit_token(TokenType::Const, "const ");
                } else {
                    self.emit_token(TokenType::Let, "let ");
                }
                self.emit_token(TokenType::Identifier, var_name);
                self.emit_token(TokenType::Assign, " = ");
                self.print_expression(*expr);
                self.emit_token(TokenType::Semicolon, ";");
            }
            Node::Assign(local_index, expr) => {
                let var_name = self.ast.get_local_name(*local_index);
                self.emit_token(TokenType::Identifier, var_name);
                self.emit_token(TokenType::Assign, " = ");
                self.print_expression(*expr);
                self.emit_token(TokenType::Semicolon, ";");
            }
            Node::LocalRead(local_index) => {
                let var_name = self.ast.get_local_name(*local_index);
                self.emit_token(TokenType::Identifier, var_name);
            }
            Node::Block(is_static, block_index, nodes_ref) => {
                if *is_static == IsStatic::Yes {
                    self.emit_token(TokenType::Static, "static ");
                }

                let block = self.ast.get_block(*block_index);
                let nodes = self.ast.get_node_refs(*nodes_ref);

                if nodes.is_empty() || nodes.len() == 1 && self.is_unit_value(nodes[0]) {
                    self.emit_token(TokenType::LeftBrace, "{ ");
                    // Print optional label for empty blocks
                    if let Some(name_ref) = block.name {
                        let name = self.ast.get_symbol(name_ref);
                        self.emit_token(TokenType::Identifier, name);
                        self.emit_token(TokenType::Colon, ": ");
                    }
                    self.emit_token(TokenType::RightBrace, "}");
                } else {
                    self.emit_token(TokenType::LeftBrace, "{");
                    // Print optional label at start of block
                    if let Some(name_ref) = block.name {
                        self.buffer.push(' ');
                        let name = self.ast.get_symbol(name_ref);
                        self.emit_token(TokenType::Identifier, name);
                        self.emit_token(TokenType::Colon, ":");
                    }
                    self.write_newline();
                    self.indent += 1;
                    for (i, &node) in nodes.iter().enumerate() {
                        if i == nodes.len() - 1 {
                            if !self.is_unit_value(node) {
                                self.print_node(node);
                                self.write_newline();
                            }
                        } else {
                            self.print_node(node);
                            self.write_newline();
                        }
                    }
                    self.indent -= 1;
                    self.emit_token(TokenType::RightBrace, "}");
                }
            }
            Node::If(is_inline, cond, then_branch, else_branch) => {
                if *is_inline == IsInline::Yes {
                    self.emit_token(TokenType::Inline, "inline ");
                }

                self.emit_token(TokenType::If, "if ");
                self.print_expression(*cond);
                self.buffer.push(' ');

                // Handle then branch (always a Block)
                self.print_node(*then_branch);

                // Handle else branch (Block or If node, but might be empty)
                if self.is_block_node(*else_branch) {
                    if !self.is_empty_or_unit_only_block(*else_branch) {
                        self.emit_token(TokenType::Else, " else ");
                        self.print_node(*else_branch);
                    }
                } else if self.is_if_node(*else_branch) {
                    self.emit_token(TokenType::Else, " else ");
                    self.print_node(*else_branch);
                }
            }
            Node::While(is_inline, cond, body) => {
                if *is_inline == IsInline::Yes {
                    self.emit_token(TokenType::Inline, "inline ");
                }

                self.emit_token(TokenType::While, "while ");
                self.print_expression(*cond);
                self.buffer.push(' ');

                // Handle body (always a Block)
                self.print_node(*body);
            }
            Node::Break(block_index, value) => {
                self.emit_token(TokenType::Break, "break");
                
                // Check if the block has a label
                let block = self.ast.get_block(*block_index);
                if let Some(label_ref) = block.name {
                    let label = self.ast.get_symbol(label_ref);
                    self.emit_token(TokenType::Identifier, &format!(" {}", label));
                }
                
                if !self.is_unit_value(*value) {
                    self.buffer.push(' ');
                    self.print_expression(*value);
                }
                self.emit_token(TokenType::Semicolon, ";");
            }
            Node::Continue(block_index, value) => {
                self.emit_token(TokenType::Continue, "continue");
                
                // Check if the block has a label
                let block = self.ast.get_block(*block_index);
                if let Some(label_ref) = block.name {
                    let label = self.ast.get_symbol(label_ref);
                    self.emit_token(TokenType::Identifier, &format!(" {}", label));
                }
                
                if !self.is_unit_value(*value) {
                    self.buffer.push(' ');
                    self.print_expression(*value);
                }
                self.emit_token(TokenType::Semicolon, ";");
            }
            Node::Return(value) => {
                self.emit_token(TokenType::Return, "return ");
                self.print_expression(*value);
                self.emit_token(TokenType::Semicolon, ";");
            }
            Node::Func(func_index, body, return_type) => {
                let func = self.ast.get_func(*func_index);
                if func.is_static {
                    self.emit_token(TokenType::Static, "static ");
                }
                if func.is_inline {
                    self.emit_token(TokenType::Inline, "inline ");
                }

                self.emit_token(TokenType::Fn, "fn");
                
                // Delegate to shared function printing logic
                self.print_function_signature_and_body(*func_index, *body, *return_type);
            }
            Node::Module(_locals_ref, nodes_ref) => {
                let nodes = self.ast.get_node_refs(*nodes_ref);
                for (i, &node) in nodes.iter().enumerate() {
                    if i > 0 {
                        self.write_newline();
                    }
                    self.print_node(node);
                }
            }
        }
    }

    fn print_expression(&mut self, node_ref: NodeRef) {
        self.print_expression_with_precedence(node_ref, 0)
    }

    fn print_expression_with_precedence(&mut self, node_ref: NodeRef, parent_precedence: i32) {
        let node = self.ast.get_node(node_ref);
        let current_precedence = self.get_node_precedence(node);

        // Add parentheses if current precedence is lower than parent precedence
        let needs_parens = current_precedence > 0 && current_precedence < parent_precedence;
        if needs_parens {
            self.emit_token(TokenType::LeftParen, "(");
        }

        match node {
            Node::BinopAdd(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Plus, " + ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopSub(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Minus, " - ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopMul(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Star, " * ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopDiv(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Slash, " / ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopEq(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Equal, " == ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopNeq(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::NotEqual, " != ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopLt(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Lt, " < ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopGt(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Gt, " > ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopLtEq(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::LtEq, " <= ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopGtEq(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::GtEq, " >= ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::UnopNeg(operand) => {
                self.emit_token(TokenType::Minus, "-");
                self.print_expression_with_precedence(*operand, current_precedence);
            }
            Node::UnopNot(operand) => {
                self.emit_token(TokenType::Not, "!");
                self.print_expression_with_precedence(*operand, current_precedence);
            }
            Node::BinopAnd(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::And, " && ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            Node::BinopOr(left, right) => {
                self.print_expression_with_precedence(*left, current_precedence);
                self.emit_token(TokenType::Or, " || ");
                self.print_expression_with_precedence(*right, current_precedence + 1);
            }
            _ => {
                // For non-expression nodes, just print normally
                self.print_node(node_ref);
            }
        }

        if needs_parens {
            self.emit_token(TokenType::RightParen, ")");
        }
    }

    fn get_node_precedence(&self, node: &Node) -> i32 {
        match node {
            Node::BinopOr(_, _) => 1,                         // Logical OR: lowest precedence
            Node::BinopAnd(_, _) => 2,                        // Logical AND
            Node::BinopEq(_, _) | Node::BinopNeq(_, _) |
            Node::BinopLt(_, _) | Node::BinopGt(_, _) |
            Node::BinopLtEq(_, _) | Node::BinopGtEq(_, _) => 3, // Equality and comparison
            Node::BinopAdd(_, _) | Node::BinopSub(_, _) => 4, // Addition/subtraction
            Node::BinopMul(_, _) | Node::BinopDiv(_, _) => 5, // Multiplication/division
            Node::UnopNeg(_) | Node::UnopNot(_) => 6,         // Unary operators: highest precedence
            _ => 0,                                           // Non-operators don't have precedence
        }
    }

    fn write_type_atom(&mut self, atom: TypeAtom) {
        match atom {
            TypeAtom::Unit => {
                self.emit_token(TokenType::Unit, "unit");
            },
            TypeAtom::Bool => {
                self.emit_token(TokenType::Bool, "bool");
            },
            TypeAtom::I32 => {
                self.emit_token(TokenType::I32, "i32");
            },
        }
    }

    fn write_escaped_string(&mut self, s: &str) {
        for ch in s.chars() {
            match ch {
                '\\' => self.buffer.push_str("\\\\"),
                '"' => self.buffer.push_str("\\\""),
                '\n' => self.buffer.push_str("\\n"),
                '\r' => self.buffer.push_str("\\r"),
                '\t' => self.buffer.push_str("\\t"),
                '\0' => self.buffer.push_str("\\0"),
                c if c.is_control() => {
                    // For other control characters, use \x format for single bytes
                    let code = c as u32;
                    if code <= 0xFF {
                        self.buffer.push_str(&format!("\\x{:02x}", code));
                    } else {
                        self.buffer.push_str(&format!("\\u{{{:x}}}", code));
                    }
                }
                c => self.buffer.push(c),
            }
        }
    }

    fn is_block_node(&self, node_ref: NodeRef) -> bool {
        matches!(self.ast.get_node(node_ref), Node::Block(_, _, _))
    }

    fn is_if_node(&self, node_ref: NodeRef) -> bool {
        matches!(self.ast.get_node(node_ref), Node::If(_, _, _, _))
    }

    fn is_empty_or_unit_only_block(&self, node_ref: NodeRef) -> bool {
        // Check if this is a Block with no statements or only unit value
        if let Node::Block(_, _, nodes_ref) = self.ast.get_node(node_ref) {
            if nodes_ref.count == 0 {
                return true;
            }
            if nodes_ref.count == 1 {
                let nodes = self.ast.get_node_refs(*nodes_ref);
                return self.is_unit_value(nodes[0]);
            }
        }
        false
    }

    fn is_unit_value(&self, node_ref: NodeRef) -> bool {
        // Check if this represents a unit value (like empty break/continue)
        matches!(self.ast.get_node(node_ref), Node::ConstUnit)
    }

    /// Print function signature and body: (params) -> return_type { body }
    /// Shared between DefineFn and Func node printing
    fn print_function_signature_and_body(&mut self, func_index: FuncIndex, body: NodeRef, return_type: NodeRef) {
        self.emit_token(TokenType::LeftParen, "(");
        
        for (i, local) in self.ast.get_func_locals(func_index).iter().enumerate() {
            if !local.is_param {
                break;
            }
            if i > 0 {
                self.emit_token(TokenType::Comma, ", ");
            }
            if local.is_static {
                self.emit_token(TokenType::Static, "static ");
            }
            let param_name = self.ast.get_symbol(local.name);
            self.emit_token(TokenType::Identifier, param_name);
            if let Some(ty) = local.ty {
                self.emit_token(TokenType::Colon, ": ");
                self.print_node(ty);
            }
        }
        
        self.emit_token(TokenType::RightParen, ")");
        
        // Print return type if not unit
        if !matches!(
            self.ast.get_node(return_type),
            Node::TypeAtom(TypeAtom::Unit)
        ) {
            self.emit_token(TokenType::Arrow, " -> ");
            self.print_node(return_type);
        }
        
        self.buffer.push(' ');
        
        // Print function body
        self.print_node(body);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Parser;

    // Test utility function for reformat mode  
    fn reformat_prints_as(input: &str, expected_output: &str) {
        let mut lexer = crate::lexer::Lexer::new(input);
        let tokens = lexer.tokenize();
        let ast = Parser::parse(input).unwrap();
        let printer = PrettyPrinter::new_reformat(&ast, &tokens, input, 4);
        let output = printer.print();
        assert_eq!(expected_output, output);
    }

    #[test]
    fn test_basic_comment_preservation() {
        // Test that comments are preserved (indentation is lost due to lexer not capturing it)
        reformat_prints_as(r#"fn main() {
    let x = 42; // Inline comment
    return x;
}"#, r#"fn main() {
    let x = 42; // Inline comment
    return x;
}"#);
    }

    #[test]
    fn test_inline_comment_detection() {
        // Test that inline comments are properly handled with spacing normalization
        reformat_prints_as(r#"fn main() {
    let x = 42;// No space
    let y = 24; // With space
    return x + y;
}"#, r#"fn main() {
    let x = 42; // No space
    let y = 24; // With space
    return x + y;
}"#);
    }

    #[test]
    fn test_standalone_comment_handling() {
        // Test that standalone comments are preserved with their indentation
        reformat_prints_as(r#"fn main() {
    // Standalone comment
    let x = 42;
    // Another comment
    return x;
}"#, r#"fn main() {
    // Standalone comment
    let x = 42;
    // Another comment
    return x;
}"#);
    }

    #[test]
    fn test_empty_line_basic_behavior() {
        // Test that empty lines are preserved (limited to max 2)
        reformat_prints_as(r#"fn main() {
let x = 42;

    let y = 24;



    return x + y;
}"#, r#"fn main() {
    let x = 42;

    let y = 24;


    return x + y;
}"#);
    }

    #[test]
    fn test_mixed_comments_and_formatting() {
        // Test comprehensive scenario showing current behavior
        reformat_prints_as(r#"// Header comment
fn main() {
    // Standalone comment
    let x = 42; // Inline comment
    
    if x == 42 {
    return true; // Success
    }
}"#, r#"// Header comment
fn main() {
    // Standalone comment
    let x = 42; // Inline comment

    if x == 42 {
        return true; // Success
    }
}"#);
    }

    #[test]
    fn test_boolean_operators_formatting() {
        // Test basic boolean operators
        reformat_prints_as(r#"fn main() {
    let a = true && false;
    let b = true || false;
    let c = !true;
    return a;
}"#, r#"fn main() {
    let a = true && false;
    let b = true || false;
    let c = !true;
    return a;
}"#);
    }

    #[test]
    fn test_boolean_operator_precedence_formatting() {
        // Test precedence with proper parentheses
        reformat_prints_as(r#"fn main() {
    let result = !true && false || true == false;
    return result;
}"#, r#"fn main() {
    let result = !true && false || true == false;
    return result;
}"#);
    }

    #[test]
    fn test_comparison_operators_formatting() {
        // Test basic comparison operators
        reformat_prints_as(r#"fn main() {
    let a = 5 < 10;
    let b = 10 > 5;
    let c = 5 <= 10;
    let d = 10 >= 5;
    return a;
}"#, r#"fn main() {
    let a = 5 < 10;
    let b = 10 > 5;
    let c = 5 <= 10;
    let d = 10 >= 5;
    return a;
}"#);
    }

    #[test]
    fn test_mixed_operators_formatting() {
        // Test mixed arithmetic, comparison, and boolean operators
        reformat_prints_as(r#"fn main() {
    let result = 1 + 2 < 5 && 10 >= 3 * 2 || false;
    return result;
}"#, r#"fn main() {
    let result = 1 + 2 < 5 && 10 >= 3 * 2 || false;
    return result;
}"#);
    }
}

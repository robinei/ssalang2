use crate::lexer::TokenType;

pub type BlockIndex = u16;
pub type FuncIndex = u16;

// Bundle func and local index together
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalRef {
    pub func: FuncIndex,
    pub index: u16,  // 0-based index within the func
}

impl LocalRef {
    pub fn new(func: FuncIndex, index: u16) -> Self {
        Self { func, index }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeRef(u32);

impl NodeRef {
    fn new(value: u32) -> Self {
        Self(value)
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

// Handle types for out-of-band storage
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringRef(u32);

impl StringRef {
    pub fn new(value: u32) -> Self {
        Self(value)
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolRef(u32);

impl SymbolRef {
    pub fn new(value: u32) -> Self {
        Self(value)
    }

    pub fn get(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalsRef {
    pub offset: u16,
    pub count: u16,
}

impl LocalsRef {
    pub fn new(offset: u16, count: u16) -> Self {
        Self { offset, count }
    }

    pub fn empty() -> Self {
        Self {
            offset: 0,
            count: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodesRef {
    pub offset: u16,
    pub count: u16,
}

impl NodesRef {
    pub fn new(offset: u16, count: u16) -> Self {
        Self { offset, count }
    }

    pub fn empty() -> Self {
        Self {
            offset: 0,
            count: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum IsInline {
    No,
    Yes
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum IsStatic {
    No,
    Yes
}


// Separate enum for type atoms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TypeAtom {
    Unit = 0,
    Bool = 1,
    I32 = 2,
}

// Unary operator types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum UnopType {
    Neg = 0, // -x
    Not = 1, // !x
}

impl UnopType {
    pub fn precedence(self) -> i32 {
        6 // Unary operators have highest precedence
    }
    
    pub fn token_type(self) -> crate::lexer::TokenType {
        match self {
            UnopType::Neg => crate::lexer::TokenType::Minus,
            UnopType::Not => crate::lexer::TokenType::Not,
        }
    }
    
    pub fn token_text(self) -> &'static str {
        match self {
            UnopType::Neg => "-",
            UnopType::Not => "!",
        }
    }
}

// Binary operator types  
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum BinopType {
    Add = 0,   // +
    Sub = 1,   // -
    Mul = 2,   // *
    Div = 3,   // /
    Eq = 4,    // ==
    Neq = 5,   // !=
    Lt = 6,    // <
    Gt = 7,    // >
    LtEq = 8,  // <=
    GtEq = 9,  // >=
    And = 10,  // &&
    Or = 11,   // ||
}

impl BinopType {
    pub fn precedence(self) -> i32 {
        match self {
            BinopType::Or => 1,                                 // Logical OR: lowest precedence
            BinopType::And => 2,                                // Logical AND
            BinopType::Eq | BinopType::Neq | BinopType::Lt | 
            BinopType::Gt | BinopType::LtEq | BinopType::GtEq => 3, // Equality and comparison
            BinopType::Add | BinopType::Sub => 4,               // Addition/subtraction
            BinopType::Mul | BinopType::Div => 5,               // Multiplication/division
        }
    }

    pub fn from_token_type(token_type: TokenType) -> Option<Self> {
        match token_type {
            TokenType::Plus => Some(BinopType::Add),
            TokenType::Minus => Some(BinopType::Sub),
            TokenType::Star => Some(BinopType::Mul),
            TokenType::Slash => Some(BinopType::Div),
            TokenType::Equal => Some(BinopType::Eq),
            TokenType::NotEqual => Some(BinopType::Neq),
            TokenType::Lt => Some(BinopType::Lt),
            TokenType::Gt => Some(BinopType::Gt),
            TokenType::LtEq => Some(BinopType::LtEq),
            TokenType::GtEq => Some(BinopType::GtEq),
            TokenType::And => Some(BinopType::And),
            TokenType::Or => Some(BinopType::Or),
            _ => None,
        }
    }
    
    pub fn token_type(self) -> crate::lexer::TokenType {
        match self {
            BinopType::Add => crate::lexer::TokenType::Plus,
            BinopType::Sub => crate::lexer::TokenType::Minus,
            BinopType::Mul => crate::lexer::TokenType::Star,
            BinopType::Div => crate::lexer::TokenType::Slash,
            BinopType::Eq => crate::lexer::TokenType::Equal,
            BinopType::Neq => crate::lexer::TokenType::NotEqual,
            BinopType::Lt => crate::lexer::TokenType::Lt,
            BinopType::Gt => crate::lexer::TokenType::Gt,
            BinopType::LtEq => crate::lexer::TokenType::LtEq,
            BinopType::GtEq => crate::lexer::TokenType::GtEq,
            BinopType::And => crate::lexer::TokenType::And,
            BinopType::Or => crate::lexer::TokenType::Or,
        }
    }
    
    pub fn token_text(self) -> &'static str {
        match self {
            BinopType::Add => " + ",
            BinopType::Sub => " - ",
            BinopType::Mul => " * ",
            BinopType::Div => " / ",
            BinopType::Eq => " == ",
            BinopType::Neq => " != ",
            BinopType::Lt => " < ",
            BinopType::Gt => " > ",
            BinopType::LtEq => " <= ",
            BinopType::GtEq => " >= ",
            BinopType::And => " && ",
            BinopType::Or => " || ",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Node {
    // Type nodes
    TypeAtom(TypeAtom), // atom

    // Constant nodes
    ConstUnit,              // unit value ()
    ConstBool(bool),        // value
    ConstI32(i32),          // value
    ConstString(StringRef), // string_ref

    // Operator nodes
    Unop(UnopType, NodeRef),              // op_type, operand
    Binop(BinopType, NodeRef, NodeRef),   // op_type, left, right

    // Local variable nodes
    DefineFn(LocalRef, NodeRef),   // local_ref, function_node - fn name() {} syntax
    Define(LocalRef, NodeRef),     // local_ref, expr - const/let/static syntax
    Assign(LocalRef, NodeRef),     // local_ref, expr - assignment to existing binding
    LocalRead(LocalRef),           // local_ref

    // Control flow nodes
    Block(BlockIndex, NodesRef), // is_static, block_index, nodes
    If(IsInline, NodeRef, NodeRef, NodeRef),    // is_inline, cond, then, els
    While(IsInline, NodeRef, NodeRef),          // is_inline, cond, body

    // Jump nodes
    Break(BlockIndex, NodeRef), // block_index, value
    Continue(BlockIndex, NodeRef), // block_index, value
    Return(NodeRef),                   // value_node

    // Function node
    Fn(FuncIndex, NodeRef, NodeRef), // func_index, body, return_type

    // Module node
    Module(FuncIndex, NodesRef), // func_index, nodes
}

// Per-node metadata stored alongside AST nodes
#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub first_token: usize, // Index into token stream where this node starts
}

impl NodeInfo {
    pub fn new(first_token: usize) -> Self {
        Self { first_token }
    }
}

// Func for function-level variable storage
#[derive(Debug, Clone)]
pub struct Func {
    pub is_static: bool,
    pub is_inline: bool,
    pub locals: LocalsRef,  // Points to locals in the flat array
    pub parent: Option<FuncIndex>,  // For nested functions later
}

// Block for control flow with optional label
#[derive(Debug, Clone)]
pub struct Block {
    pub is_static: bool,
    pub name: Option<SymbolRef>,  // Optional block label
}

// Separate structure for local variable metadata (used in function contexts)
#[derive(Debug, Clone)]
pub struct Local {
    pub name: SymbolRef,
    pub is_param: bool,
    pub is_static: bool,
    pub is_const: bool,
    pub ty: Option<NodeRef>,
}

// AST that owns all associated data including the tree structure itself
pub struct Ast {
    nodes: Vec<Node>,         // All AST nodes
    node_info: Vec<NodeInfo>, // Per-node metadata (source location, etc.)
    locals: Vec<Local>,       // Flat array of all locals across all functions
    node_refs: Vec<NodeRef>,  // Flat array of all node references in blocks and modules
    strings: Vec<String>,     // String storage for string literals
    symbols: Vec<String>,     // Symbol storage for identifiers (interned)
    symbol_map: std::collections::HashMap<String, SymbolRef>, // For symbol interning
    funcs: Vec<Func>,       // All function funcs
    blocks: Vec<Block>,       // All blocks
    root: Option<NodeRef>,    // Root node of the tree
}

impl Ast {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            node_info: Vec::new(),
            locals: Vec::new(),
            node_refs: Vec::new(),
            strings: Vec::new(),
            symbols: Vec::new(),
            symbol_map: std::collections::HashMap::new(),
            funcs: Vec::new(),
            blocks: Vec::new(),
            root: None,
        }
    }

    pub fn set_root(&mut self, root_ref: NodeRef) {
        self.root = Some(root_ref);
    }

    pub fn get_root(&self) -> Option<NodeRef> {
        self.root
    }

    // AST node methods
    pub fn add_node(&mut self, node: Node, info: NodeInfo) -> NodeRef {
        let index = self.nodes.len();
        self.nodes.push(node);
        self.node_info.push(info);
        NodeRef::new(index as u32)
    }

    pub fn get_node(&self, node_ref: NodeRef) -> &Node {
        let index = node_ref.get() as usize;
        &self.nodes[index]
    }

    pub fn get_node_mut(&mut self, node_ref: NodeRef) -> &mut Node {
        let index = node_ref.get() as usize;
        &mut self.nodes[index]
    }

    pub fn get_node_info(&self, node_ref: NodeRef) -> &NodeInfo {
        let index = node_ref.get() as usize;
        &self.node_info[index]
    }

    // Local storage methods
    pub fn add_local(&mut self, local: Local) -> u16 {
        let index = self.locals.len() as u16;
        self.locals.push(local);
        index
    }

    pub fn add_locals(&mut self, locals: &[Local]) -> LocalsRef {
        let count = locals.len() as u16;
        if count == 0 {
            return LocalsRef::empty();
        }
        let offset = self.locals.len() as u16;
        self.locals.extend_from_slice(locals);
        LocalsRef::new(offset, count)
    }

    pub fn get_locals(&self, locals_ref: LocalsRef) -> &[Local] {
        let start = locals_ref.offset as usize;
        let end = start + (locals_ref.count as usize);
        &self.locals[start..end]
    }

    pub fn get_func_locals(&self, func_index: FuncIndex) -> &[Local] {
        let func = &self.funcs[func_index as usize];
        self.get_locals(func.locals)
    }

    pub fn get_local(&self, local_ref: LocalRef) -> &Local {
        let locals = self.get_func_locals(local_ref.func);
        &locals[local_ref.index as usize]
    }

    pub fn get_local_name(&self, local_ref: LocalRef) -> &str {
        let local = self.get_local(local_ref);
        self.get_symbol(local.name)
    }

    pub fn locals_len(&self) -> usize {
        self.locals.len()
    }

    // Func management methods
    pub fn add_func(&mut self, func: Func) -> FuncIndex {
        let func_index = self.funcs.len() as FuncIndex;
        self.funcs.push(func);
        func_index
    }

    pub fn get_func(&self, func_index: FuncIndex) -> &Func {
        &self.funcs[func_index as usize]
    }

    pub fn get_func_mut(&mut self, func_index: FuncIndex) -> &mut Func {
        &mut self.funcs[func_index as usize]
    }

    // Block management methods
    pub fn add_block(&mut self, block: Block) -> BlockIndex {
        let block_index = self.blocks.len() as BlockIndex;
        self.blocks.push(block);
        block_index
    }

    pub fn get_block(&self, block_index: BlockIndex) -> &Block {
        &self.blocks[block_index as usize]
    }

    pub fn get_block_mut(&mut self, block_index: BlockIndex) -> &mut Block {
        &mut self.blocks[block_index as usize]
    }

    // Node reference storage methods
    pub fn add_node_refs(&mut self, node_refs: &[NodeRef]) -> NodesRef {
        let offset = self.node_refs.len() as u16;
        let count = node_refs.len() as u16;
        self.node_refs.extend_from_slice(node_refs);
        NodesRef::new(offset, count)
    }

    pub fn get_node_refs(&self, nodes_ref: NodesRef) -> &[NodeRef] {
        let start = nodes_ref.offset as usize;
        let end = start + (nodes_ref.count as usize);
        &self.node_refs[start..end]
    }

    // String storage methods (for string literals)
    pub fn add_string(&mut self, string: String) -> StringRef {
        let string_ref = StringRef::new(self.strings.len() as u32);
        self.strings.push(string);
        string_ref
    }

    pub fn get_string(&self, string_ref: StringRef) -> &str {
        let index = string_ref.get() as usize;
        &self.strings[index]
    }

    // Symbol storage methods (for identifiers, interned)
    pub fn intern_symbol(&mut self, symbol: String) -> SymbolRef {
        // Check if symbol already exists (interning)
        if let Some(&existing_ref) = self.symbol_map.get(&symbol) {
            return existing_ref;
        }

        // Add new symbol
        let symbol_ref = SymbolRef::new(self.symbols.len() as u32);
        self.symbols.push(symbol.clone());
        self.symbol_map.insert(symbol, symbol_ref);
        symbol_ref
    }

    pub fn get_symbol(&self, symbol_ref: SymbolRef) -> &str {
        let index = symbol_ref.get() as usize;
        &self.symbols[index]
    }

    // Debug method to check symbol interning
    #[cfg(test)]
    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_interning() {
        let mut ast = Ast::new();

        // Add the same symbol multiple times
        let sym1 = ast.intern_symbol("test".to_string());
        let sym2 = ast.intern_symbol("test".to_string());
        let sym3 = ast.intern_symbol("different".to_string());
        let sym4 = ast.intern_symbol("test".to_string());

        // Should only have 2 unique symbols stored
        assert_eq!(ast.symbol_count(), 2);

        // Same symbol name should return same reference
        assert_eq!(sym1, sym2);
        assert_eq!(sym1, sym4);
        assert_ne!(sym1, sym3);

        // Verify we can get back the correct strings
        assert_eq!(ast.get_symbol(sym1), "test");
        assert_eq!(ast.get_symbol(sym3), "different");
    }
}

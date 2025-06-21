pub type LocalIndex = u16;
pub type ScopeIndex = u16;

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

// Flags for packing boolean values across all AST node types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flags(u8);

impl Flags {
    const IS_STATIC: u8 = 0x01; // Block, If, While, Break, Continue, Func: is_static
    const IS_INLINE: u8 = 0x02; // If, While, Func: is_inline

    pub fn new() -> Self {
        Self(0)
    }

    pub fn is_static(self) -> bool {
        (self.0 & Self::IS_STATIC) != 0
    }

    pub fn set_is_static(&mut self, value: bool) {
        if value {
            self.0 |= Self::IS_STATIC;
        } else {
            self.0 &= !Self::IS_STATIC;
        }
    }

    pub fn is_inline(self) -> bool {
        (self.0 & Self::IS_INLINE) != 0
    }

    pub fn set_is_inline(&mut self, value: bool) {
        if value {
            self.0 |= Self::IS_INLINE;
        } else {
            self.0 &= !Self::IS_INLINE;
        }
    }
}

// Separate enum for type atoms
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum TypeAtom {
    Unit = 0,
    Bool = 1,
    I32 = 2,
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

    // Unary operation nodes
    UnopNeg(NodeRef), // operand
    UnopNot(NodeRef), // operand

    // Binary operation nodes
    BinopAdd(NodeRef, NodeRef), // left, right
    BinopSub(NodeRef, NodeRef), // left, right
    BinopMul(NodeRef, NodeRef), // left, right
    BinopDiv(NodeRef, NodeRef), // left, right
    BinopEq(NodeRef, NodeRef),  // left, right
    BinopNeq(NodeRef, NodeRef), // left, right
    BinopLt(NodeRef, NodeRef),  // left, right
    BinopGt(NodeRef, NodeRef),  // left, right
    BinopLtEq(NodeRef, NodeRef), // left, right
    BinopGtEq(NodeRef, NodeRef), // left, right
    BinopAnd(NodeRef, NodeRef), // left, right
    BinopOr(NodeRef, NodeRef),  // left, right

    // Local variable nodes
    LocalWrite(bool, LocalIndex, NodeRef), // is_definition, local_index, expr
    LocalRead(LocalIndex),                 // local_index

    // Control flow nodes
    Block(Flags, ScopeIndex, NodesRef), // flags, scope_index, nodes
    If(Flags, NodeRef, NodeRef, NodeRef),    // flags, cond, then, els
    While(Flags, NodeRef, NodeRef),          // flags, cond, body

    // Jump nodes
    Break(Flags, ScopeIndex, NodeRef), // flags, scope_index, value
    Continue(Flags, ScopeIndex, NodeRef), // flags, scope_index, value
    Return(NodeRef),                   // value_node

    // Function node
    Func(Flags, LocalsRef, NodeRef, NodeRef), // flags, parameters, body, return_type

    // Module node
    Module(NodesRef), // nodes
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

// Separate structure for local variable metadata (used in function contexts)
#[derive(Debug, Clone)]
pub struct Local {
    pub name: SymbolRef,
    pub is_param: bool,
    pub is_static: bool,
    pub is_const: bool,
    pub ty: NodeRef,
}

// AST that owns all associated data including the tree structure itself
pub struct Ast {
    nodes: Vec<Node>,                                         // All AST nodes
    node_info: Vec<NodeInfo>, // Per-node metadata (source location, etc.)
    locals: Vec<Local>,       // Flat array of all locals across all functions
    node_refs: Vec<NodeRef>, // Flat array of all node references in blocks and modules
    strings: Vec<String>,     // String storage for string literals
    symbols: Vec<String>,     // Symbol storage for identifiers (interned)
    symbol_map: std::collections::HashMap<String, SymbolRef>, // For symbol interning
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

    pub fn get_node_info(&self, node_ref: NodeRef) -> &NodeInfo {
        let index = node_ref.get() as usize;
        &self.node_info[index]
    }

    // Local storage methods
    pub fn add_local(&mut self, local: Local) -> LocalIndex {
        let index = self.locals.len() as LocalIndex;
        self.locals.push(local);
        index
    }

    pub fn add_locals(&mut self, locals: &[Local]) -> LocalsRef {
        let offset = self.locals.len() as u16;
        let count = locals.len() as u16;
        self.locals.extend_from_slice(locals);
        LocalsRef::new(offset, count)
    }

    pub fn get_locals(&self, locals_ref: LocalsRef) -> &[Local] {
        let start = locals_ref.offset as usize;
        let end = start + (locals_ref.count as usize);
        &self.locals[start..end]
    }

    pub fn get_local(&self, index: LocalIndex) -> &Local {
        &self.locals[index as usize]
    }

    pub fn get_local_name(&self, index: LocalIndex) -> &str {
        let local = self.get_local(index);
        self.get_symbol(local.name)
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

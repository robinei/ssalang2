use crate::lexer::Token;

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
pub struct LocalsRef {
    pub offset: u16,
    pub count: u16,
}

impl LocalsRef {
    pub fn new(offset: u16, count: u16) -> Self {
        Self { offset, count }
    }

    pub fn empty() -> Self {
        Self { offset: 0, count: 0 }
    }
}

// Flags for packing boolean values across all AST node types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flags(u8);

impl Flags {
    const IS_STATIC: u8 = 0x01;      // Block, If, While, Break, Continue, Func: is_static
    const IS_INLINE: u8 = 0x02;      // If, While, Func: is_inline

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
    Void = 0,
    Bool = 1,
    I32 = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Node {
    // Type nodes
    TypeAtom(TypeAtom), // atom
    
    // Constant nodes  
    ConstBool(bool), // value
    ConstI32(i32), // value
    ConstString(StringRef), // string_ref

    // Binary operation nodes
    BinopAdd(NodeRef, NodeRef), // left, right
    BinopEq(NodeRef, NodeRef),  // left, right
    BinopNeq(NodeRef, NodeRef), // left, right
    
    // Local variable nodes
    LocalWrite(bool, LocalIndex, NodeRef), // is_definition, local_index, expr
    LocalRead(LocalIndex), // local_index
    
    // Control flow nodes
    Block(Flags, ScopeIndex, NodeRef), // flags, scope_index, first_child
    If(Flags, ScopeIndex, NodeRef, NodeRef, NodeRef), // flags, scope_index, cond, then, els
    While(Flags, ScopeIndex, NodeRef, NodeRef), // flags, scope_index, cond, body
    
    // Jump nodes
    Break(Flags, ScopeIndex, NodeRef), // flags, scope_index, value
    Continue(Flags, ScopeIndex, NodeRef), // flags, scope_index, value
    Return(NodeRef), // value_node
    
    // Function node
    Func(Flags, LocalsRef, NodeRef, NodeRef), // flags, locals_ref, body, return_type
}

// Per-node metadata stored alongside AST nodes
#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub first_token: usize,  // Index into token stream where this node starts
}

impl NodeInfo {
    pub fn new(first_token: usize) -> Self {
        Self { first_token }
    }
}

// Separate structure for local variable metadata (used in function contexts)
#[derive(Debug, Clone)]
pub struct Local {
    pub name: StringRef,
    pub is_param: bool,
    pub is_static: bool,
    pub is_const: bool,
    pub ty: NodeRef,
}


// AST that owns all associated data including the tree structure itself
pub struct Ast {
    nodes: Vec<Node>,         // All AST nodes
    node_info: Vec<NodeInfo>, // Per-node metadata (source location, etc.)
    locals: Vec<Local>,       // Flat array of all locals across all functions
    strings: Vec<String>,     // String storage
    tokens: Vec<Token>,       // Full token stream including formatting tokens
    source: String,           // Original source text for token text extraction
    root: Option<NodeRef>,    // Root node of the tree
}

impl Ast {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            node_info: Vec::new(),
            locals: Vec::new(),
            strings: Vec::new(),
            tokens: Vec::new(),
            source: String::new(),
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
        self.get_string(local.name)
    }

    // String storage methods
    pub fn add_string(&mut self, string: String) -> StringRef {
        let string_ref = StringRef::new(self.strings.len() as u32);
        self.strings.push(string);
        string_ref
    }

    pub fn get_string(&self, string_ref: StringRef) -> &str {
        let index = string_ref.get() as usize;
        &self.strings[index]
    }

    // Token storage methods
    pub fn set_tokens(&mut self, tokens: Vec<Token>) {
        self.tokens = tokens;
    }

    pub fn get_tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn set_source(&mut self, source: String) {
        self.source = source;
    }

    pub fn get_source(&self) -> &str {
        &self.source
    }

}

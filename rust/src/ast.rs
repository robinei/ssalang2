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
    pub source_loc: u32,
}

impl NodeInfo {
    pub fn new(source_loc: u32) -> Self {
        Self { source_loc }
    }
}

// Separate structure for local variable metadata (used in function contexts)
#[derive(Debug, Clone)]
pub struct Local {
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
    root: Option<NodeRef>,    // Root node of the tree
}

impl Ast {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            node_info: Vec::new(),
            locals: Vec::new(),
            strings: Vec::new(),
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_node_ref() {
        let mut tree = Ast::new();
        let r1 = tree.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let r2 = tree.add_node(Node::ConstBool(false), NodeInfo::new(0));
        
        assert_eq!(r1.get(), 0);
        assert_eq!(r2.get(), 1);
        assert_ne!(r1, r2);
        
        // Test that we can get the underlying value
        let val: u32 = r1.get();
        assert_eq!(val, 0);
    }

    #[test]
    fn test_flags() {
        let mut flags = Flags::new();
        assert!(!flags.is_static());
        assert!(!flags.is_inline());

        flags.set_is_static(true);
        flags.set_is_inline(true);
        assert!(flags.is_static());
        assert!(flags.is_inline());
    }

    #[test]
    fn test_ast_node_flags() {
        let mut tree = Ast::new();
        let cond_ref = tree.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let then_ref = tree.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let else_ref = tree.add_node(Node::ConstBool(false), NodeInfo::new(0));
        
        let mut if_flags = Flags::new();
        if_flags.set_is_static(true);
        if_flags.set_is_inline(false);
        let if_node = Node::If(
            if_flags,
            5, 
            cond_ref, 
            then_ref, 
            else_ref
        );

        if let Node::If(flags, _, _, _, _) = if_node {
            assert!(flags.is_static());
            assert!(!flags.is_inline());
        } else {
            panic!("Expected If node");
        }

        let body_ref = tree.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let return_type_ref = tree.add_node(Node::TypeAtom(TypeAtom::Bool), NodeInfo::new(0));
        
        let mut func_flags = Flags::new();
        func_flags.set_is_static(false);
        func_flags.set_is_inline(true);
        let locals_ref = LocalsRef::new(0, 2);
        let func_node = Node::Func(
            func_flags,
            locals_ref,
            body_ref,
            return_type_ref
        );

        if let Node::Func(flags, locals_ref, _, _) = func_node {
            assert!(!flags.is_static());
            assert!(flags.is_inline());
            assert_eq!(locals_ref.offset, 0);
            assert_eq!(locals_ref.count, 2);
        } else {
            panic!("Expected Func node");
        }
    }

    #[test]
    fn test_ast_node_size() {
        // Test that the enum is optimally sized
        assert_eq!(std::mem::size_of::<Node>(), 16);
    }

    #[test]
    fn test_local_write_definition_flag() {
        let mut tree = Ast::new();
        let expr_ref = tree.add_node(Node::ConstI32(42), NodeInfo::new(0));
        
        let write_node = Node::LocalWrite(true, 10, expr_ref);
        if let Node::LocalWrite(is_definition, _, _) = write_node {
            assert!(is_definition);
        } else {
            panic!("Expected LocalWrite node");
        }

        let expr_ref2 = tree.add_node(Node::ConstI32(100), NodeInfo::new(0));
        let assign_node = Node::LocalWrite(false, 20, expr_ref2);
        if let Node::LocalWrite(is_definition, _, _) = assign_node {
            assert!(!is_definition);
        } else {
            panic!("Expected LocalWrite node");
        }
    }

    #[test]
    fn test_ast_tree_locals() {
        let mut tree = Ast::new();
        
        // Create type references for locals
        let type1_ref = tree.add_node(Node::TypeAtom(TypeAtom::I32), NodeInfo::new(0));
        let type2_ref = tree.add_node(Node::TypeAtom(TypeAtom::Bool), NodeInfo::new(0));
        
        // Create some locals
        let local1 = Local { is_param: true, is_static: false, is_const: false, ty: type1_ref };
        let local2 = Local { is_param: true, is_static: false, is_const: true, ty: type2_ref };
        let locals = vec![local1, local2];
        
        // Store the locals and get LocalsRef
        let locals_ref = tree.add_locals(&locals);
        assert_eq!(locals_ref.offset, 0);
        assert_eq!(locals_ref.count, 2);
        
        // Retrieve the locals using LocalsRef
        let retrieved_locals = tree.get_locals(locals_ref);
        assert_eq!(retrieved_locals.len(), 2);
        assert!(retrieved_locals[0].is_param);
        assert!(!retrieved_locals[0].is_const);
        assert!(retrieved_locals[1].is_param);
        assert!(retrieved_locals[1].is_const);
        
        // Test individual local access
        let local = tree.get_local(0);
        assert!(local.is_param);
        assert_eq!(local.ty.get(), type1_ref.get());
    }

    #[test]
    fn test_ast_tree_strings() {
        let mut tree = Ast::new();
        
        // Store some strings
        let hello_ref = tree.add_string("Hello".to_string());
        let world_ref = tree.add_string("World".to_string());
        
        // Retrieve the strings using refs
        assert_eq!(tree.get_string(hello_ref), "Hello");
        assert_eq!(tree.get_string(world_ref), "World");
        
        // Test empty string by adding one
        let empty_ref = tree.add_string("".to_string());
        assert_eq!(tree.get_string(empty_ref), "");
        
        // Test ConstString node
        let string_node = Node::ConstString(hello_ref);
        if let Node::ConstString(string_ref) = string_node {
            assert_eq!(string_ref.get(), hello_ref.get());
        } else {
            panic!("Expected ConstString node");
        }
    }

    #[test]
    fn test_ast_tree_nodes() {
        let mut tree = Ast::new();
        
        // Add some nodes to the context
        let bool_node = Node::ConstBool(true);
        let i32_node = Node::ConstI32(42);
        
        let bool_ref = tree.add_node(bool_node, NodeInfo::new(100));
        let i32_ref = tree.add_node(i32_node, NodeInfo::new(200));
        
        // Retrieve nodes
        if let Node::ConstBool(value) = tree.get_node(bool_ref) {
            assert_eq!(*value, true);
        } else {
            panic!("Expected ConstBool node");
        }
        
        if let Node::ConstI32(value) = tree.get_node(i32_ref) {
            assert_eq!(*value, 42);
        } else {
            panic!("Expected ConstI32 node");
        }
    }

    #[test]
    fn test_ast_local() {
        let mut tree = Ast::new();
        let type_ref = tree.add_node(Node::TypeAtom(TypeAtom::I32), NodeInfo::new(0));
        let local = Local { is_param: true, is_static: false, is_const: false, ty: type_ref };
        assert!(local.is_param);
        assert!(!local.is_static);
        assert!(!local.is_const);
        assert_eq!(local.ty.get(), type_ref.get());
    }

    #[test]
    fn test_node_info() {
        let mut tree = Ast::new();
        let bool_ref = tree.add_node(Node::ConstBool(true), NodeInfo::new(123));
        let i32_ref = tree.add_node(Node::ConstI32(42), NodeInfo::new(456));
        
        let bool_info = tree.get_node_info(bool_ref);
        let i32_info = tree.get_node_info(i32_ref);
        
        assert_eq!(bool_info.source_loc, 123);
        assert_eq!(i32_info.source_loc, 456);
    }
}
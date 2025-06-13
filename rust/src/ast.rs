pub type AstRef = u32;
pub type AstNodeRef = AstRef;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstNodeTag {
    TypeAtom,
    
    ConstBool,
    ConstI32,

    BinopAdd,
    BinopEq,
    BinopNeq,
    
    LocalWrite,
    LocalRead,
    
    Block,
    If,
    While,
    
    Break,
    Cont,
    Return,
    
    Func,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstTypeAtomTag {
    Void,
    Bool,
    I32,
}

#[derive(Debug)]
pub struct AstNode {
    pub tag: AstNodeTag,
    pub source_pos: u32,
}

#[derive(Debug)]
pub struct AstConstNode {
    pub node: AstNode,
    pub value: AstConstValue,
}

#[derive(Debug)]
pub enum AstConstValue {
    Bool(bool),
    I32(i32),
}

#[derive(Debug)]
pub struct AstBinopNode {
    pub node: AstNode,
    pub left_node: AstNodeRef,
    pub right_node: AstNodeRef,
}

#[derive(Debug)]
pub struct AstTypeAtomNode {
    pub node: AstNode,
    pub atom: AstTypeAtomTag,
}

#[derive(Debug)]
pub struct AstLocalWriteNode {
    pub node: AstNode,
    pub is_definition: bool,
    pub local_index: u32,
    pub expr: AstNodeRef,
}

#[derive(Debug)]
pub struct AstLocalReadNode {
    pub node: AstNode,
    pub local_index: u32,
}

#[derive(Debug)]
pub struct AstBlockNode {
    pub node: AstNode,
    pub is_static: bool,
    pub scope_index: u32,
    pub nodes: Vec<AstNodeRef>,
}

#[derive(Debug)]
pub struct AstIfNode {
    pub node: AstNode,
    pub is_static: bool,
    pub is_inline: bool,
    pub scope_index: u32,
    pub cond: AstNodeRef,
    pub then: AstNodeRef,
    pub els: AstNodeRef,
}

#[derive(Debug)]
pub struct AstWhileNode {
    pub node: AstNode,
    pub is_static: bool,
    pub is_inline: bool,
    pub scope_index: u32,
    pub cond: AstNodeRef,
    pub body: AstNodeRef,
}

#[derive(Debug)]
pub struct AstBreakContNode {
    pub node: AstNode,
    pub is_static: bool,
    pub scope_index: u32,
    pub value: AstNodeRef,
}

#[derive(Debug)]
pub struct AstReturnNode {
    pub node: AstNode,
    pub value_node: AstNodeRef,
}

#[derive(Debug)]
pub struct AstLocal {
    pub is_param: bool,
    pub is_static: bool,
    pub is_const: bool,
    pub r#type: AstNodeRef,
}

#[derive(Debug)]
pub struct AstFuncNode {
    pub node: AstNode,
    pub is_static: bool,
    pub is_inline: bool,
    pub body: AstNodeRef,
    pub return_type: AstNodeRef,
    pub params_count: u32,
    pub locals: Vec<AstLocal>,
}
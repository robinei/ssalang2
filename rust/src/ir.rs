pub type IrOperand = i16;

pub type IrInstrRef = IrOperand;
pub type IrBlockRef = IrOperand;
pub type IrPhiRef = IrOperand;
pub type IrVarRef = IrOperand;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrType {
    Void,
    Bool,
    I32,
}

pub const IR_FLAG_MARK: u32 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrInstrTag {
    Nop,
    Identity,
    Print,
    Label,
    Jump,
    Branch,
    Ret,
    Upsilon,
    Phi,
    Const,
    Arg,
    Add,
    Eq,
    Neq,
}

impl IrInstrTag {
    pub fn is_pure(&self) -> bool {
        matches!(self, Self::Const | Self::Arg | Self::Add | Self::Eq | Self::Neq)
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self, Self::Jump | Self::Branch | Self::Ret)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IrInstr {
    pub tag: IrInstrTag,
    pub r#type: IrType,
    pub flags: u32,
    pub arg0: IrOperand,
    pub args: IrInstrArgs,
}

#[derive(Debug, Clone, Copy)]
pub union IrInstrArgs {
    pub args: IrInstrArgPair,
    pub bool_const: bool,
    pub i32_const: i32,
    pub u32_const: u32,
    pub f32_const: f32,
}

#[derive(Debug, Clone, Copy)]
pub struct IrInstrArgPair {
    pub arg1: IrOperand,
    pub arg2: IrOperand,
}
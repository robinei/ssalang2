pub type Operand = i16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstrRef(pub Operand);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockRef(pub Operand);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhiRef(pub Operand);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarRef(pub Operand);

impl From<Operand> for InstrRef {
    fn from(value: Operand) -> Self {
        Self(value)
    }
}

impl From<InstrRef> for Operand {
    fn from(value: InstrRef) -> Self {
        value.0
    }
}

impl From<Operand> for BlockRef {
    fn from(value: Operand) -> Self {
        Self(value)
    }
}

impl From<BlockRef> for Operand {
    fn from(value: BlockRef) -> Self {
        value.0
    }
}

impl From<Operand> for PhiRef {
    fn from(value: Operand) -> Self {
        Self(value)
    }
}

impl From<PhiRef> for Operand {
    fn from(value: PhiRef) -> Self {
        value.0
    }
}

impl From<Operand> for VarRef {
    fn from(value: Operand) -> Self {
        Self(value)
    }
}

impl From<VarRef> for Operand {
    fn from(value: VarRef) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Type {
    Void = 0,
    Bool = 1,
    I32 = 2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Meta(u8);

impl Meta {
    pub fn new(ty: Type) -> Self {
        Self(ty as u8)
    }
    
    pub fn get_type(self) -> Type {
        match self.0 & 0x7F {
            0 => Type::Void,
            1 => Type::Bool,
            2 => Type::I32,
            _ => unreachable!(),
        }
    }
    
    pub fn is_marked(self) -> bool {
        (self.0 & 0x80) != 0
    }
    
    pub fn set_marked(&mut self, marked: bool) {
        if marked {
            self.0 |= 0x80;
        } else {
            self.0 &= 0x7F;
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Instr {
    Nop(Meta),
    Identity(Meta, InstrRef),
    Print(Meta, InstrRef),
    Label(Meta, BlockRef),
    Jump(Meta, BlockRef),
    Branch(Meta, InstrRef, BlockRef, BlockRef),
    Ret(Meta, InstrRef),
    Upsilon(Meta, PhiRef, InstrRef),
    Phi(Meta, PhiRef),
    ConstBool(Meta, bool),
    ConstI32(Meta, i32),
    Arg(Meta, Operand),
    Add(Meta, InstrRef, InstrRef),
    Eq(Meta, InstrRef, InstrRef),
    Neq(Meta, InstrRef, InstrRef),
}

impl Instr {
    pub fn is_pure(&self) -> bool {
        matches!(self,
            Self::ConstBool(..) | 
            Self::ConstI32(..) | 
            Self::Arg(..) | 
            Self::Add(..) | 
            Self::Eq(..) | 
            Self::Neq(..)
        )
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self, Self::Jump(..) | Self::Branch(..) | Self::Ret(..))
    }

    pub fn get_meta(&self) -> Meta {
        match self {
            Self::Nop(meta) => *meta,
            Self::Identity(meta, ..) => *meta,
            Self::Print(meta, ..) => *meta,
            Self::Label(meta, ..) => *meta,
            Self::Jump(meta, ..) => *meta,
            Self::Branch(meta, ..) => *meta,
            Self::Ret(meta, ..) => *meta,
            Self::Upsilon(meta, ..) => *meta,
            Self::Phi(meta, ..) => *meta,
            Self::ConstBool(meta, ..) => *meta,
            Self::ConstI32(meta, ..) => *meta,
            Self::Arg(meta, ..) => *meta,
            Self::Add(meta, ..) => *meta,
            Self::Eq(meta, ..) => *meta,
            Self::Neq(meta, ..) => *meta,
        }
    }

    pub fn get_type(&self) -> Type {
        self.get_meta().get_type()
    }

    pub fn is_marked(&self) -> bool {
        self.get_meta().is_marked()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instr_size() {
        // With packed Meta (1 byte), the largest variant (Branch) needs:
        // tag(1) + meta(1) + 3*Operand(6) = 8 bytes
        assert_eq!(std::mem::size_of::<Instr>(), 8);
        assert_eq!(std::mem::size_of::<Meta>(), 1);
        assert_eq!(std::mem::size_of::<Operand>(), 2);
    }

    #[test]
    fn test_meta() {
        let meta1 = Meta::new(Type::I32);
        assert_eq!(meta1.get_type(), Type::I32);
        assert!(!meta1.is_marked());
        
        let mut meta2 = Meta::new(Type::Bool);
        meta2.set_marked(true);
        assert_eq!(meta2.get_type(), Type::Bool);
        assert!(meta2.is_marked());
        
        let mut meta3 = Meta::new(Type::Void);
        assert!(!meta3.is_marked());
        meta3.set_marked(true);
        assert!(meta3.is_marked());
        meta3.set_marked(false);
        assert!(!meta3.is_marked());
    }

    #[test]
    fn test_bool_const() {
        let instr_true = Instr::ConstBool(
            Meta::new(Type::Bool),
            true,
        );
        
        let instr_false = Instr::ConstBool(
            Meta::new(Type::Bool),
            false,
        );
        
        if let Instr::ConstBool(_, value) = instr_true {
            assert_eq!(value, true);
        } else {
            panic!("Expected ConstBool variant");
        }
        
        if let Instr::ConstBool(_, value) = instr_false {
            assert_eq!(value, false);
        } else {
            panic!("Expected ConstBool variant");
        }
    }

    #[test]
    fn test_i32_const() {
        let instr = Instr::ConstI32(
            Meta::new(Type::I32),
            0x12345678,
        );
        
        if let Instr::ConstI32(_, value) = instr {
            assert_eq!(value, 0x12345678);
        } else {
            panic!("Expected ConstI32 variant");
        }
        
        let instr_neg = Instr::ConstI32(
            Meta::new(Type::I32),
            -1,
        );
        
        if let Instr::ConstI32(_, value) = instr_neg {
            assert_eq!(value, -1);
        } else {
            panic!("Expected ConstI32 variant");
        }
    }

    #[test]
    fn test_instr_methods() {
        let mut meta = Meta::new(Type::Void);
        meta.set_marked(true);
        let nop = Instr::Nop(meta);
        
        assert_eq!(nop.get_type(), Type::Void);
        assert!(nop.is_marked());
        assert!(!nop.is_pure());
        assert!(!nop.is_terminal());
        
        // Test get_meta method
        let meta = nop.get_meta();
        assert_eq!(meta.get_type(), Type::Void);
        assert!(meta.is_marked());
        
        let add = Instr::Add(
            Meta::new(Type::I32),
            InstrRef(1),
            InstrRef(2),
        );
        
        assert!(add.is_pure());
        assert!(!add.is_terminal());
        assert!(!add.is_marked());
        assert_eq!(add.get_meta().get_type(), Type::I32);
        
        let jump = Instr::Jump(
            Meta::new(Type::Void),
            BlockRef(5),
        );
        
        assert!(!jump.is_pure());
        assert!(jump.is_terminal());
    }

    #[test]
    fn test_ref_types() {
        let instr_ref = InstrRef::from(42);
        assert_eq!(Operand::from(instr_ref), 42);
        assert_eq!(instr_ref.0, 42);
        
        let block_ref = BlockRef::from(-1);
        assert_eq!(Operand::from(block_ref), -1);
        assert_eq!(block_ref.0, -1);
        
        let phi_ref = PhiRef::from(100);
        assert_eq!(Operand::from(phi_ref), 100);
        assert_eq!(phi_ref.0, 100);
        
        // Test equality and hashing work
        assert_eq!(InstrRef(42), InstrRef(42));
        assert_ne!(InstrRef(42), InstrRef(43));
    }
}

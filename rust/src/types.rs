use smallvec::SmallVec;

use std::collections::HashMap;
use std::sync::{Arc, LazyLock, RwLock};

struct TypeStore {
    pub type_table: Vec<Arc<Type>>,                    // TypeId indexes into this
    pub type_to_id: HashMap<Arc<Type>, TypeId>,        // For deduplication/interning
}

impl TypeStore {
    pub fn new() -> Self {
        Self {
            type_table: Vec::new(),
            type_to_id: HashMap::new(),
        }
    }

    /// Intern a type and return its TypeId (deduplicates identical types)
    pub fn intern_type(&mut self, mut ty: Type) -> TypeId {
        if let Type::Struct(fields) = &mut ty {
            fields.sort();
        }
        if let Some(&type_id) = self.type_to_id.get(&ty) {
            type_id
        } else {
            let type_id = TypeId::new(self.type_table.len() as u32);
            let tyref = Arc::new(ty);
            self.type_table.push(tyref.clone());
            self.type_to_id.insert(tyref, type_id);
            type_id
        }
    }

    /// Get a type by its TypeId
    pub fn lookup_type(&self, type_id: TypeId) -> Option<Arc<Type>> {
        self.type_table.get(type_id.as_u32() as usize).cloned()
    }
}

static TYPES: LazyLock<RwLock<TypeStore>> = LazyLock::new(|| RwLock::new(TypeStore::new()));

struct TypeIdCache {
    unit_id: TypeId,
    bool_id: TypeId,
    i32_id: TypeId,
    type_id: TypeId,
}

impl TypeIdCache {
    pub fn new() -> Self {
        Self {
            unit_id: Type::Unit.intern(),
            bool_id: Type::Bool.intern(),
            i32_id: Type::I32.intern(),
            type_id: Type::Type.intern(),
        }
    }
}

static CACHE: LazyLock<TypeIdCache> = LazyLock::new(|| TypeIdCache::new());


/// Language type system - richer than IR types but maps to them
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Bool,
    I32,
    Type,  // Represents the type of types (compile-time only)
    Fn(TypeId, SmallVec<[TypeId; 3]>),
    Struct(Vec<(String, TypeId)>)
}

impl Type {
    pub fn intern(self) -> TypeId {
        TYPES.write().unwrap().intern_type(self)
    }
}

/// Deterministic identifier for types (indexes into type table)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(u32);

impl TypeId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
    
    pub fn as_u32(self) -> u32 {
        self.0
    }
    
    pub fn as_i32(self) -> i32 {
        self.0 as i32
    }

    pub fn get_type(self) -> Option<Arc<Type>> {
        TYPES.read().unwrap().lookup_type(self)
    }

    pub fn unit_id() -> TypeId { CACHE.unit_id }
    pub fn bool_id() -> TypeId { CACHE.bool_id }
    pub fn i32_id() -> TypeId { CACHE.i32_id }
    pub fn type_id() -> TypeId { CACHE.type_id }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::I32 => write!(f, "i32"),
            Type::Type => write!(f, "type"),
            Type::Fn(ret, params) => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                p.fmt(f)?;
                if i < params.len() - 1 {
                    write!(f, ", ")?;
                }
                }
                if *ret == TypeId::unit_id() {
                write!(f, ")")?;
                } else {
                write!(f, ") -> ")?;
                ret.fmt(f)?;
                }
                Ok(())
            },
            Type::Struct(..) => write!(f, "struct"),
        }
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(type_ref) = self.get_type() {
            type_ref.fmt(f)?;
            Ok(())
        } else {
            write!(f, "TypeId({})", self.0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_size() {
        assert_eq!(32, size_of::<Type>());
    }
}

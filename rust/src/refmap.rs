/// A generic wrapper for storing values indexed by reference types.
/// This type ensures that index 0 is reserved and never accessible,
/// as all ref types use NonZeroI16 internally and cannot represent 0.
#[derive(Debug, Clone)]
pub struct RefMap<R, T> {
    data: Vec<T>,
    _phantom: std::marker::PhantomData<R>,
}

impl<R, T> RefMap<R, T> 
where 
    R: Copy + From<crate::ir::RefType> + Into<crate::ir::RefType>,
    T: Default,
{
    /// Creates a new RefMap with a default dummy element at index 0
    pub fn new() -> Self {
        let mut data = Vec::new();
        data.push(T::default()); // Reserve index 0 with default value
        Self {
            data,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Creates a new RefMap with specified capacity and a default dummy element at index 0
    pub fn with_capacity(capacity: usize) -> Self {
        let mut data = Vec::with_capacity(capacity.max(1));
        data.push(T::default()); // Reserve index 0 with default value
        Self {
            data,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Pushes a new element and returns its reference
    pub fn push(&mut self, item: T) -> R {
        self.data.push(item);
        let index = self.data.len() as crate::ir::RefType - 1;
        // This should always succeed since we start from 1
        R::from(index)
    }

    /// Gets a reference to an element by its ref, panics if not present
    /// This does NOT create missing elements
    pub fn get(&self, r: R) -> &T {
        let index = r.into() as usize;
        assert!(index > 0, "Cannot access reserved index 0");
        self.data.get(index).unwrap_or_else(|| panic!("RefMap entry at index {} does not exist", index))
    }

    /// Gets a mutable reference to an element by its ref, creating default elements as needed.
    /// This WILL create missing elements up to the requested index.
    pub fn get_mut(&mut self, r: R) -> &mut T {
        let index = r.into() as usize;
        assert!(index > 0, "Cannot access reserved index 0");
        
        // Extend with default values if necessary
        while self.data.len() <= index {
            self.data.push(T::default());
        }
        
        &mut self.data[index]
    }

    /// Gets the number of user elements (excluding the dummy at index 0)
    pub fn len(&self) -> usize {
        self.data.len() - 1
    }

    /// Returns true if there are no user elements
    pub fn is_empty(&self) -> bool {
        self.data.len() <= 1
    }

    /// Clears all user elements, keeping only the default dummy at index 0
    pub fn clear(&mut self) {
        self.data.clear();
        self.data.push(T::default());
    }
}

impl<R, T> Default for RefMap<R, T>
where
    R: Copy + From<crate::ir::RefType> + Into<crate::ir::RefType>,
    T: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BlockRef, VarRef};

    #[derive(Debug, Default, Clone, PartialEq)]
    struct TestItem {
        value: i32,
    }

    impl TestItem {
        fn new(value: i32) -> Self {
            Self { value }
        }
    }

    #[test]
    fn test_refmap_basic_operations() {
        let mut map: RefMap<BlockRef, TestItem> = RefMap::new();
        
        // Should start empty (index 0 doesn't count)
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
        
        // Push some items
        let ref1 = map.push(TestItem::new(42));
        let ref2 = map.push(TestItem::new(24));
        
        assert_eq!(map.len(), 2);
        assert!(!map.is_empty());
        
        // Should be able to get the items
        assert_eq!(map.get(ref1).value, 42);
        assert_eq!(map.get(ref2).value, 24);
    }

    #[test]
    fn test_refmap_get_mut_creates() {
        let mut map: RefMap<VarRef, TestItem> = RefMap::new();
        
        // Create a reference that doesn't exist yet
        let var_ref = VarRef::new(5).unwrap();
        
        // get_mut should create all missing elements up to index 5
        let item = map.get_mut(var_ref);
        item.value = 100;
        
        assert_eq!(map.len(), 5); // Should have created 5 elements
        assert_eq!(map.get(var_ref).value, 100);
        
        // Earlier indices should exist with default values
        let var_ref1 = VarRef::new(1).unwrap();
        assert_eq!(map.get(var_ref1).value, 0); // Default value
    }

    #[test]
    fn test_refmap_clear() {
        let mut map: RefMap<BlockRef, TestItem> = RefMap::new();
        
        map.push(TestItem::new(1));
        map.push(TestItem::new(2));
        assert_eq!(map.len(), 2);
        
        map.clear();
        assert_eq!(map.len(), 0);
        assert!(map.is_empty());
        
        // Should be able to add new items after clear
        let ref1 = map.push(TestItem::new(42));
        assert_eq!(map.get(ref1).value, 42);
    }

    #[test]
    fn test_refmap_with_capacity() {
        let map: RefMap<VarRef, TestItem> = RefMap::with_capacity(10);
        assert!(map.is_empty());
        assert_eq!(map.len(), 0);
        // Internal capacity should be at least 10 (though we can't easily test this)
    }
}
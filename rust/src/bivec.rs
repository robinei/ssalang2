use std::ops::{Index, IndexMut};

/// A vector-like data structure that supports negative indices and can grow in both directions.
/// Positive indices work like a normal Vec, negative indices grow backwards from -1.
#[derive(Debug, Clone)]
pub struct BiVec<T> {
    /// Storage for positive indices (0, 1, 2, ...)
    positive: Vec<T>,
    /// Storage for negative indices (-1, -2, -3, ...) in reverse order
    /// negative[0] = element at index -1
    /// negative[1] = element at index -2, etc.
    negative: Vec<T>,
}

impl<T> BiVec<T> {
    /// Creates a new empty BiVec
    pub fn new() -> Self {
        Self {
            positive: Vec::new(),
            negative: Vec::new(),
        }
    }

    /// Creates a new BiVec with the specified capacities for positive and negative indices
    pub fn with_capacity(positive_cap: usize, negative_cap: usize) -> Self {
        Self {
            positive: Vec::with_capacity(positive_cap),
            negative: Vec::with_capacity(negative_cap),
        }
    }

    /// Returns the number of elements in the BiVec
    pub fn len(&self) -> usize {
        self.positive.len() + self.negative.len()
    }

    /// Returns true if the BiVec is empty
    pub fn is_empty(&self) -> bool {
        self.positive.is_empty() && self.negative.is_empty()
    }

    /// Appends an element to the back (positive direction)
    pub fn push(&mut self, value: T) {
        self.positive.push(value);
    }

    /// Prepends an element to the front (negative direction)
    pub fn push_front(&mut self, value: T) {
        self.negative.push(value);
    }

    /// Returns the count of positive elements (indices 0, 1, 2, ...)
    pub fn positive_count(&self) -> usize {
        self.positive.len()
    }

    /// Returns the count of negative elements (indices -1, -2, -3, ...)
    pub fn negative_count(&self) -> usize {
        self.negative.len()
    }

    /// Returns an iterator over all elements with their indices
    pub fn iter_with_indices(&self) -> BiVecIterator<T> {
        BiVecIterator::new(&self.negative, &self.positive)
    }


    /// Gets an element at the given index, returns None if out of bounds
    pub fn get(&self, index: isize) -> Option<&T> {
        if index >= 0 {
            self.positive.get(index as usize)
        } else {
            let neg_index = (-index - 1) as usize;
            self.negative.get(neg_index)
        }
    }

    /// Gets a mutable reference to an element at the given index, returns None if out of bounds
    pub fn get_mut(&mut self, index: isize) -> Option<&mut T> {
        if index >= 0 {
            self.positive.get_mut(index as usize)
        } else {
            let neg_index = (-index - 1) as usize;
            self.negative.get_mut(neg_index)
        }
    }

    /// Sets the value at the given index, growing the BiVec if necessary
    pub fn set(&mut self, index: isize, value: T) {
        if index >= 0 {
            let pos_index = index as usize;
            if pos_index >= self.positive.len() {
                self.positive.resize_with(pos_index + 1, || panic!("Cannot resize without default value"));
            }
            self.positive[pos_index] = value;
        } else {
            let neg_index = (-index - 1) as usize;
            if neg_index >= self.negative.len() {
                self.negative.resize_with(neg_index + 1, || panic!("Cannot resize without default value"));
            }
            self.negative[neg_index] = value;
        }
    }
}

impl<T: Default> BiVec<T> {
    /// Sets the value at the given index, growing the BiVec with default values if necessary
    pub fn set_with_default(&mut self, index: isize, value: T) {
        if index >= 0 {
            let pos_index = index as usize;
            if pos_index >= self.positive.len() {
                self.positive.resize_with(pos_index + 1, T::default);
            }
            self.positive[pos_index] = value;
        } else {
            let neg_index = (-index - 1) as usize;
            if neg_index >= self.negative.len() {
                self.negative.resize_with(neg_index + 1, T::default);
            }
            self.negative[neg_index] = value;
        }
    }
}

impl<T> Index<isize> for BiVec<T> {
    type Output = T;

    fn index(&self, index: isize) -> &Self::Output {
        self.get(index).expect("Index out of bounds")
    }
}

impl<T> IndexMut<isize> for BiVec<T> {
    fn index_mut(&mut self, index: isize) -> &mut Self::Output {
        self.get_mut(index).expect("Index out of bounds")
    }
}

impl<T> Default for BiVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator for BiVec that yields (index, &T) pairs
pub struct BiVecIterator<'a, T> {
    negative: &'a [T],
    positive: &'a [T],
    current_index: isize,
    end_index: isize,
}

impl<'a, T> BiVecIterator<'a, T> {
    fn new(negative: &'a [T], positive: &'a [T]) -> Self {
        let start_index = -(negative.len() as isize);
        let end_index = positive.len() as isize;
        Self {
            negative,
            positive,
            current_index: start_index,
            end_index,
        }
    }
}

impl<'a, T> Iterator for BiVecIterator<'a, T> {
    type Item = (isize, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index >= self.end_index {
            return None;
        }
        
        let index = self.current_index;
        let value = if index < 0 {
            // Negative index: convert to array index
            let array_index = (-index - 1) as usize;
            &self.negative[array_index]
        } else {
            // Positive index: direct array index
            &self.positive[index as usize]
        };
        
        self.current_index += 1;
        Some((index, value))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_operations() {
        let mut bv = BiVec::new();
        
        // Test push (positive direction)
        bv.push(10);
        bv.push(20);
        assert_eq!(bv[0], 10);
        assert_eq!(bv[1], 20);
        
        // Test push_front (negative direction)
        bv.push_front(5);
        bv.push_front(1);
        assert_eq!(bv[-1], 5);
        assert_eq!(bv[-2], 1);
    }

    #[test]
    fn test_indexing() {
        let mut bv = BiVec::new();
        bv.push(100);
        bv.push(200);
        bv.push_front(50);
        bv.push_front(25);
        
        // Test positive indices
        assert_eq!(bv[0], 100);
        assert_eq!(bv[1], 200);
        
        // Test negative indices
        assert_eq!(bv[-1], 50);
        assert_eq!(bv[-2], 25);
        
        // Test mutable indexing
        bv[0] = 999;
        bv[-1] = 888;
        assert_eq!(bv[0], 999);
        assert_eq!(bv[-1], 888);
    }

    #[test]
    fn test_get() {
        let mut bv = BiVec::new();
        bv.push(42);
        bv.push_front(24);
        
        assert_eq!(bv.get(0), Some(&42));
        assert_eq!(bv.get(-1), Some(&24));
        assert_eq!(bv.get(10), None);
        assert_eq!(bv.get(-10), None);
    }

    #[test]
    fn test_counts() {
        let mut bv = BiVec::new();
        bv.push(1);
        bv.push(2);
        bv.push_front(3);
        bv.push_front(4);
        
        assert_eq!(bv.positive_count(), 2);
        assert_eq!(bv.negative_count(), 2);
        assert_eq!(bv.len(), 4);
    }

    #[test]
    fn test_iter_with_indices() {
        let mut bv = BiVec::new();
        bv.push(10);      // index 0
        bv.push(20);      // index 1
        bv.push_front(5); // index -1
        bv.push_front(1); // index -2
        
        let items: Vec<_> = bv.iter_with_indices().collect();
        assert_eq!(items, vec![
            (-2, &1),
            (-1, &5),
            (0, &10),
            (1, &20),
        ]);
    }

    #[test]
    fn test_set_with_default() {
        let mut bv: BiVec<i32> = BiVec::new();
        
        // Set at positive index that requires growing
        bv.set_with_default(3, 42);
        assert_eq!(bv[0], 0); // default value
        assert_eq!(bv[1], 0); // default value
        assert_eq!(bv[2], 0); // default value
        assert_eq!(bv[3], 42);
        
        // Set at negative index that requires growing
        bv.set_with_default(-3, 99);
        assert_eq!(bv[-1], 0); // default value
        assert_eq!(bv[-2], 0); // default value
        assert_eq!(bv[-3], 99);
    }

}
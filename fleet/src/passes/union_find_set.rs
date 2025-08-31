use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

#[derive(Debug, Default)]
pub struct UnionFindSetPtr<T>(u64, PhantomData<T>);

impl<T> PartialEq for UnionFindSetPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for UnionFindSetPtr<T> {}

impl<T> Hash for UnionFindSetPtr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> Copy for UnionFindSetPtr<T> {}

impl<T> Clone for UnionFindSetPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone, Debug)]
pub struct UnionFindSet<T> {
    parent: HashMap<UnionFindSetPtr<T>, UnionFindSetPtr<T>>,
    pub data: HashMap<UnionFindSetPtr<T>, T>,
    highest_ptr: UnionFindSetPtr<T>,
}

impl<T> Default for UnionFindSet<T> {
    fn default() -> Self {
        Self {
            parent: HashMap::default(),
            data: HashMap::default(),
            highest_ptr: UnionFindSetPtr(0, PhantomData),
        }
    }
}

pub enum UnionFindSetMergeResult<T> {
    Merged(T),
    NotMerged { a: T, b: T },
}

impl<T> UnionFindSet<T> {
    pub fn insert_set(&mut self, initial_data: T) -> UnionFindSetPtr<T> {
        let current_ptr = self.highest_ptr;
        self.highest_ptr.0 += 1;
        self.data.insert(current_ptr, initial_data);
        self.parent.insert(current_ptr, current_ptr);
        current_ptr
    }
    /// Creates a new, detached, set with the representative being a copy of the one for [`ptr`]
    pub fn detach(&mut self, ptr: UnionFindSetPtr<T>) -> UnionFindSetPtr<T>
    where
        T: Clone,
    {
        self.insert_set(self.get(ptr).clone())
    }
    pub fn get_repr(&self, mut ptr: UnionFindSetPtr<T>) -> UnionFindSetPtr<T> {
        while self.parent.get(&ptr) != Some(&ptr) {
            ptr = *self.parent.get(&ptr).unwrap();
        }
        ptr
    }
    pub fn get(&self, ptr: UnionFindSetPtr<T>) -> &T {
        self.data.get(&self.get_repr(ptr)).unwrap()
    }
    pub fn get_mut(&mut self, ptr: UnionFindSetPtr<T>) -> &mut T {
        self.data.get_mut(&self.get_repr(ptr)).unwrap()
    }
    pub fn try_merge<Combiner: FnOnce(T, T, &mut Self) -> UnionFindSetMergeResult<T>>(
        &mut self,
        a: UnionFindSetPtr<T>,
        b: UnionFindSetPtr<T>,
        combiner: Combiner,
    ) -> bool {
        let a_repr = self.get_repr(a);
        let b_repr = self.get_repr(b);
        if a_repr == b_repr {
            return true;
        }

        let a_data = self.data.remove(&a_repr).unwrap();
        let b_data = self.data.remove(&b_repr).unwrap();
        match combiner(a_data, b_data, self) {
            UnionFindSetMergeResult::Merged(combined_data) => {
                *self.parent.get_mut(&b_repr).unwrap() = a_repr;
                self.data.insert(a_repr, combined_data);
                true
            }
            UnionFindSetMergeResult::NotMerged {
                a: a_data,
                b: b_data,
            } => {
                self.data.insert(a_repr, a_data);
                self.data.insert(b_repr, b_data);
                false
            }
        }
    }
}

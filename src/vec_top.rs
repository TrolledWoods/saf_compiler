use std::ops::{ Deref, DerefMut };

#[derive(Debug)]
pub struct VecTop<'a, T> {
    vec: &'a mut Vec<T>,
    stack_begin: usize,
}

impl<'a, T> VecTop<'a, T> {
    pub fn at_top(source: &'a mut Vec<T>) -> Self {
        VecTop {
            vec: source,
            stack_begin: 0,
        }
    }

    pub fn top<'b>(&'b mut self) -> VecTop<'b, T> {
        VecTop {
            stack_begin: self.vec.len(),
            vec: &mut self.vec,
        }
    }

    pub fn temp_clone<'b>(&'b mut self) -> VecTop<'b, T> {
        VecTop {
            stack_begin: self.stack_begin,
            vec: &mut self.vec,
        }
    }

    pub fn index_of<Q: PartialEq<T>>(&self, wanted: &Q)
            -> Index {
        let mut index = None;

        for (i, member) in 
            self.full_slice()
                .iter()
                .enumerate()
                .rev() {
            if wanted == member {
                index = Some(i);
                break;
            }
        }

        match index {
            Some(index) => if index >= self.stack_begin {
                Index::Inside(index - self.stack_begin)
            } else {
                Index::InsideFull(index)
            },
            None => Index::NotInside,
        }
    }

    pub fn push(&mut self, item: T) {
        self.vec.push(item);
    }

    /// # Panics
    /// If you pop off so many elements that
    /// it goes beyond the stack beginning
    pub fn pop(&mut self) -> Option<T> {
        let item = self.vec.pop();

        assert!(self.vec.len() >= self.stack_begin);
        item
    }

    pub fn full_slice(&self) -> &[T] {
        &*self.vec
    }

    pub fn full_slice_mut(&mut self) -> &mut [T] {
        &mut *self.vec
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Index {
    /// Not inside the vector
    NotInside,
    /// Inside the VecTop
    Inside(usize),
    /// Not inside the VecTop,
    /// but inside the underlying vector
    InsideFull(usize),
}

impl<T> Deref for VecTop<'_, T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.vec[self.stack_begin..]
    }
}

impl<T> DerefMut for VecTop<'_, T> {
    fn deref_mut(&mut self) -> &mut [T] {
        &mut self.vec[self.stack_begin..]
    }
}

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

    pub fn index_of_by(
        &self, 
        mut comparison: impl FnMut(&T) -> bool,
    ) -> Index<T> {
        let mut index = None;

        for (i, member) in 
            self.full_slice()
                .iter()
                .enumerate()
                .rev() {
            if comparison(member) {
                index = Some((member, i));
                break;
            }
        }

        match index {
            Some((member, index)) => if index >= self.stack_begin {
                Index::Inside(index - self.stack_begin, member)
            } else {
                Index::InsideFull(index, member)
            },
            None => Index::NotInside,
        }
    }

    pub fn index_of<Q: PartialEq<T>>(
        &self, 
        wanted: &Q
    ) -> Index<T> {
        self.index_of_by(|member| wanted == member)
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
pub enum Index<'a, T> {
    /// Not inside the vector
    NotInside,
    /// Inside the VecTop
    Inside(usize, &'a T),
    /// Not inside the VecTop,
    /// but inside the underlying vector
    InsideFull(usize, &'a T),
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

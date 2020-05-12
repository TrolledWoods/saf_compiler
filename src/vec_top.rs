use std::ops::{ Deref, DerefMut };

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

    pub fn full_slice(&self) -> &[T] {
        &*self.vec
    }

    pub fn full_slice_mut(&mut self) -> &mut [T] {
        &mut *self.vec
    }
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

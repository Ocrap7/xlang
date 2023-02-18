use std::{
    ops::Deref,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

pub struct Rf<T: ?Sized>(pub Arc<RwLock<T>>);

impl<T: ?Sized> Clone for Rf<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Rf<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Rf").field(&self.borrow()).finish()
    }
}

impl<T> Deref for Rf<T> {
    type Target = RwLock<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Rf<T> {
    pub fn new(t: T) -> Rf<T> {
        Rf(Arc::new(RwLock::new(t)))
    }

    pub fn borrow_mut(&self) -> RwLockWriteGuard<'_, T> {
        self.write().unwrap()
    }

    pub fn borrow(&self) -> RwLockReadGuard<'_, T> {
        self.read().unwrap()
        // self.().unwrap()
    }
}

impl<T> From<T> for Rf<T> {
    fn from(t: T) -> Self {
        Rf::new(t)
    }
}

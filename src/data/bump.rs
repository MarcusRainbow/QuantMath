/// Interface that bumps must support
pub trait Bump<T> {

    /// Applies the bump to the old value, returning the new value
    fn apply(&self, old_value: T) -> T;
}

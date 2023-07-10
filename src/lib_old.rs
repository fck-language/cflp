/// Parser error struct
///
/// This is the error type returned by the [`Parser`] trait when an error is found. The types are
/// different to allow the iterator values to contain additional data that does need to be compared
/// such as position within a file. When this is returned it means that the `found` value is not
/// equal to the `expected` value.
pub struct Error<F, E> {
    /// Value that was found in the input iterator
    pub found: Option<F>,
    /// Value that was expected in the input iterator
    pub expected: E,
}

impl<F: Debug, E: Debug> Debug for Error<F, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "cflp::Error {{ found: {:?}, expected: {:?} }}",
            self.found, self.expected
        )
    }
}

pub use cflp_macros::{rule, rule_no_types, Parser};
use std::fmt::{Debug, Formatter};

/// Parser trait. Implemented by all generated structs from the [`rule!`] macro
///
/// # Generics
/// * `I` - Type in input iterator. Token with class and other data
/// * `C` - Type to compare to. Token class
pub trait Parser<I: PartialEq<C>, C> {
    /// Parse an AST from a given input. Returns `Ok(Self)` if the AST is found and
    /// `Err(cflp::Error<I, C>)` if the AST was not found
    fn parse<T: Iterator<Item = I> + Clone>(src: &mut T) -> Result<Self, Error<I, C>>
    where
        Self: Sized;
}

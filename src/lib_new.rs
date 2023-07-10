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
	pub expected: E
}

/// Optional wrapper around a node that contains additional node data for the start and end of the match
///
/// This is wrapped around a resultant node not a match. Matches use a [`MatchWrapper`]
pub struct NodeWrapper<T, D> {
	/// Node value
	pub node: T,
	/// Data for the first matched token
	pub start: D,
	/// Data for the last matched token. This is generated with `<T as NodeData<D>>::
	pub end: D,
}

/// Optional wrapper around a non-calling match that contains additional data
///
/// This is wrapped around a resultant node not a match. Matches use a [`MatchWrapper`]
pub struct MatchWrapper<T, D> {
	/// Node value
	pub node: T,
	/// Data for the first matched
	pub data: D,
}

impl<F: Debug, E: Debug> Debug for Error<F, E> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "cflp::Error {{ found: {:?}, expected: {:?} }}", self.found, self.expected)
	}
}

use std::fmt::{Debug, Formatter};
pub use cflp_macros::{rule, rule_no_types, parser};

/// Parser trait. Implemented by all generated structs from the [`rule!`] macro
///
/// # Generics
/// * `I` - Type in input iterator. Token with class and other data
/// * `C` - Type to compare to. Token class
/// * `R` - Ok returned type. This will either be `Self` or [`NodeWrapper<Self, D>`]
pub trait Parser<I: PartialEq<C>, C, R> {
	/// Parse an AST from a given input. Returns `Ok(Self)` if the AST is found and
	/// `Err(cflp::Error<I, C>)` if the AST was not found
	fn parse<T: Iterator<Item=I> + Clone>(src: &mut T) -> Result<R, Error<I, C>> where Self: Sized;
}

/// Node data trait required for the [`NodeWrapper`] struct
///
/// If the macro you're using will return a [`NodeWrapper<T, D>`] instead of `T`, your iterator item
/// type must implement this trait.
pub trait NodeData<D: Sized> where Self: Sized {
	/// Returns the data saved into [`NodeWrapper::first`]
	fn first(&self) -> D;
	/// Returns the data saved into [`NodeWrapper::last`]
	fn last(&self) -> D;
	#[inline]
	fn get_data(&self) -> (D, D) {
		(<Self as NodeData<D>>::first(self), <Self as NodeData<D>>::last(self))
	}
}

impl<T: Sized, S: Sized + Clone> NodeData<S> for NodeWrapper<T, S> {
	#[inline]
	fn first(&self) -> S { self.start.clone() }
	#[inline]
	fn last(&self) -> S { self.end.clone() }
}

impl<T: Sized, S: Sized + Clone> NodeData<S> for MatchWrapper<T, (S, S)> {
	#[inline]
	fn first(&self) -> S { self.data.0.clone() }
	#[inline]
	fn last(&self) -> S { self.data.1.clone() }
}

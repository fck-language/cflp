#![doc=include_str!("../README.md")]
#![cfg_attr(
    docs,
    feature(doc_auto_cfg),
    deny(rustdoc::broken_intra_doc_links, missing_docs)
)]
#![cfg_attr(
    not(docs),
    warn(rustdoc::broken_intra_doc_links, missing_docs)
)]
use std::fmt::{Debug, Formatter};
/// # Parser derive macro
///
/// Derive macro used for the [`Parser`] trait. See the book for how to use this macro
pub use cflp_macros::{Parser, PartialEqRef};

/// # Parser error struct
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

impl<F: Debug, E: Debug> Debug for Error<F, E> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "cflp::Error {{ found: {:?}, expected: {:?} }}", self.found, self.expected)
	}
}

/// # Parser trait
///
/// Geneal trait for parsing a stream of input tokens. This can be derived using
/// [`#[derive(Parser)]`](cflp_macros::Parser)
///
/// ## Generics
/// * `I` - Type in input iterator. Token with class and other data
/// * `C` - Type to compare to. Token class
/// * `R` - Ok returned type. This will either be `Self` or [`NodeWrapper<Self, D>`]
pub trait Parser<I: PartialEq<C>, C: PartialEq<C>, R = Self> {
	/// Parse an AST from a given input. Returns `Ok(Self)` (optionally wrapped) if the AST is
	/// found and `Err(cflp::Error<I, C>)` if the AST was not found
	fn parse<T: Iterator<Item=I> + Clone>(src: &mut T) -> Result<R, Error<I, C>> where Self: Sized;
}

/// # Node wrapper
///
/// This struct is used to wrap a node returned from parsing. It contains the
/// [wrapped node](Self::node), and [starting](Self::start) and [ending](Self::end) data from
/// the [`NodeData`] trait
#[derive(Clone)]
pub struct NodeWrapper<T: Sized, D: Sized + Clone + Default> {
	/// Wrapped node
	pub node: T,
	/// Starting data
	pub start: D,
	/// Ending data
	pub end: D
}

impl<T: Sized + Debug, D: Sized + Debug + Default + Clone> Debug for NodeWrapper<T, D> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "NodeWrapper {{ node: {:?}, start: {:?}, end: {:?} }}", self.node, self.start, self.end)
	}
}

impl<T: Sized, D: Sized + Clone + Default> NodeData<D> for NodeWrapper<T, D> {
	#[inline] fn start(&self) -> D { self.start.clone() }
	#[inline] fn end(&self) -> D { self.end.clone() }
}

/// # Node data
///
/// Access starting and ending node data for any node. Required if you want to have a wrapped parser
pub trait NodeData<T: Sized, D: Sized = ()> {
	/// Get the starting node data
	fn start(&self) -> T;
	/// Get the ending node data
	fn end(&self) -> T;
}

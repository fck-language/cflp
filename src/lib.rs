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
use std::iter::Peekable;
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
pub struct Error<F, E, B> {
	/// Value that was found in the input iterator
	pub found: Option<F>,
	/// Value that was expected in the input iterator
	pub expected: E,
	/// Scope being parsed when the error was found. This is pushed too for each parsing level.
	/// For example, if an you got an error from parsing `S1` which was called from `S2`, `scope`
	/// should be `vec![S1::scope(), S2::scope()]`
	///
	/// See [`Scope::scope`](cflp::Scope<B>::scope) for details on how the scope works
	pub scope: Vec<B>
}

impl<F, E, B> Error<F, E, B> {
	/// Append a scope to a scoped error. Intended for use in backtracking
	pub fn push_scope(&mut self, new_scope: B) {
		self.scope.push(new_scope)
	}

	/// Owned version of [`push_scope`](Error::push_scope) that returns self
	pub fn push_scope_self(mut self, new_scope: B) -> Self { self.scope.push(new_scope); self }
}

impl<F: Debug, E: Debug, B: Debug> Debug for Error<F, E, B> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("cflp::Error")
			.field("found", &self.found)
			.field("expected", &self.expected)
			.field("scope", &self.scope)
			.finish()
	}
}

/// # Error scope
///
/// Provides the function [`scope`](Scope::scope) to provide information on where an error came from
pub trait Scope<B> {
	/// Give the scope for `Self`
	///
	/// This is used for error reporting. For example:
	/// ```
	/// # struct Atom;
	/// # struct Op;
	/// # use cflp::Scope;
	/// struct Expr {
	/// 	left: Atom,
	/// 	op: Op,
	/// 	right: Atom
	/// }
	///
	/// enum NodeType {
	/// 	Expr,
	/// 	Atom,
	/// 	Op
	/// }
	///
	/// impl Scope<NodeType> for Expr {
	/// 	fn scope() -> NodeType {
	/// 		NodeType::Expr
	/// 	}
	/// }
	/// ```
	///
	/// This would mean that if an an `Expr` was being parsed and found an error, the error you'd
	/// get back would tell you that it came from the scope `NodeType::Expr`
	fn scope() -> B;
}

/// # Parser trait
///
/// General trait for parsing a stream of input tokens. This can be derived using
/// [`#[derive(Parser)]`](cflp_macros::Parser)
///
/// ## Generics
/// * `I` - Type in input iterator. Token with class and other data
/// * `C` - Type to compare to. Token class
/// * `B` - Scope type. Type used to specify the scope for errors
/// * `R` - Ok returned type. This will either be `Self` or [`NodeWrapper<Self, D>`]
pub trait Parser<I, C: PartialEq<C>, B, R = Self> where Self: Scope<B> {
	/// Parse an AST from a given input. Returns `Ok(Self)` (optionally wrapped) if the AST is
	/// found and `Err(cflp::Error<I, C>)` if the AST was not found
	fn parse<T: Iterator<Item=I> + Clone>(src: &mut Peekable<T>) -> Result<R, Error<I, C, B>> where Self: Sized {
		<Self as Parser<I, C, B, R>>::parse_with_recursion(src, true)
	}
	
	/// **THIS SHOULD ONLY BE CALLED IF YOU KNOW THE RISKS OF ZERO-CONSUMPTION BRANCHING. USE [`parse`](Parser::parse) OTHERWISE**
	///
	/// Performs [`parse`](Parser::parse) with restrictions on zero-consumption branching.
	fn parse_with_recursion<T: Iterator<Item=I> + Clone>(src: &mut Peekable<T>, recurse: bool) -> Result<R, Error<I, C, B>> where Self: Sized;
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
		f.debug_struct("cflp::NodeWrapper")
			.field("node", &self.node)
			.field("start", &self.start)
			.field("end", &self.end)
			.finish()
	}
}

impl<T: Sized + PartialEq, D: Sized + PartialEq + Default + Clone> PartialEq for NodeWrapper<T, D> {
	fn eq(&self, other: &Self) -> bool {
		self.node == other.node && self.start == other.start && self.end == other.end
	}
}

impl<T: Sized, D: Sized + Clone + Default> NodeData<D> for NodeWrapper<T, D> {
	#[inline] fn start(&self) -> D { self.start.clone() }
	#[inline] fn end(&self) -> D { self.end.clone() }
}

/// # Node data
///
/// Access starting and ending node data for any node. Required if you want to have a wrapped parser
pub trait NodeData<T: Sized> {
	/// Get the starting node data
	fn start(&self) -> T;
	/// Get the ending node data
	fn end(&self) -> T;
}

/// Simple macro to unwrap an expression that returns a `Result`, appending a scope to an error or
/// returning the unwrapped value if it was ok
#[macro_export]
macro_rules! scoped_unwrap {
    ($e:expr) => {
		match $e {
			Ok(t) => t,
			Err(mut e) => {
				e.push_scope(<Self as cflp::Scope<_>>::scope());
				return Err(e)
			}
		}
	};
}

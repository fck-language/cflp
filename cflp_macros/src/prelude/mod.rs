pub(crate) mod no_types;
mod impls;

use proc_macro::TokenStream;
use syn::{Ident, Expr, Type, Visibility};

/// Single or group of values
#[derive(Clone)]
pub(crate) enum Value {
	/// Single value
	Single(Expr),
	/// Call to another rule
	Call(Ident),
	/// Saved value
	Save(SaveType),
	/// Group of values
	Group(Vec<Group>, bool),
}

#[derive(Clone)]
pub(crate) enum SaveType {
	Literal(Expr),
	Call(Ident),
	Unwrapping(Expr, Vec<SaveUnwrapType>),
}

#[derive(Clone)]
pub(crate) enum SaveUnwrapType {
	Use(Type),
	Ignore,
}

/// Closures on a single or group of values
#[derive(Clone)]
pub(crate) enum Group {
	/// Literal group ()
	Literal(Value, bool),
	/// Kleene closure ()*
	Kleene(Value, bool),
	/// Positive closure ()+
	Positive(Value, bool),
	/// Optional value ()?
	Option(Value, bool),
}

/// Macro rule
#[derive(Clone)]
pub(crate) struct MacroInner {
	/// Metadata
	pub meta: Meta,
	/// Rules
	pub rules: Rules,
}

/// Metadata for the macro
#[derive(Clone)]
pub(crate) struct Meta {
	/// CFG parsing function visibility
	pub struct_vis: Visibility,
	/// Input token type
	pub tok_type: Type,
	/// Comparison token type
	pub comp_type: Type,
	/// Token to comp function
	pub map_fn: Expr,
	/// Traits to derive for generated structs
	pub derived_traits: TokenStream,
}

/// Macro rules
#[derive(Clone)]
pub(crate) struct Rules(pub Vec<Rule>);

/// Macro rule
#[derive(Clone)]
pub(crate) struct Rule {
	/// Rule name
	pub name: Ident,
	/// Rule group
	pub inner: RuleInnerEnum,
}

/// Enum over a rule inner
#[derive(Clone)]
pub(crate) enum RuleInnerEnum {
	/// Single rule option (struct type)
	Single(RuleInner),
	/// Multiple rule options (enum type)
	Multiple(Vec<RuleInner>),
}

/// Rule inner. Named wrapper around a `Vec<Group>`
#[derive(Clone)]
pub(crate) struct RuleInner {
	pub(crate) name: Option<Ident>,
	pub(crate) inner: Vec<Group>,
}

/// Lifetime enum. Used to determine where errors should return to and if a successful result should
/// be wrapped in a `Result`
#[derive(Copy, Clone)]
pub(crate) enum ReturnType {
	Function,
	Lifetime(u8, bool),
}

use syn::{Expr, ExprClosure, Type};
use crate::prelude::{RuleInnerEnum, Rules};

/// Macro rule with no type generation
#[derive(Clone)]
pub(crate) struct MacroInnerNoGen {
	/// Metadata
	pub meta: MetaNoGen,
	/// Rules
	pub rules: Rules
}

/// # Derive options
///
/// Options parsed from the `#[parser(...)]` helper attribute
#[derive(Clone)]
pub(crate) struct Meta {
	/// Input token type
	pub tok_type: Type,
	/// Comparison token type
	pub cmp_type: Type,
	/// Token to token data map
	pub map_fn: ExprClosure,
	/// Is the result wrapped
	pub wrapped: WrapType
}

/// # Return wrap type
///
/// If the return is wrapped, its wrapped in
#[derive(Clone)]
pub(crate) enum WrapType {
	Wrapped(Type),
	None
}

/// # Derive macro options
///
/// Meta from the `#[parser(...)]` helper attribute on an item and enum variants
#[derive(Clone)]
pub(crate) struct Options {
	/// Metadata for the macro call
	pub meta: Meta,
	/// Rules
	pub rule: RuleInnerEnum
}

/// Metadata for the macro with no type generation
#[derive(Clone)]
pub(crate) struct MetaNoGen {
	/// Input token type
	pub tok_type: Type,
	/// Comparison token type
	pub comp_type: Type,
	/// Token to comp function
	pub map_fn: Expr,
	pub wrapped: WrapType
}

use syn::{Expr, Type};
use crate::prelude::{RuleInnerEnum, Rules};

/// Macro rule with no type generation
#[derive(Clone)]
pub(crate) struct MacroInnerNoGen {
	/// Metadata
	pub meta: MetaNoGen,
	/// Rules
	pub rules: Rules
}

#[derive(Clone)]
pub(crate) struct MacroInnerAttrMeta {
	/// Input token type
	pub tok_type: Type,
	/// Comparison token type
	pub comp_type: Type,
	/// Token to comp function
	pub map_fn: Expr,
}

/// Macro rule with no type generation
#[derive(Clone)]
pub(crate) struct MacroInnerAttr {
	/// Metadata for the macro call
	pub meta: MacroInnerAttrMeta,
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
}

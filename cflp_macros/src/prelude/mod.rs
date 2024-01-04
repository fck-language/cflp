//! This module contains all the types used throughout the codebase

use syn::{Path, Type, Ident, Expr, Pat, PathSegment, WhereClause};

mod impls;
mod parser;

/// # Macro input
///
/// Represents the macro input. Contains the [metadata](Self::meta) and [rules](Self::rules)
#[derive(Clone)]
pub struct MacroInner {
	/// Metadata
	pub meta: Meta,
	/// Rules
	pub rules: Rules,
}

/// # Struct parser attribute intermediate
///
/// This struct is an intermediate representation of [`MacroInner`]. It's parsed from the single
/// `#[parser(...)]` attribute on a struct and turned into a [`MacroInner`]
pub struct StructParserAttribute {
	/// See [`MacroInner::meta`]
	pub meta: Meta,
	/// Matched rules. Converted into [`RuleInner`] later
	pub rule: SplitRule
}

/// # Derive options
///
/// Options parsed from the `#[parser(...)]` helper attribute on the item deriving `Parser`
#[derive(Clone)]
pub struct Meta {
	/// Input token type
	pub tok_type: Type,
	/// Comparison token type
	pub cmp_type: Type,
	/// Scope/backtrace type
	pub scope: Type,
	/// Token to token data map
	pub map: bool,
	/// If the result is wrapped, contains a `Some` of the type with the wrapped data, otherwise
	/// is `None`.
	///
	/// For example, if the result is wrapped with a `usize` (`NodeWrapper<Self, usize>`), this
	/// would be `Some(usize)`
	pub wrapped: Option<Type>,
	/// Alias for `Self::wrapped::is_some`. Trades one byte for a tiny improvement in performance
	///
	/// Let me have my bad decisions
	pub is_wrapped: bool,
	/// Name of the type deriving `Parser`. Should *ONLY* be used in the `impl` line, not the body of a function since
	/// it can include generic constraints
	pub _self: PathSegment,
	/// Optional `where` statement for the type
	pub where_stms: Option<WhereClause>
}

/// # Rules
///
/// All the match rules for the type deriving the `Parser` trait
#[derive(Clone)]
pub enum Rules {
	/// Single rule. Used for structs
	Single(RuleInner),
	/// Single rule. Used for enum variants
	Multiple { first: RuleInner, rem: Vec<RuleInner> },
}

/// # Rule match
///
/// Representation of a rule
#[derive(Clone)]
pub struct RuleInner {
	/// Item the rule matches.
	/// - For a struct, this is the name of the struct as a `Path` e.g. `MyStruct`
	/// - For an enum, this is the enum name, followed by the variant name e.g. `MyEnum::MyVariant`
	pub name: Path,
	/// The rule match
	pub inner: RuleInnerMatch,
}

/// # Rule wrapper
///
/// Wrapper around a rule to save if the type (struct or enum variant) is named or unnamed
#[derive(Clone)]
pub enum RuleInnerMatch {
	/// Named type such as for `MyStruct { a: 1, b: 2 }`
	Named(Vec<Ident>, SplitRule),
	/// Unnamed type such as for `MyStruct(1, 2)`
	Unnamed(SplitRule),
	/// Unit struct such as `struct Example;`
	Unit(SplitRule)
}

/// # Seperated rule
///
/// This enum is used to store seperated rules. It's either a [single group](Self::Single) or
/// [more than one rule](Self::Other) (rules matching nothing are not allowed). This is useful for
/// generating wrapped parser impls that require node data
///
/// *NOTE*: PNM means _possible no match_ and is either a [`Group::Kleene`] or [`Group::Option`]
/// group. These groups can match no tokens
#[derive(Clone)]
#[allow(non_snake_case)]
pub enum SplitRule {
	/// Only PNM groups
	AllPNM(Vec<Group>),
	/// A singular non-PNM with any number of PNM groups before and after
	Single {
		pre_PNM: Vec<Group>,
		group: Box<Group>,
		post_PNM: Vec<Group>
	},
	/// Rule with more that one base group in it. Note that [`middle`](Self::Other::middle),
	/// [`pre_PNM`](Self::Other::pre_PNM), and [`post_PNM`](Self::Other::post_PNM) are not
	/// guaranteed to contain elements
	Other {
		pre_PNM: Vec<Group>,
		start: Box<Group>, middle: Vec<Group>, end: Box<Group>,
		post_PNM: Vec<Group>
	}
}

/// # Singular value
///
/// This is the base type for rules. It contains all the possible single types, and a group type.
#[derive(Clone)]
pub enum Value {
	/// Single value
	Single(Pat),
	/// Call to another rule
	Call(Path),
	/// Saved value. See [`SaveType`]
	Save { group: SaveType, boxed: bool },
	/// Group of values. The `bool` is `true` iff the group contains a [save](Value::Save),
	/// otherwise it's `false`
	Group(SplitRule, bool),
}

/// # Group of values
///
/// This is a group of values under different closures. For each variant, the `bool` represents the
/// same as it does for [`Value::Group`]
#[derive(Clone)]
pub enum Group {
	/// Literal group ()
	Literal(Value, bool),
	/// Kleene closure ()*
	Kleene(Value, bool),
	/// Positive closure ()+
	Positive(Value, bool),
	/// Optional value ()?
	Option(Value, bool),
}

/// # Saved value
///
/// A saved value is a [`Value`] that's saved. In the attribute this is indicated with it being in
/// square brackets. It is either:
/// - [A call to another rule](Self::Call)
/// - [Anything else](Self::Other)
#[derive(Clone)]
pub enum SaveType {
	/// This is a [`Path`](syn::Path) with a `@` prefixing it. This calls the parser impl for the
	/// provided type
	Call(Path),
	/// Anything that's not a call to another rule is interpreted as a [pattern](syn::Pat). This
	/// will normally either be a [literal](syn::Pat::Ident), or a [named type](syn::Pat::Struct)
	/// such as `Foo { a, .. }` or an [un-named type](syn::Pat::TupleStruct) such as `Bar(_, b)`
	Other { pattern: Pat, default: Option<Expr> },
}

/// Lifetime enum. Used to determine where errors should return to and if a successful result should
/// be wrapped in a `Result`
#[derive(Copy, Clone)]
pub enum ReturnType {
	/// Should return out of a function (using `return`)
	Function,
	/// Should break to a lifetime (using `break 'lifetime`)
	Lifetime(u8, bool),
}

/// # Position data saving type
///
/// When deriving an impl for a type that's wrapped in a `NodeWrapper`, we need the first and last
/// matched input token to save data into the `NodeWrapper`s `first` and `last` fields. This enum
/// describes what type of match is currently being done.
#[derive(Copy, Clone, PartialEq)]
pub enum PositionType {
	/// First matched token. Check `start_set: bool` before writing to `start`
	Start,
	/// Last matched token
	End,
	/// First and last matched token. Will be split if applied to a group with more than one item in\
	/// Check `start_set: bool` before writing to `start`
	StartEnd,
}

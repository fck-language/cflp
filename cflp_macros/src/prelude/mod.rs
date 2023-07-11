//! This module contains all the types used throughout the codebase

use syn::{ExprClosure, Path, Type, Ident, Expr, Pat};

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
	/// Token to token data map
	pub map_fn: Option<ExprClosure>,
	/// Is the result wrapped
	pub wrapped: Option<Type>,
	/// Name of the type deriving `Parser`
	pub _self: Ident
}

/// # Rules
///
/// All the match rules for the type deriving the `Parser` trait
#[derive(Clone)]
pub enum Rules {
	/// Single rule. Used for [`struct`]
	Single(RuleInner),
	/// Single rule. Used for [`enum`] variants
	Multiple { first: RuleInner, rem: Vec<RuleInner> },
}

/// # Rule match
///
/// Representation of a rule
#[derive(Clone)]
pub struct RuleInner {
	/// Item the rule matches.
	/// - For a struct, this is the name of the struct as a `Path`.
	/// - For an enum, this is the enum name, followed by the variant name
	pub name: Path,
	/// The rule match
	pub inner: RuleInnerMatch,
}

/// # Rule wrapper
///
/// Wrapper around a rule to save if the type (struct or enum variant) is named or unnamed
#[derive(Clone)]
pub enum RuleInnerMatch {
	/// Named type
	Named(Vec<Ident>, SplitRule),
	/// Unnamed type
	Unnamed(SplitRule)
}

/// # Seperated rule
///
/// This enum is used to store seperated rules. It's either a [single group](Self::Single) or
/// [more than one rule](Self::Other) (rules matching nothing are not allowed). This is useful for
/// generating wrapped parser impls that require node data
#[derive(Clone)]
pub enum SplitRule {
	/// Rule with only one base group in it
	Single(Box<Group>),
	/// Rule with more that one base group in it
	Other { start: Box<Group>, middle: Vec<Group>, end: Box<Group> }
}

/// Single or group of values
#[derive(Clone)]
pub enum Value {
	/// Single value
	Single(Expr),
	/// Call to another rule
	Call(Ident),
	/// Saved value
	Save { group: SaveType, boxed: bool },
	/// Group of values
	Group(SplitRule, bool),
}

/// Closures on a single or group of values
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
	Other(Pat),
}

/// Lifetime enum. Used to determine where errors should return to and if a successful result should
/// be wrapped in a `Result`
#[derive(Copy, Clone)]
pub enum ReturnType {
	Function,
	Lifetime(u8, bool),
}

/// # Position data saving type
///
/// When deriving an impl for a type that's wrapped in a `NodeWrapper`, we need the first and last
/// matched input token to save data into the `NodeWrapper`s `first` and `last` fields. This enum
/// describes what type of match is currently being done.
#[derive(Copy, Clone)]
pub enum PositionType {
	/// First matched token
	Start,
	/// Last matched token
	End,
	/// First and last matched token. Will be split if applied to a group with more than one item in
	StartEnd
}

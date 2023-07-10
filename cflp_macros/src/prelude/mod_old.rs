pub(crate) mod no_types;
mod impls;

use proc_macro::{TokenStream, Delimiter};
use quote::ToTokens;
use syn::{Ident, Expr, Type, Visibility, Path};
use crate::prelude::no_types::WrapType;
use crate::saving::SaveType;
use util::*;

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

pub(crate) struct StructParserAttribute {
	pub meta: Meta,
	pub rule: RuleInner
}

/// # Root rule type
///
/// Derive rule for either a struct or enum
#[derive(Clone)]
pub(crate) enum Rules {
	/// `struct` rule
	Single(RuleInner),
	/// `enum` variant rules (one per variant)
	Multiple(Vec<RuleInner>),
}

/// # Match rule
///
/// This is the representation of matching rules. It has a
#[derive(Clone)]
pub(crate) struct RuleInner {
	pub name: Path,
	pub inner: RuleInnerMatch,
}

/// # Rule wrapper
///
/// Wrapper around a rule to save if the type (struct or enum variant) is named or unnamed
pub(crate) enum RuleInnerMatch {
	/// Named type
	Named(Vec<Ident>, Vec<Group>),
	/// Unnamed type
	Unnamed(Vec<Group>)
}

/// Wrapped around a [`RuleInner`] to allow parsing to use `parenthesized!`
pub(crate) struct ParenthesizedRuleInner(pub RuleInner);

/// Lifetime enum. Used to determine where errors should return to and if a successful result should
/// be wrapped in a `Result`
#[derive(Copy, Clone)]
pub(crate) enum ReturnType {
	Function,
	Lifetime(u8, bool),
}

/// Generates the tokens to call another rule
/// ```rust
/// # trait Parser<A, B, C> { fn parse(e: A) -> Self; }
/// # fn doc<'a>(src: &'a IterItem) {
/// <MyStruct as Parser<&'a IterItem, CompItem, WrappedOrNotType<Self, D>>>::parse(src)
/// # }
/// ```
pub(crate) fn rule_call_tokens(name: &str, wrapped: bool, iter_type: Type, comp_type: Type) -> TokenStream {
	let mut out = TokenStream::from_iter(vec![punc!('<'), ident!(name), ident!("as"), ident!("Parser"), punc!('<'), punc!('&'), puncj!('\''), ident!("a")]);
	out.extend(TokenStream::from(iter_type.to_token_stream()));
	out.extend(Some(punc!(',')));
	out.extend(TokenStream::from(comp_type.to_token_stream()));
	out.extend(Some(punc!(',')));
	if wrapped {
		// WrappedMatch<name, D>
		out.extend(vec![ident!("WrappedMatch"), punc!('<'), ident!(name), punc!(','), ident!("D"), punc!('>')])
	} else {
		out.extend(Some(ident!(name)))
	}
	out.extend(vec![punc!('>'), punc!('>'), puncj!(':'), punc!(':'), ident!("parse"), group!(Delimiter::Parenthesis, Some(ident!("src")))]);
	out
}

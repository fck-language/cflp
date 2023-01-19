use proc_macro::{Delimiter, Spacing, TokenStream, Span};
use quote::ToTokens;
use syn::{Ident, Expr, Type, Visibility};

/// Single or group of values
#[derive(Clone)]
pub(crate) enum Value {
	/// Single value
	Single(Expr),
	/// Saved value
	Save(SaveType),
	/// Group of values
	Group(Vec<Group>, bool),
}

#[derive(Clone)]
pub(crate) enum SaveType {
	Literal(Expr),
	Unwrapping(Expr, Vec<SaveUnwrapType>)
}

#[derive(Clone)]
pub(crate) enum SaveUnwrapType {
	Use(Type),
	Ignore
}

impl SaveUnwrapType {
	pub fn should_use(&self) -> bool {
		match self {
			SaveUnwrapType::Use(_) => true,
			SaveUnwrapType::Ignore => false
		}
	}
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
	// Or values ()|()|...|()
	// Or(Vec<Value>, bool)
}

/// Macro rule
#[derive(Clone)]
pub(crate) struct MacroInner {
	/// Metadata
	pub meta: Meta,
	/// Rules
	pub rules: Rules
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
	pub derived_traits: TokenStream
}

/// Macro rule
#[derive(Clone)]
pub(crate) struct Rule {
	/// Rule name
	pub name: Ident,
	/// Rule group
	pub inner: Vec<Group>
}

/// Macro rules
#[derive(Clone)]
pub(crate) struct Rules(pub Vec<Rule>);

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))}; }

impl Rule {
	pub(crate) fn return_type(&self, match_type: &TokenStream) -> TokenStream {
		let mut inner_ret = self.inner.iter().map(|t| t.get_return_type(match_type)).filter(|t| !t.is_empty());
		let mut out = TokenStream::new();
		if let Some(i) = inner_ret.next() {
			out.extend(i);
			for i in inner_ret {
				out.extend(Some(punc!(',')));
				out.extend(i)
			}
		}
		TokenStream::from(group!(Delimiter::Parenthesis; out))
	}
}

impl Value {
	pub(crate) fn get_return_type(&self, match_type: &TokenStream) -> TokenStream {
		match self {
			Value::Single(_) => TokenStream::new(),
			Value::Save(inner) => {
				match inner {
					SaveType::Literal(_) => match_type.clone(),
					SaveType::Unwrapping(_, args) => {
						let mut out = TokenStream::new();
						let mut iter = args.iter().filter_map(|t| if let SaveUnwrapType::Use(t) = t { Some(t) } else { None });
						if let Some(n) = iter.next() {
							out.extend(TokenStream::from(n.to_token_stream()));
							for n in iter {
								out.extend(Some(punc!(',')));
								out.extend(TokenStream::from(n.to_token_stream()));
							}
						}
						if out.is_empty() { out } else {
							TokenStream::from(group!(Delimiter::Parenthesis; out))
						}
					}
				}
			},
			Value::Group(v, s) => {
				if !s { TokenStream::new() } else {
					let mut out = Vec::new();
					for i in v {
						if i.contains_save() {
							out.extend(i.get_return_type(match_type));
							out.push(punc!(','))
						}
					}
					out.pop();
					TokenStream::from_iter(vec![group!(Delimiter::Parenthesis, out)])
				}
			}
		}
	}
	
	pub(crate) fn contains_save(&self) -> bool {
		match self {
			Value::Single(_) => false,
			Value::Save(_) => true,
			Value::Group(_, s) => s.clone()
		}
	}
}

impl Group {
	pub(crate) fn get_return_type(&self, match_type: &TokenStream) -> TokenStream {
		match self {
			Group::Literal(v, s) => if *s { v.get_return_type(match_type) } else { TokenStream::new() },
			Group::Kleene(v, s) | Group::Positive(v, s) => {
				if !s { return TokenStream::new() }
				let mut out = TokenStream::from_iter(vec![ident!("Vec"), punc!('<')]);
				out.extend(v.get_return_type(match_type));
				out.extend(Some(punc!('>')));
				out
			}
			Group::Option(v, s) => {
				if !s { return TokenStream::new() }
				let mut out = TokenStream::from_iter(vec![ident!("Option"), punc!('<')]);
				out.extend(v.get_return_type(match_type));
				out.extend(Some(punc!('>')));
				out
			}
		}
	}
	
	pub(crate) fn contains_save(&self) -> bool {
		match self {
			Group::Literal(_, t) | Group::Kleene(_, t) | Group::Positive(_, t) | Group::Option(_, t) => t.clone()
		}
	}
}

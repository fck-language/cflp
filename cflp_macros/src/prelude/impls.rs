use proc_macro::{TokenStream, TokenTree, Spacing, Delimiter, Span};
use syn::{Ident, Type};
use crate::prelude::{Group, ReturnType, RuleInner, Value};

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))}; }
macro_rules! puncj { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Joint))}; }

impl ReturnType {
	/// Make a new non-conflicting lifetime
	pub fn new_lifetime(&self, wrap_result: bool) -> Self {
		match self {
			ReturnType::Function => Self::Lifetime(0, wrap_result),
			ReturnType::Lifetime(i, _) => ReturnType::Lifetime(i + 1, wrap_result)
		}
	}
	
	/// Set if the lifetime should wrap it's result
	pub fn set_wrapped(self, t: bool) -> Self {
		match self {
			ReturnType::Function => self,
			ReturnType::Lifetime(i, _) => Self::Lifetime(i, t)
		}
	}
	
	/// Check if the return type should be wrapped
	pub fn is_wrapped(&self) -> bool {
		match self {
			ReturnType::Function => false,
			ReturnType::Lifetime(_, t) => *t
		}
	}
	
	/// Returns a `TokenStream` of the current lifetime
	pub fn get_lifetime(&self) -> TokenStream {
		match self {
			ReturnType::Function => TokenStream::new(),
			ReturnType::Lifetime(i, _) => TokenStream::from_iter(vec![puncj!('\''), ident!(&*format!("l{}", i))])
		}
	}
	
	/// Builds a `TokenStream` to return the given `TokenStream`
	pub fn to_token_stream(&self, inner: TokenStream) -> TokenStream {
		let mut out = match self {
			ReturnType::Function => TokenStream::from(ident!("return")),
			ReturnType::Lifetime(i, _) => TokenStream::from_iter(vec![ident!("break"), puncj!('\''), ident!(&*format!("l{}", i))])
		};
		out.extend(inner);
		out
	}
}

impl RuleInner {
	
	/// Build wrapper
	pub(crate) fn build(&self, caller: &Ident, return_type: ReturnType, comp_type: &Type, map_fn: &TokenStream) -> (TokenStream, TokenTree) {
		let mut out = TokenStream::new();
		let mut final_return = Vec::new();
		for (k, i) in self.inner.iter().enumerate() {
			if i.contains_save() {
				final_return.extend(vec![ident!(&*format!("v_{}", k)), punc!(',')]);
				out.extend(i.build_save(format!("v_{}", k), caller, return_type, comp_type, map_fn));
			} else {
				out.extend(i.build_no_save(return_type, comp_type, map_fn));
			}
			out.extend(Some(punc!(';')));
		}
		final_return.pop();
		(out, group!(Delimiter::Parenthesis, final_return))
	}
}

impl Value {
	
	/// Returns if the value is or has a nested `Value::Save`
	pub(crate) fn contains_save(&self) -> bool {
		match self {
			Value::Single(_) => false,
			Value::Call(_) => false,
			Value::Save(_) => true,
			Value::Group(_, s) => s.clone()
		}
	}
}

impl Group {
	/// Returns if the value is or has a nested `Value::Save`
	pub(crate) fn contains_save(&self) -> bool {
		match self {
			Group::Literal(_, t) | Group::Kleene(_, t) | Group::Positive(_, t) | Group::Option(_, t) => t.clone()
		}
	}
}

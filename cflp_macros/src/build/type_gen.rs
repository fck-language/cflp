use proc_macro::{Delimiter, TokenStream};
use std::collections::HashSet;
use quote::ToTokens;
use syn::{Ident, Visibility, Type};
use crate::prelude::{Group, NamedRuleInner, Rule, RuleInnerEnum, Value};
use crate::saving::{MatchArg, SaveType};
use crate::lifetimes::Lifetimes;

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, proc_macro::Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, proc_macro::TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, proc_macro::TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, proc_macro::Spacing::Alone))}; }

impl Rule {
	pub(crate) fn type_gen(&self, comp_type: &Type, vis: &Visibility) -> TokenStream {
		let mut out = TokenStream::from(vis.to_token_stream());
		let mut lifetimes = HashSet::new();
		self.lifetimes(comp_type, &mut lifetimes);
		match &self.inner {
			RuleInnerEnum::Single(inner) => {
				out.extend(Some(ident!("struct")));
				out.extend(TokenStream::from(self.name.to_token_stream()));
				if !lifetimes.is_empty() {
					out.extend(Some(punc!('<')));
					for l in lifetimes {
						out.extend(TokenStream::from(l.to_token_stream()));
						out.extend(Some(punc!(',')));
					}
					out.extend(Some(punc!('>')));
				}
				out.extend(Some(group!(Delimiter::Parenthesis; inner.type_gen(&self.name, comp_type))));
				out.extend(Some(punc!(';')));
			}
			RuleInnerEnum::Multiple(all_inner) => {
				out.extend(Some(ident!("enum")));
				out.extend(TokenStream::from(self.name.to_token_stream()));
				if !lifetimes.is_empty() {
					out.extend(Some(punc!('<')));
					for l in lifetimes {
						out.extend(TokenStream::from(l.to_token_stream()));
						out.extend(Some(punc!(',')));
					}
					out.extend(Some(punc!('>')));
				}
				let mut enum_inner = TokenStream::new();
				for (index, rule) in all_inner.iter().enumerate() {
					if let Some(name) = &rule.name {
						enum_inner.extend(TokenStream::from(name.to_token_stream()));
					} else {
						enum_inner.extend(Some(ident!(&*format!("Var{}", index + 1))));
					}
					enum_inner.extend(vec![
						group!(Delimiter::Parenthesis; rule.type_gen(&self.name, comp_type)),
						punc!(','),
					])
				}
				out.extend(Some(group!(Delimiter::Brace; enum_inner)));
			}
		}
		out
	}
}

trait TypeGen {
	/// Generate the type specific tokens for the generated type returned by the associated Parser
	/// implementation
	fn type_gen(&self, caller: &Ident, comp_type: &Type) -> TokenStream;
}

impl TypeGen for NamedRuleInner {
	fn type_gen(&self, caller: &Ident, comp_type: &Type) -> TokenStream {
		let mut out_raw = Vec::new();
		for group in self.inner.iter() {
			let group_type = group.type_gen(caller, comp_type);
			if !group_type.is_empty() {
				out_raw.push(group_type);
			}
		}
		let mut out_raw = out_raw.iter();
		let mut out = TokenStream::new();
		if let Some(i) = out_raw.next() {
			let mut inner = i.clone();
			for i in out_raw {
				inner.extend(Some(punc!(',')));
				inner.extend(i.clone());
			}
			out.extend(inner)
		}
		out
	}
}

impl TypeGen for Group {
	fn type_gen(&self, caller: &Ident, comp_type: &Type) -> TokenStream {
		match self {
			Group::Literal(v, s) => if *s { v.type_gen(caller, comp_type) } else { TokenStream::new() },
			Group::Kleene(v, s) | Group::Positive(v, s) => {
				if !s { return TokenStream::new(); }
				let mut out = TokenStream::from_iter(vec![ident!("Vec"), punc!('<')]);
				out.extend(v.type_gen(caller, comp_type));
				out.extend(Some(punc!('>')));
				out
			}
			Group::Option(v, s) => {
				if !s { return TokenStream::new(); }
				let mut out = TokenStream::from_iter(vec![ident!("Option"), punc!('<')]);
				out.extend(v.type_gen(caller, comp_type));
				out.extend(Some(punc!('>')));
				out
			}
		}
	}
}

impl TypeGen for Value {
	fn type_gen(&self, caller: &Ident, comp_type: &Type) -> TokenStream {
		match self {
			Value::Single(_) => TokenStream::new(),
			Value::Call(n) => TokenStream::from(n.to_token_stream()),
			Value::Save(inner) => inner.type_gen(caller, comp_type),
			Value::Group(v, s) => {
				if !s { TokenStream::new() } else {
					let mut out = Vec::new();
					for i in v {
						if i.contains_save() {
							out.push(i.type_gen(caller, comp_type));
						}
					}
					let mut out = out.iter();
					let mut inner = out.next().unwrap().clone();
					for i in out {
						inner.extend(Some(punc!(',')));
						inner.extend(i.clone());
					}
					inner
				}
			}
		}
	}
}

impl TypeGen for SaveType {
	fn type_gen(&self, caller: &Ident, comp_type: &Type) -> TokenStream {
		match self {
			SaveType::Match(_, args) => {
				if args.iter().position(|t| t.is_type()).is_some() {
					// unwrap match
					let mut out = Vec::new();
					for i in args.iter() {
						if let MatchArg::Type(t) = i {
							out.push(TokenStream::from(t.to_token_stream()));
						}
					}
					let mut out = out.iter();
					let mut inner = out.next().unwrap().clone();
					if out.len() == 0 {
						inner
					} else {
						for i in out {
							inner.extend(Some(punc!(',')));
							inner.extend(i.clone());
						}
						TokenStream::from(group!(Delimiter::Parenthesis; inner))
					}
				} else {
					// simple match
					TokenStream::from(comp_type.to_token_stream())
				}
			}
			SaveType::Call(n, lifetimes) => {
				let mut out = if n == caller {
					TokenStream::from_iter(vec![ident!("Box"), punc!('<')])
				} else {
					TokenStream::new()
				};
				out.extend(TokenStream::from(n.to_token_stream()));
				let mut lifetimes = lifetimes.iter();
				if let Some(l) = lifetimes.next() {
					out.extend(Some(punc!('<')));
					out.extend(TokenStream::from(l.to_token_stream()));
					for l in lifetimes {
						out.extend(Some(punc!(',')));
						out.extend(TokenStream::from(l.to_token_stream()));
					}
					out.extend(Some(punc!('>')));
				}
				if n == caller { out.extend(Some(punc!('>'))); }
				out
			}
		}
	}
}

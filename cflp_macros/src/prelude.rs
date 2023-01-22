use proc_macro::{Delimiter, Spacing, TokenStream, Span, TokenTree};
use quote::ToTokens;
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
	pub inner: RuleInnerEnum
}

#[derive(Clone)]
pub(crate) enum RuleInnerEnum {
	Single(RuleInner),
	Multiple(Vec<RuleInner>)
}

#[derive(Clone)]
pub(crate) struct RuleInner(pub Vec<Group>);

/// Macro rules
#[derive(Clone)]
pub(crate) struct Rules(pub Vec<Rule>);

#[derive(Copy, Clone)]
pub(crate) enum ReturnType {
	Function,
	Lifetime(u8, bool)
}

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))}; }
macro_rules! puncj { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Joint))}; }

impl ReturnType {
	pub fn new_lifetime(&self, wrap_result: bool) -> Self {
		match self {
			ReturnType::Function => Self::Lifetime(0, wrap_result),
			ReturnType::Lifetime(i, _) => ReturnType::Lifetime(i + 1, wrap_result)
		}
	}
	
	pub fn set_wrapped(self, t: bool) -> Self {
		match self {
			ReturnType::Function => self,
			ReturnType::Lifetime(i, _) => Self::Lifetime(i, t)
		}
	}
	
	pub fn is_wrapped(&self) -> bool {
		match self {
			ReturnType::Function => false,
			ReturnType::Lifetime(_, t) => *t
		}
	}
	
	pub fn get_lifetime(&self) -> TokenStream {
		match self {
			ReturnType::Function => TokenStream::new(),
			ReturnType::Lifetime(i, _) => TokenStream::from_iter(vec![puncj!('\''), ident!(&*format!("l{}", i))])
		}
	}
	
	pub fn to_token_stream(&self, inner: TokenStream) -> TokenStream {
		let mut out = match self {
			ReturnType::Function => TokenStream::from(ident!("return")),
			ReturnType::Lifetime(i, _) => TokenStream::from_iter(vec![ident!("break"), puncj!('\''), ident!(&*format!("l{}", i))])
		};
		out.extend(inner);
		out
	}
}

impl Rule {
	pub(crate) fn generate_type(&self, visibility: &Visibility, match_type: &TokenStream) -> TokenStream {
		let mut out = TokenStream::from(visibility.to_token_stream());
		match &self.inner {
			RuleInnerEnum::Single(inner) => {
				out.extend(Some(ident!("struct")));
				out.extend(TokenStream::from(self.name.to_token_stream()));
				out.extend(inner.return_type(match_type));
				out.extend(Some(punc!(';')));
			},
			RuleInnerEnum::Multiple(all_inner) => {
				out.extend(Some(ident!("enum")));
				out.extend(TokenStream::from(self.name.to_token_stream()));
				let mut enum_inner = TokenStream::new();
				for (index, rule) in all_inner.iter().enumerate() {
					enum_inner.extend(vec![
						ident!(&*format!("Var{}", index + 1)),
						group!(Delimiter::Parenthesis; rule.return_type(match_type)),
						punc!(',')
					])
				}
				out.extend(Some(group!(Delimiter::Brace; enum_inner)));
			}
		}
		out
	}
}

impl RuleInner {
	pub(crate) fn return_type(&self, match_type: &TokenStream) -> TokenStream {
		let mut inner_ret = self.0.iter().map(|t| t.get_return_type(match_type)).filter(|t| !t.is_empty());
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
	
	pub(crate) fn build(&self, return_type: ReturnType, comp_type: &TokenStream, map_fn: &TokenStream) -> (TokenStream, TokenTree) {
		let mut out = TokenStream::new();
		let mut final_return = Vec::new();
		for (k, i) in self.0.iter().enumerate() {
			if i.contains_save() {
				final_return.extend(vec![ident!(&*format!("v_{}", k)), punc!(',')]);
				out.extend(i.build_save(format!("v_{}", k), return_type, comp_type, map_fn));
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
	pub(crate) fn get_return_type(&self, match_type: &TokenStream) -> TokenStream {
		match self {
			Value::Single(_) => TokenStream::new(),
			Value::Call(n) => TokenStream::from(n.to_token_stream()),
			Value::Save(inner) => {
				match inner {
					SaveType::Literal(_) => match_type.clone(),
					SaveType::Call(n) => TokenStream::from(n.to_token_stream()),
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
			Value::Call(_) => false,
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

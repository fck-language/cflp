use proc_macro::{Delimiter, TokenStream, Spacing, Span};
use quote::ToTokens;
use syn::Ident;
use crate::prelude::{Value, Group, SaveType, ReturnType};

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))}; }
macro_rules! puncj { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Joint))}; }

impl SaveType {
	pub(crate) fn if_condition(&self, wrap: bool, map_fn: &TokenStream) -> TokenStream {
		let mut ret = if wrap { TokenStream::from(ident!("Ok")) } else { TokenStream::new() };
		match self {
			SaveType::Literal(e) => {
				// if Some($e) == next.map($map_fn) { Ok($e) }
				ret.extend(vec![
					group!(Delimiter::Parenthesis; TokenStream::from(e.to_token_stream()))
				]);
				TokenStream::from_iter(vec![
					ident!("if"), ident!("Some"), group!(Delimiter::Parenthesis; TokenStream::from(e.to_token_stream())),
					puncj!('='), punc!('='), ident!("next"), punc!('.'), ident!("map"),
					group!(Delimiter::Parenthesis; map_fn.clone()),
					group!(Delimiter::Brace; ret)
				])
			}
			SaveType::Call(_) => unreachable!("SaveType::Call(_).if_condition should be unreachable. Covered by another calling match arm"),
			SaveType::Unwrapping(e, t) => {
				// if let Some($e($t_match)) = next.map($map_fn) { Ok($t_return) }
				let mut match_vars = Vec::new();
				let mut return_vars = Vec::new();
				for (index, value) in t.iter().enumerate() {
					if value.should_use() {
						match_vars.extend(vec![ident!(&*format!("n_{}", index)), punc!(',')]);
						return_vars.extend(vec![ident!(&*format!("n_{}", index)), punc!(',')]);
					} else {
						match_vars.extend(vec![ident!("_"), punc!(',')]);
					}
				}
				match_vars.pop();
				return_vars.pop();
				
				let mut some_inner = TokenStream::from(e.to_token_stream());
				some_inner.extend(Some(group!(Delimiter::Parenthesis, match_vars)));
				
				ret.extend(vec![
					group!(Delimiter::Parenthesis, Some(group!(Delimiter::Parenthesis, return_vars)))
				]);
				
				TokenStream::from_iter(vec![
					ident!("if"), ident!("let"), ident!("Some"), group!(Delimiter::Parenthesis; some_inner),
					punc!('='), ident!("next"), punc!('.'), ident!("map"),
					group!(Delimiter::Parenthesis; map_fn.clone()),
					group!(Delimiter::Brace; ret)
				])
			}
		}
	}
	
	pub(crate) fn to_type(&self) -> TokenStream {
		match self {
			SaveType::Literal(e) => TokenStream::from(e.to_token_stream()),
			SaveType::Call(_) => unreachable!("SaveType::Call(_).to_type should be unreachable. Covered by another calling match arm"),
			SaveType::Unwrapping(e, t) => {
				let mut out = TokenStream::from(e.to_token_stream());
				let mut t_iter = t.iter();
				if let Some(_) = t_iter.next() {
					let mut inner = TokenStream::from_iter(vec![ident!("Default"), puncj!(':'), punc!(':'), ident!("default"), group!(Delimiter::Parenthesis)]);
					for _ in t_iter {
						inner.extend(vec![punc!(','), ident!("Default"), puncj!(':'), punc!(':'), ident!("default"), group!(Delimiter::Parenthesis)]);
					}
					out.extend(Some(group!(Delimiter::Parenthesis; inner)))
				}
				out
			}
		}
	}
}

impl Value {
	pub(crate) fn build_save(&self, n: String, return_type: ReturnType, match_type: &TokenStream, map_fn: &TokenStream) -> TokenStream {
		match self {
			Value::Single(_) => unreachable!("Value::Single variant should be inaccessible under a save function"),
			Value::Call(_) => unreachable!("Value::Call variant should be inaccessible under a save function"),
			Value::Save(SaveType::Call(n)) => build_value_save_call(n, return_type),
			Value::Save(v) => build_value_save_other(v, return_type, map_fn),
			Value::Group(g, _) => {
				let inner_return_type = return_type.set_wrapped(false);
				let mut out = TokenStream::new();
				let mut returned = Vec::new();
				for (k, i) in g.iter().enumerate() {
					if i.contains_save() {
						returned.extend(vec![ident!(&*format!("{}_{}", n, k)), punc!(',')]);
						out.extend(i.build_save(format!("{}_{}", n, k), inner_return_type, match_type, map_fn))
					} else {
						out.extend(i.build_no_save(inner_return_type, match_type, map_fn))
					}
				}
				returned.pop();
				out.extend(Some(punc!(';')));
				if return_type.is_wrapped() {
					out.extend(Some(ident!("Ok")))
				}
				out.extend(Some(group!(Delimiter::Parenthesis, returned)));
				out
			}
		}
	}
}

impl Group {
	pub(crate) fn build_save(&self, n: String, return_type: ReturnType, match_type: &TokenStream, map_fn: &TokenStream) -> TokenStream {
		let mut out = TokenStream::from_iter(vec![
			ident!("let"), ident!(&*n), punc!('=')
		]);
		let inner = match self {
			Group::Literal(v, _) => v.build_save(n, return_type, match_type, map_fn),
			Group::Kleene(v, _) => {
				let mut out = TokenStream::from_iter(vec![
					ident!("let"), ident!("mut"), ident!(&*format!("{}_out", n)), punc!('='),
					ident!("Vec"), puncj!(':'), punc!(':'), ident!("new"), group!(Delimiter::Parenthesis), punc!(';')
				]);
				out.extend(kleene_inner(v, n.clone(), return_type, match_type, map_fn));
				out
			}
			Group::Positive(v, _) => {
				// a+ == a a*
				let mut out = Group::Literal(v.clone(), true).build_save(format!("{}_0", n), return_type, match_type, map_fn);
				// $n.push($n_0)
				out.extend(TokenStream::from_iter(vec![
					ident!("let"), ident!("mut"), ident!(&*format!("{}_out", n)), punc!('='),
					ident!("vec"), puncj!('!'), group!(Delimiter::Bracket, Some(ident!(&*format!("{}_0", n)))), punc!(';')
				]));
				out.extend(kleene_inner(v, n.clone(), return_type, match_type, map_fn));
				out
			}
			Group::Option(v, _) => build_group_option(v, n.clone(), return_type, match_type, map_fn)
		};
		out.extend(vec![group!(Delimiter::Brace; inner), punc!(';')]);
		out
	}
}

/// ```rust
/// loop {
/// 	let src_old = src.clone();
/// 	match 'l: {
/// 		v.build_save()
/// 	} {
/// 		Ok(t) => n_out.push(t),
/// 		Err(_) => {
/// 			*src = src_old;
/// 			break
/// 		}
/// 	}
/// }
/// ```
fn kleene_inner(v: &Value, n: String, return_type: ReturnType, match_type: &TokenStream, map_fn: &TokenStream) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let mut loop_inner = TokenStream::from_iter(vec![
		ident!("let"), ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';'),
		ident!("match")
	]);
	loop_inner.extend(inner_return_type.get_lifetime());
	loop_inner.extend(vec![
		punc!(':'),
		group!(Delimiter::Brace; v.build_save(format!("{}_0", n), inner_return_type, match_type, map_fn)),
		group!(Delimiter::Brace, vec![
			ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), puncj!('='), punc!('>'), ident!(&*format!("{}_out", n)), punc!('.'), ident!("push"), group!(Delimiter::Parenthesis, Some(ident!("t"))), punc!(','),
			ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("_"))), puncj!('='), punc!('>'), group!(Delimiter::Brace, vec![
				punc!('*'), ident!("src"), punc!('='), ident!("src_old"), punc!(';'), ident!("break")
			])
		])
	]);
	TokenStream::from_iter(vec![
		ident!("loop"), group!(Delimiter::Brace; loop_inner),
		ident!(&*format!("{}_out", n))
	])
}

/// ```rust
/// match e::parse(src) {
/// 	Ok(t) => (Ok)(t),
/// 	Err(e) => return_type.to_token_stream(Err(e))
/// }
/// ```
fn build_value_save_call(e: &Ident, return_type: ReturnType) -> TokenStream {
	let mut out = TokenStream::from(ident!("match"));
	out.extend(TokenStream::from(e.to_token_stream()));
	out.extend(vec![
		puncj!(':'), punc!(':'), ident!("parse"), group!(Delimiter::Parenthesis, Some(ident!("src")))
	]);
	let mut match_inner = TokenStream::from_iter(vec![
		ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), puncj!('='), punc!('>')
	]);
	if return_type.is_wrapped() {
		match_inner.extend(vec![ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t")))])
	} else {
		match_inner.extend(Some(ident!("t")))
	}
	match_inner.extend(vec![
		punc!(','), ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("e"))), puncj!('='), punc!('>')
	]);
	match_inner.extend(return_type.to_token_stream(TokenStream::from_iter(vec![
		ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("e")))
	])));
	out.extend(Some(group!(Delimiter::Brace; match_inner)));
	out
}

/// ```rust
/// let next = src.next();
/// if e.if_condition()
/// else {
/// 	return_type.to_token_stream(
/// 		Err(cflp::Error { expected: e.to_type(), found: next }
/// 	)
/// }
/// ```
fn build_value_save_other(e: &SaveType, return_type: ReturnType, map_fn: &TokenStream) -> TokenStream {
	let mut err_inner = TokenStream::from_iter(vec![ident!("expected"), punc!(':')]);
	err_inner.extend(TokenStream::from(e.to_type()));
	err_inner.extend(vec![punc!(','), ident!("found"), punc!(':'), ident!("next")]);
	let err = TokenStream::from_iter(vec![ident!("Err"), group!(Delimiter::Parenthesis, vec![
		ident!("cflp"), puncj!(':'), punc!(':'), ident!("Error"), group!(Delimiter::Brace; err_inner)
	])]);
	let mut out = TokenStream::from_iter(vec![
		ident!("let"), ident!("next"), punc!('='), ident!("src"), punc!('.'), ident!("next"), group!(Delimiter::Parenthesis), punc!(';')
	]);
	out.extend(e.if_condition(return_type.is_wrapped(), map_fn));
	out.extend(vec![
		ident!("else"), group!(Delimiter::Brace; return_type.to_token_stream(err))
	]);
	out
}

/// ```rust
/// let src_old = src.clone();
/// match 'l: { v.build() } {
/// 	Ok(t) => Some(t),
/// 	Err(e) => { *src = src_old; None }
/// }
/// ```
fn build_group_option(e: &Value, n: String, return_type: ReturnType, match_type: &TokenStream, map_fn: &TokenStream) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let mut out = TokenStream::from_iter(vec![
		ident!("let"), ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';'),
		ident!("match")
	]);
	out.extend(inner_return_type.get_lifetime());
	out.extend(vec![
		punc!(':'),
		group!(Delimiter::Brace; e.build_save(format!("{}_0", n), inner_return_type, match_type, map_fn)),
		group!(Delimiter::Brace, vec![
			ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), puncj!('='), punc!('>'), ident!("Some"), group!(Delimiter::Parenthesis, Some(ident!("t"))), punc!(','),
			ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("_"))), puncj!('='), punc!('>'), group!(Delimiter::Brace, vec![
				punc!('*'), ident!("src"), punc!('='), ident!("src_old"), punc!(';'), ident!("None")
			])
		])
	]);
	out
}

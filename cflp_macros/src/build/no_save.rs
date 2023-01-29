//! Token streams for values and groups

use proc_macro::{Delimiter, Spacing, Span, TokenStream};
use quote::ToTokens;
use syn::{Expr, Ident, Type};

use crate::prelude::{Group, ReturnType, Value};

macro_rules! ident {
	($t:expr) => {
		proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))
	};
}
macro_rules! group {
	($t:expr) => {
		proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))
	};
	($t:expr, $s:expr) => {
		proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))
	};
	($t:expr; $s:expr) => {
		proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))
	};
}
macro_rules! punc {
	($t:literal) => {
		proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))
	};
}
macro_rules! puncj {
	($t:literal) => {
		proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Joint))
	};
}

impl Value {
	/// Builds a `Value` to a `TokenStream` without saving it
	pub(crate) fn build_no_save(
		&self, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream,
	) -> TokenStream {
		match self {
			Value::Single(t) => build_value_single(t, return_type, map_fn),
			Value::Call(n) => build_value_call(n, return_type),
			Value::Save(_) => unreachable!("Value::Save variant should be inaccessible under a no_save function"),
			Value::Group(g, _) => {
				let mut out = TokenStream::new();
				for i in g {
					out.extend(i.build_no_save(return_type, match_type, map_fn))
				}
				out
			}
		}
	}
}

impl Group {
	/// Builds a `Group` to a `TokenStream` without saving it
	pub(crate) fn build_no_save(
		&self, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream,
	) -> TokenStream {
		match self {
			Group::Literal(v, _) => v.build_no_save(return_type, match_type, map_fn),
			Group::Kleene(v, _) => build_group_kleene(v, return_type, match_type, map_fn),
			Group::Positive(v, _) => {
				// a+ == a a*
				let mut out = v.clone().build_no_save(return_type, match_type, map_fn);
				out.extend(build_group_kleene(v, return_type, match_type, map_fn));
				out
			}
			Group::Option(v, _) => build_group_option(v, return_type, match_type, map_fn),
		}
	}
}

/// ```rust
/// let next = src.next();
/// if next.clone().map(map_fn) != e.to_token_stream() {
/// 	return_type.to_token_stream(cflp::Error{expected: e.to_token_stream(), found: next})
/// }
/// ```
fn build_value_single(e: &Expr, return_type: ReturnType, map_fn: &TokenStream) -> TokenStream {
	let mut err_inner = TokenStream::from_iter(vec![ident!("expected"), punc!(':')]);
	err_inner.extend(TokenStream::from(e.to_token_stream()));
	err_inner.extend(vec![punc!(','), ident!("found"), punc!(':'), ident!("next")]);
	TokenStream::from_iter(vec![
		ident!("let"),
		ident!("next"),
		punc!('='),
		ident!("src"),
		punc!('.'),
		ident!("next"),
		group!(Delimiter::Parenthesis),
		punc!(';'),
		ident!("if"),
		ident!("next"),
		punc!('.'),
		ident!("clone"),
		group!(Delimiter::Parenthesis),
		punc!('.'),
		ident!("map"),
		group!(Delimiter::Parenthesis; map_fn.clone()),
		puncj!('!'),
		punc!('='),
		ident!("Some"),
		group!(Delimiter::Parenthesis; TokenStream::from(e.to_token_stream())),
		group!(Delimiter::Brace;
			return_type.to_token_stream(TokenStream::from_iter(vec![
				ident!("Err"), group!(Delimiter::Parenthesis, vec![
					ident!("cflp"), puncj!(':'), punc!(':'), ident!("Error"),
					group!(Delimiter::Brace; err_inner)
				])
			]))
		),
	])
}

/// ```rust
/// if let Err(e) = e::parse(src) { return_type.to_token_stream(Err(e)) }
/// ```
fn build_value_call(e: &Ident, return_type: ReturnType) -> TokenStream {
	let mut out = TokenStream::from_iter(vec![
		ident!("if"),
		ident!("let"),
		ident!("Err"),
		group!(Delimiter::Parenthesis, Some(ident!("e"))),
		punc!('='),
	]);
	out.extend(TokenStream::from(e.to_token_stream()));
	out.extend(vec![
		puncj!(':'),
		punc!(':'),
		ident!("parse"),
		group!(Delimiter::Parenthesis, Some(ident!("src"))),
	]);
	out.extend(return_type.to_token_stream(TokenStream::from_iter(vec![
		ident!("Err"),
		group!(Delimiter::Parenthesis, Some(ident!("e"))),
	])));
	out
}

/// ```rust
/// loop {
/// 	let src_old = src.clone();
///  	if return_type.new_lifetime(true).get_lifetime(): {
/// 		e.build_no_save(
/// 			return_type.new_lifetime(true),
/// 			match_type, map_fn
/// 		);
/// 		Ok(())
/// 	}.is_err() {
/// 		*src = src_old;
/// 		break
/// 	}
/// }
/// ```
fn build_group_kleene(e: &Value, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let mut out = TokenStream::from_iter(vec![
		ident!("let"),
		ident!("src_old"),
		punc!('='),
		ident!("src"),
		punc!('.'),
		ident!("clone"),
		group!(Delimiter::Parenthesis),
		punc!(';'),
		ident!("if"),
	]);
	out.extend(inner_return_type.get_lifetime());
	let mut condition_inner = e.build_no_save(inner_return_type, match_type, map_fn);
	condition_inner.extend(vec![
		ident!("Ok"),
		group!(Delimiter::Parenthesis, Some(group!(Delimiter::Parenthesis))),
	]);
	out.extend(vec![
		punc!(':'),
		group!(Delimiter::Brace; condition_inner),
		punc!('.'),
		ident!("is_err"),
		group!(Delimiter::Parenthesis),
		group!(
			Delimiter::Brace,
			vec![
				punc!('*'),
				ident!("src"),
				punc!('='),
				ident!("src_old"),
				punc!(';'),
				ident!("break")
			]
		),
	]);
	TokenStream::from_iter(vec![ident!("loop"), group!(Delimiter::Brace; out)])
}

/// ```rust
/// let src_old = src.clone();
/// if return_type.new_lifetime(true).get_lifetime(): {
/// 	e.build_no_save(
/// 		return_type.new_lifetime(true),
/// 		match_type, map_fn
/// 	);
/// 	Ok(())
/// }.is_err() {
/// 	*src = src_old;
/// }
/// ```
fn build_group_option(e: &Value, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let mut out = TokenStream::from_iter(vec![
		ident!("let"),
		ident!("src_old"),
		punc!('='),
		ident!("src"),
		punc!('.'),
		ident!("clone"),
		group!(Delimiter::Parenthesis),
		punc!(';'),
		ident!("if"),
	]);
	out.extend(inner_return_type.get_lifetime());
	let mut condition_inner = e.build_no_save(inner_return_type, match_type, map_fn);
	condition_inner.extend(vec![
		ident!("Ok"),
		group!(Delimiter::Parenthesis, Some(group!(Delimiter::Parenthesis))),
	]);
	out.extend(vec![
		punc!(':'),
		group!(Delimiter::Parenthesis; condition_inner),
		punc!('.'),
		ident!("is_err"),
		group!(Delimiter::Parenthesis),
		group!(
			Delimiter::Brace,
			vec![punc!('*'), ident!("src"), punc!('='), ident!("src_old")]
		),
	]);
	out
}

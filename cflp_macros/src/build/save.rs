#[allow(rustdoc::invalid_rust_codeblocks)]
use proc_macro::{Delimiter, TokenStream};
use quote::ToTokens;
use syn::{Ident, Type};

use crate::{
	prelude::{Group, ReturnType, Value},
	saving::{MatchArg, SaveType},
};

macro_rules! ident {
	($t:expr) => {
		proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, proc_macro::Span::mixed_site()))
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
		proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, proc_macro::Spacing::Alone))
	};
}
macro_rules! puncj {
	($t:literal) => {
		proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, proc_macro::Spacing::Joint))
	};
}

impl Value {
	/// Builds a `Value` to a `TokenStream` and saves it
	pub(crate) fn build_save(
		&self, n: String, caller: &Ident, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream,
	) -> TokenStream {
		match self {
			Value::Single(_) => unreachable!("Value::Single variant should be inaccessible under a save function"),
			Value::Call(_) => unreachable!("Value::Call variant should be inaccessible under a save function"),
			Value::Save(SaveType::Call(rule, _)) => build_value_save_call(rule, return_type, caller == rule),
			Value::Save(SaveType::Match(ident, args)) => build_value_save_other(ident, args, return_type, map_fn),
			Value::Group(g, _) => {
				let inner_return_type = return_type.set_wrapped(false);
				let mut out = TokenStream::new();
				let mut returned = Vec::new();
				for (k, i) in g.iter().enumerate() {
					if i.contains_save() {
						returned.extend(vec![ident!(&*format!("{}_{}", n, k)), punc!(',')]);
						out.extend(i.build_save(format!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn))
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
	/// Builds a `Group` to a `TokenStream` and saves it
	pub(crate) fn build_save(
		&self, n: String, caller: &Ident, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream,
	) -> TokenStream {
		let mut out = TokenStream::from_iter(vec![ident!("let"), ident!(&*n), punc!('=')]);
		let inner = match self {
			Group::Literal(v, _) => v.build_save(n, caller, return_type, match_type, map_fn),
			Group::Kleene(v, _) => {
				let mut out = TokenStream::from_iter(vec![
					ident!("let"),
					ident!("mut"),
					ident!(&*format!("{}_out", n)),
					punc!('='),
					ident!("Vec"),
					puncj!(':'),
					punc!(':'),
					ident!("new"),
					group!(Delimiter::Parenthesis),
					punc!(';'),
				]);
				out.extend(kleene_inner(v, n.clone(), caller, return_type, match_type, map_fn));
				out
			}
			Group::Positive(v, _) => {
				// a+ == a a*
				let mut out = Group::Literal(v.clone(), true).build_save(
					format!("{}_0", n),
					caller,
					return_type,
					match_type,
					map_fn,
				);
				// $n.push($n_0)
				out.extend(TokenStream::from_iter(vec![
					ident!("let"),
					ident!("mut"),
					ident!(&*format!("{}_out", n)),
					punc!('='),
					ident!("vec"),
					puncj!('!'),
					group!(Delimiter::Bracket, Some(ident!(&*format!("{}_0", n)))),
					punc!(';'),
				]));
				out.extend(kleene_inner(v, n.clone(), caller, return_type, match_type, map_fn));
				out
			}
			Group::Option(v, _) => build_group_option(v, n.clone(), caller, return_type, match_type, map_fn),
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
fn kleene_inner(
	v: &Value, n: String, caller: &Ident, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream,
) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let mut loop_inner = TokenStream::from_iter(vec![
		ident!("let"),
		ident!("src_old"),
		punc!('='),
		ident!("src"),
		punc!('.'),
		ident!("clone"),
		group!(Delimiter::Parenthesis),
		punc!(';'),
		ident!("match"),
	]);
	loop_inner.extend(inner_return_type.get_lifetime());
	loop_inner.extend(vec![
		punc!(':'),
		group!(Delimiter::Brace; v.build_save(format!("{}_0", n), caller, inner_return_type, match_type, map_fn)),
		group!(
			Delimiter::Brace,
			vec![
				ident!("Ok"),
				group!(Delimiter::Parenthesis, Some(ident!("t"))),
				puncj!('='),
				punc!('>'),
				ident!(&*format!("{}_out", n)),
				punc!('.'),
				ident!("push"),
				group!(Delimiter::Parenthesis, Some(ident!("t"))),
				punc!(','),
				ident!("Err"),
				group!(Delimiter::Parenthesis, Some(ident!("_"))),
				puncj!('='),
				punc!('>'),
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
				)
			]
		),
	]);
	TokenStream::from_iter(vec![
		ident!("loop"),
		group!(Delimiter::Brace; loop_inner),
		ident!(&*format!("{}_out", n)),
	])
}

/// Matches the returned value of a rule call and optionally boxes and/or wrap the result in an `Ok`
/// ```rust
/// match e::parse(src) {
/// 	Ok(t) => t,
/// 	Err(e) => return_type.to_token_stream(Err(e))
/// }
/// ```
fn build_value_save_call(e: &Ident, return_type: ReturnType, is_boxed: bool) -> TokenStream {
	let mut out = TokenStream::from(ident!("match"));
	out.extend(TokenStream::from(e.to_token_stream()));
	out.extend(vec![
		puncj!(':'),
		punc!(':'),
		ident!("parse"),
		group!(Delimiter::Parenthesis, Some(ident!("src"))),
	]);
	let mut match_inner = TokenStream::from_iter(vec![
		ident!("Ok"),
		group!(Delimiter::Parenthesis, Some(ident!("t"))),
		puncj!('='),
		punc!('>'),
	]);
	let returned = if is_boxed {
		TokenStream::from_iter(vec![
			ident!("Box"),
			puncj!(':'),
			punc!(':'),
			ident!("new"),
			group!(Delimiter::Parenthesis, Some(ident!("t"))),
		])
	} else {
		TokenStream::from(ident!("t"))
	};
	if return_type.is_wrapped() {
		match_inner.extend(vec![ident!("Ok"), group!(Delimiter::Parenthesis; returned)])
	} else {
		match_inner.extend(returned)
	}
	match_inner.extend(vec![
		punc!(','),
		ident!("Err"),
		group!(Delimiter::Parenthesis, Some(ident!("e"))),
		puncj!('='),
		punc!('>'),
	]);
	match_inner.extend(return_type.to_token_stream(TokenStream::from_iter(vec![
		ident!("Err"),
		group!(Delimiter::Parenthesis, Some(ident!("e"))),
	])));
	out.extend(Some(group!(Delimiter::Brace; match_inner)));
	out
}

/// ```rust
/// let next = src.next();
/// match next.clone().map(map_fn) {
/// 	Some(e_match_arm()) => some_return_value,
/// 	_ => Err(err_inner)
/// };
/// ```
fn build_value_save_other(
	e: &Type, args: &Vec<MatchArg>, return_type: ReturnType, map_fn: &TokenStream,
) -> TokenStream {
	let mut err_inner = TokenStream::from_iter(vec![ident!("expected"), punc!(':')]);
	err_inner.extend(TokenStream::from(e.to_token_stream()));
	err_inner.extend(Some(group!(
		Delimiter::Parenthesis,
		args.iter().fold(TokenStream::new(), |mut acc, t| {
			match t {
				MatchArg::Literal(e) => acc.extend(TokenStream::from(e.to_token_stream())),
				_ => acc.extend(vec![
					ident!("Default"),
					puncj!(':'),
					punc!(':'),
					ident!("default"),
					group!(Delimiter::Parenthesis),
				]),
			}
			acc
		})
	)));
	err_inner.extend(vec![punc!(','), ident!("found"), punc!(':'), ident!("next")]);
	let err = TokenStream::from_iter(vec![
		ident!("Err"),
		group!(
			Delimiter::Parenthesis,
			vec![
				ident!("cflp"),
				puncj!(':'),
				punc!(':'),
				ident!("Error"),
				group!(Delimiter::Brace; err_inner)
			]
		),
	]);

	let mut arg_index = 0;
	let mut returned_args = Vec::new();
	let mut match_arm = TokenStream::from(e.to_token_stream());
	let mut match_arm_inner = TokenStream::new();
	for arg in args {
		match_arm_inner.extend(arg.inner_match_arm(&mut arg_index, &mut returned_args))
	}
	returned_args.pop();
	match_arm.extend(Some(group!(Delimiter::Parenthesis; match_arm_inner)));
	let mut match_arm = TokenStream::from_iter(vec![
		ident!("Some"),
		group!(Delimiter::Parenthesis; match_arm),
		puncj!('='),
		punc!('>'),
	]);
	if return_type.is_wrapped() {
		match_arm.extend(Some(ident!("Ok")))
	}
	if arg_index == 0 {
		// simple match
		match_arm.extend(Some(group!(
			Delimiter::Parenthesis,
			vec![
				ident!("next"),
				punc!('.'),
				ident!("map"),
				group!(Delimiter::Parenthesis; map_fn.clone()),
				punc!('.'),
				ident!("unwrap"),
				group!(Delimiter::Parenthesis),
				punc!('.'),
				ident!("clone"),
				group!(Delimiter::Parenthesis)
			]
		)))
	} else {
		// unwrapping match
		if returned_args.len() == 1 {
			match_arm.extend(Some(returned_args.pop().unwrap()))
		} else {
			match_arm.extend(Some(group!(Delimiter::Parenthesis, returned_args)))
		}
	}
	match_arm.extend(vec![punc!(','), ident!("_"), puncj!('='), punc!('>')]);
	match_arm.extend(return_type.to_token_stream(err));
	let mut out = TokenStream::from_iter(vec![
		ident!("let"),
		ident!("next"),
		punc!('='),
		ident!("src"),
		punc!('.'),
		ident!("next"),
		group!(Delimiter::Parenthesis),
		punc!(';'),
	]);
	out.extend(vec![
		ident!("match"),
		ident!("next"),
		punc!('.'),
		ident!("clone"),
		group!(Delimiter::Parenthesis),
		punc!('.'),
		ident!("map"),
		group!(Delimiter::Parenthesis; map_fn.clone()),
		group!(Delimiter::Brace; match_arm),
	]);
	out
}

/// ```rust
/// let src_old = src.clone();
/// match 'l: { v.build() } {
/// 	Ok(t) => Some(t),
/// 	Err(e) => { *src = src_old; None }
/// };
/// ```
fn build_group_option(
	e: &Value, n: String, caller: &Ident, return_type: ReturnType, match_type: &Type, map_fn: &TokenStream,
) -> TokenStream {
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
		ident!("match"),
	]);
	out.extend(inner_return_type.get_lifetime());
	out.extend(vec![
		punc!(':'),
		group!(Delimiter::Brace; e.build_save(format!("{}_0", n), caller, inner_return_type, match_type, map_fn)),
		group!(
			Delimiter::Brace,
			vec![
				ident!("Ok"),
				group!(Delimiter::Parenthesis, Some(ident!("t"))),
				puncj!('='),
				punc!('>'),
				ident!("Some"),
				group!(Delimiter::Parenthesis, Some(ident!("t"))),
				punc!(','),
				ident!("Err"),
				group!(Delimiter::Parenthesis, Some(ident!("_"))),
				puncj!('='),
				punc!('>'),
				group!(
					Delimiter::Brace,
					vec![
						punc!('*'),
						ident!("src"),
						punc!('='),
						ident!("src_old"),
						punc!(';'),
						ident!("None")
					]
				)
			]
		),
	]);
	out
}

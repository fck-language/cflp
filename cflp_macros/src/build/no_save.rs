//! # Non-saving code gen
//!
//! This module contains code for building impl sections for elements that are not saved. There are
//! never any calls to functions in the [saving](crate::build::save) equivalent module

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Expr, Ident, Type};
use crate::prelude::{Value, Group, ReturnType, SplitRule};

impl Value {
	/// Builds a `Value` to a `TokenStream` without saving it
	pub(crate) fn build_no_save(&self, return_type: ReturnType, match_type: &Type, map: bool) -> TokenStream {
		match self {
			Value::Single(t) => build_value_single(t, match_type, return_type, map),
			Value::Call(n) => build_value_call(n, return_type),
			Value::Save { .. } => unreachable!("Value::Save variant should be inaccessible under a no_save function\n{}", std::backtrace::Backtrace::force_capture()),
			Value::Group(g, _) => {
				match g {
					SplitRule::Single(inner) => inner.build_no_save(return_type, match_type, map),
					SplitRule::Other { start, middle, end } => {
						let start = start.build_no_save(return_type, match_type, map);
						let middle = middle.iter().map(|i| i.build_no_save(return_type, match_type, map));
						let end = end.build_no_save(return_type, match_type, map);
						quote!{ #start; #(#middle;)* #end; }
					}
				}
			}
		}
	}
}

impl Group {
	/// Builds a `Group` to a `TokenStream` without saving it
	pub(crate) fn build_no_save(&self, return_type: ReturnType, match_type: &Type, map: bool) -> TokenStream {
		match self {
			Group::Literal(v, _) => {
				v.build_no_save(return_type, match_type, map)
			},
			Group::Kleene(v, _) => build_group_kleene(v, return_type, match_type, map),
			Group::Positive(v, _) => {
				// a+ == a a*
				let mut out = v.clone().build_no_save(return_type, match_type, map);
				out.extend(build_group_kleene(v, return_type, match_type, map));
				out
			}
			Group::Option(v, _) => build_group_option(v, return_type, match_type, map)
		}
	}
}

/// ```rust
/// let next = src.next();
/// if next.clone().map(map_fn) != Some(e.to_token_stream()) {
/// 	return_type.to_token_stream(cflp::Error{expected: e.to_token_stream(), found: next})
/// }
/// ```
fn build_value_single(e: &Expr, cmp: &Type, return_type: ReturnType, map: bool) -> TokenStream {
	let ret_err = return_type.to_token_stream(quote!{ Err(cflp::Error{ expected: #e, found: next }) });
	if map {
		quote! {
			let next = src.next();
			if next.clone().map(Into::<#cmp>::into) != Some(#e) {
				#ret_err
			}
		}
	} else {
		quote! {
			let next = src.next();
			if next != Some(&#e) {
				#ret_err
			}
		}
	}
}

/// ```rust
/// if let Err(E) = <e as cflp::Parser>::parse(src) { return_type.to_token_stream(Err(e)) }
/// ```
fn build_value_call(e: &Ident, return_type: ReturnType) -> TokenStream {
	let ret_err = return_type.to_token_stream(quote!{Err(e)});
	quote!{if let Err(e) = <#e as cflp::Parser<_, _, _>>::parse(src) { #ret_err }}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_kleene(e: &Value, return_type: ReturnType, match_type: &Type, map: bool) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_no_save(inner_return_type, match_type, map);
	quote!{
		loop {
			let src_old = src.clone();
			if #ret_lifetime {
				#inner;
				Ok(())
			}.is_err() {
				*src = src_old;
				break;
			}
		}
	}
}

/// ```rust,ignore
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
fn build_group_option(e: &Value, return_type: ReturnType, match_type: &Type, map: bool) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_no_save(inner_return_type, match_type, map);
	quote!{
		let src_old = src.clone();
		if #ret_lifetime: {
			#inner;
			Ok(())
		}.is_err() {
			*src = src_old;
		}
	}
}

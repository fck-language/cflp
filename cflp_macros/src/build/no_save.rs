//! # Non-saving code gen
//!
//! This module contains code for building impl sections for elements that are not saved. There are
//! never any calls to functions in the [saving](crate::build::save) equivalent module

use proc_macro2::TokenStream;
use quote::quote;
use syn::{Pat, Path};
use crate::build::save::value_save_other_expected;
use crate::prelude::{Value, Group, ReturnType, SplitRule, Meta};

impl Value {
	/// Builds a `Value` to a `TokenStream` without saving it
	pub(crate) fn build_no_save(&self, return_type: ReturnType, meta: &Meta) -> TokenStream {
		match self {
			Value::Single(t) => build_value_single(t, return_type, meta),
			Value::Call(n) => build_value_call(n, return_type, meta),
			Value::Save { .. } => unreachable!("Value::Save variant should be inaccessible under a no_save function\n{}", std::backtrace::Backtrace::force_capture()),
			Value::Group(g, _) => {
				match g {
					SplitRule::AllPNM(groups) => {
						let groups = groups.iter().map(|i| i.build_no_save(return_type, meta));
						quote!{ #(#groups;)* }
					}
					SplitRule::Single { pre_PNM, group, post_PNM } => {
						let pre_pnm = pre_PNM.iter().map(|i| i.build_no_save(return_type, meta));
						let group = group.build_no_save(return_type, meta);
						let post_pnm = post_PNM.iter().map(|i| i.build_no_save(return_type, meta));
						quote!{ #(#pre_pnm;)* #group; #(#post_pnm;)* }
					}
					SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
						let pre_pnm = pre_PNM.iter().map(|i| i.build_no_save(return_type, meta));
						let start = start.build_no_save(return_type, meta);
						let middle = middle.iter().map(|i| i.build_no_save(return_type, meta));
						let end = end.build_no_save(return_type, meta);
						let post_pnm = post_PNM.iter().map(|i| i.build_no_save(return_type, meta));
						quote!{ #(#pre_pnm;)* #start; #(#middle;)* #end; #(#post_pnm;)* }
					}
				}
			}
		}
	}
}

impl Group {
	/// Builds a `Group` to a `TokenStream` without saving it
	pub(crate) fn build_no_save(&self, return_type: ReturnType, meta: &Meta) -> TokenStream {
		match self {
			Group::Literal(v, _) => {
				v.build_no_save(return_type, meta)
			},
			Group::Kleene(v, _) => build_group_kleene(v, return_type, meta),
			Group::Positive(v, _) => {
				// a+ == a a*
				let mut out = v.clone().build_no_save(return_type, meta);
				out.extend(build_group_kleene(v, return_type, meta));
				out
			}
			Group::Option(v, _) => build_group_option(v, return_type, meta)
		}
	}
}

/// ```rust, ignore
/// let next = src.next();
/// if next.clone().map(map_fn) != Some(e.to_token_stream()) {
/// 	return_type.to_token_stream(cflp::Error{
/// 		expected: e.to_token_stream(),
/// 		found: next,
/// 		scope: vec![<Self as cflp::Scope<#scope>>::scope()]
/// 	})
/// }
/// ```
fn build_value_single(e: &Pat, return_type: ReturnType, meta: &Meta) -> TokenStream {
	let scope = &meta.scope;
	let cmp = &meta.cmp_type;

	let expect = value_save_other_expected(e);
	let ret_err = return_type.to_token_stream(quote!{ Err(cflp::Error{ expected: #expect, found: next, scope: vec![<Self as cflp::Scope<#scope>>::scope()] }) });
	if meta.is_wrapped {
		quote! {
			let next = src.next();
			if let Some(#e) = next.clone().map(Into::<#cmp>::into) {} else {
				#ret_err
			}
		}
	} else {
		quote! {
			let next = src.next();
			if let Some(&#e) = next {} else {
				#ret_err
			}
		}
	}
}

/// ```rust, ignore
/// if let Err(E) = <e as cflp::Parser>::parse(src) { return_type.to_token_stream(Err(e)) }
/// ```
fn build_value_call(e: &Path, return_type: ReturnType, meta: &Meta) -> TokenStream {
	let scope = &meta.scope;

	let ret_err = return_type.to_token_stream(quote!{Err(e)});
	quote!{if let Err(mut e) = <#e as cflp::Parser<_, _, #scope, _>>::parse(src) {
		e.push_scope(<Self as cflp::Scope<#scope>>::scope());
		#ret_err
	}}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_kleene(e: &Value, return_type: ReturnType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_no_save(inner_return_type, meta);
	quote!{
		loop {
			let src_old = src.clone();
			if #ret_lifetime: {
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
fn build_group_option(e: &Value, return_type: ReturnType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_no_save(inner_return_type, meta);
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

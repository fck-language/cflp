//! # Saving code gen
//!
//! This module contains code for building impl sections for elements that are saved. There are
//! calls to functions in the [non-saving](crate::build::no_save) equivalent

use std::backtrace::Backtrace;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{Expr, Ident, Pat, Path, Token};
use syn::punctuated::Punctuated;
use crate::prelude::{Value, Group, ReturnType, SaveType, SplitRule, Meta};

impl Value {
	/// Builds a `Value` to a `TokenStream` and saves it
	pub(crate) fn build_save(&self, n: &Ident, caller: &Path, return_type: ReturnType, meta: &Meta) -> TokenStream {
		match self {
			Value::Single(_) => unreachable!("Value::Single variant should be inaccessible under a save function\n{}", Backtrace::force_capture()),
			Value::Call(_) => unreachable!("Value::Call variant should be inaccessible under a save function\n{}", Backtrace::force_capture()),
			Value::Save { group: SaveType::Call(rule), boxed } => build_value_save_call(rule, caller, return_type, *boxed, meta),
			Value::Save { group: SaveType::Other{ pattern, default }, .. } => build_value_save_other(pattern, default, return_type, meta),
			Value::Group(g, _) => g.build_save(n, caller, return_type, meta)
		}
	}
}

impl Group {
	/// Builds a `Group` to a `TokenStream` and saves the matched group. Does not save any
	/// positional information
	///
	/// # Arguments
	/// * `n`: Identifier that the save group should use
	/// * `caller`: The name of the type deriving the impl; or whatever `Self` is an alias for
	/// * `return_type`: How to return. See [`ReturnType`]
	/// * `match_type`: Type the tokens are being compared to
	/// * `map`: Are the input and comparison types the same
	/// * `scope`: Scope/backtrace type
	pub(crate) fn build_save(&self, n: &Ident, caller: &Path, return_type: ReturnType, meta: &Meta) -> TokenStream {
		let mut out = quote!{ let #n = };
		let inner = match self {
			Group::Literal(v, _) => v.build_save(n, caller, return_type, meta),
			Group::Kleene(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let mut out = quote!{ let mut #n_ident_out = Vec::new(); };
				out.extend(kleene_inner(v, n.clone(), n_ident_out, caller, return_type, meta));
				out
			}
			Group::Positive(v, _) => {
				// a+ == a a*
				let n_0 = format_ident!("{}_0", n);
				let mut out = Group::Literal(v.clone(), true).build_save(&n_0, caller, return_type, meta);
				let n_ident_out = format_ident!("{}_out", n);
				out.extend(quote!{let mut #n_ident_out = vec![#n_0]; });
				out.extend(kleene_inner(v, n.clone(), n_ident_out, caller, return_type, meta));
				out
			}
			Group::Option(v, _) => build_group_option(v, n.clone(), caller, return_type, meta)
		};
		out.extend(quote!{ { #inner }; });
		out
	}
}

impl SplitRule {
	/// Builds a `Value` to a `TokenStream` and saves it
	pub(crate) fn build_save(&self, n: &Ident, caller: &Path, return_type: ReturnType, meta: &Meta) -> TokenStream {
		let inner_return_type = return_type.set_wrapped(false);
		let mut out = TokenStream::new();
		let mut k = 0usize;
		match self {
			SplitRule::AllPNM(groups) => {
				for i in groups.iter() {
					if i.contains_save() {
						out.extend(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
						k += 1
					} else {
						out.extend(i.build_no_save(inner_return_type, meta));
					}
				}
			}
			SplitRule::Single { pre_PNM, group, post_PNM } => {
				for i in pre_PNM.iter() {
					if i.contains_save() {
						out.extend(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
						k += 1
					} else {
						out.extend(i.build_no_save(inner_return_type, meta));
					}
				}
				if group.contains_save() {
					out.extend(group.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
					k += 1
				} else {
					out.extend(group.build_no_save(inner_return_type, meta));
				}
				for i in post_PNM.iter() {
					if i.contains_save() {
						out.extend(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
						k += 1
					} else {
						out.extend(i.build_no_save(inner_return_type, meta));
					}
				}
			}
			SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
				for i in pre_PNM.iter() {
					if i.contains_save() {
						out.extend(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
						k += 1
					} else {
						out.extend(i.build_no_save(inner_return_type, meta));
					}
				}
				if start.contains_save() {
					out.extend(start.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
					k += 1
				} else {
					out.extend(start.build_no_save(inner_return_type, meta));
				}
				for i in middle.iter() {
					if i.contains_save() {
						out.extend(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
						k += 1
					} else {
						out.extend(i.build_no_save(inner_return_type, meta));
					}
				}
				if end.contains_save() {
					out.extend(end.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
					k += 1
				} else {
					out.extend(end.build_no_save(inner_return_type, meta));
				}
				for i in post_PNM.iter() {
					if i.contains_save() {
						out.extend(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
						k += 1
					} else {
						out.extend(i.build_no_save(inner_return_type, meta));
					}
				}
			}
		}
		let vars = (0..k).map(|t| format_ident!("{}_{}", n, t));
		match (return_type.is_wrapped(), k) {
			(true, 1) => quote!{ #out; Ok(#(#vars),*) },
			(false, 1) => quote!{ #out; #(#vars),* },
			(true, _) => quote!{ #out; Ok((#(#vars),*)) },
			(false, _) => quote!{ #out; (#(#vars),*) },
		}
	}
}

pub fn build_match_arm_err(pattern: &Pat, default: Option<&Expr>, meta: &Meta) -> (TokenStream, TokenStream) {
	let scope = &meta.scope;
	let expected = if let Some(def) = default { def.to_token_stream() } else { value_save_other_expected(pattern) };
	(
		quote!{ _ => Err(cflp::Error { expected: #expected, found: None, scope: vec![<Self as cflp::Scope<#scope>>::scope()] }) },
		quote!{ _ => Err(cflp::Error { expected: #expected, found: Some(t_unwrapped), scope: vec![<Self as cflp::Scope<#scope>>::scope()] }) }
	)
}

/// ```rust, ignore
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
fn kleene_inner(v: &Value, n: Ident, out: Ident, caller: &Path, return_type: ReturnType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let lifetime = inner_return_type.get_lifetime();
	let n_0 = format_ident!("{}_0", n);
	let match_inner = v.build_save(&n_0, caller, inner_return_type, meta);
	quote!{
		loop {
			let src_old = src.clone();
			match #lifetime: { #match_inner } {
				Ok(t) => #out.push(t),
				Err(_) => { *src = src_old; break }
			}
		}
		#out
	}
}

/// Matches the returned value of a rule call and optionally boxes and/or wrap the result in an `Ok`
fn build_value_save_call(e: &Path, caller: &Path, return_type: ReturnType, is_boxed: bool, meta: &Meta) -> TokenStream {
	let scope = &meta.scope;
	let ok_ret = match (is_boxed, return_type.is_wrapped()) {
		(true, true) => quote!{ Ok(Box::new(t)) },
		(true, false) => quote!{ Box::new(t) },
		(false, true) => quote!{ Ok(t) },
		(false, false) => quote!{ t }
	};
	let ret = return_type.to_token_stream(quote!{Err(e)});
	let first_seg_e = e.segments.first().unwrap().ident.to_string();
	if first_seg_e == caller.segments.first().unwrap().ident.to_string() || &*first_seg_e == "Self" {
		let ret_recurse = return_type.to_token_stream(quote!{
			Err(cflp::Error {
				expected: Default::default(),
				found: None,
				scope: vec![<Self as cflp::Scope<#scope>>::scope()]
			})
		});
		quote!{
			if !recurse && !consumed_tokens {
				#ret_recurse
			}
			match <#e as cflp::Parser<_, _, #scope, _>>::parse_with_recursion(src, false) {
				Ok(t) => #ok_ret,
				Err(mut e) => {
					e.push_scope(<Self as Scope<#scope>>::scope());
					#ret
				}
			}
		}
	} else {
		quote!{
			match <#e as cflp::Parser<_, _, #scope, _>>::parse_with_recursion(src, true) {
				Ok(t) => #ok_ret,
				Err(mut e) => {
					e.push_scope(<Self as Scope<#scope>>::scope());
					#ret
				}
			}
		}
	}
}

/// ```rust, ignore
/// let next = src.next();
/// match next {
/// 	Some(__next_unwrapped) => {
/// 		if Into::<cmp>::into(__next_unwrapped.clone()) ==
/// 	}
/// 	None => Err(err_inner)
/// }
/// match next.clone().map(Into::<cmp>::into) {
/// 	Some(next_match @ e_match_arm()) => some_return_value,
/// 	_ => Err(err_inner)
/// };
/// ```
fn build_value_save_other(p: &Pat, default: &Option<Expr>, return_type: ReturnType, meta: &Meta) -> TokenStream {
	let cmp = &meta.cmp_type;
	let scope = &meta.scope;

	let returned_args = value_save_other_get_args(p);
	let ok_arm = if returned_args.is_empty() {
		if return_type.is_wrapped() {
			quote!{ Ok(__next.clone()) }
		} else {
			quote!{ __next.clone() }
		}
	} else {
		match (returned_args.len(), return_type.is_wrapped()) {
			(1, false) => quote!{ #(#returned_args.clone()),* },
			(1, true) => quote!{ Ok(#(#returned_args.clone()),*) },
			(_, false) => quote!{ (#(#returned_args.clone()),*) },
			(_, true) => quote!{ Ok((#(#returned_args.clone()),*)) },
		}
	};
	
	let expect = if let Some(def) = default { quote!{ #def } } else { value_save_other_expected(p) };
	let err = return_type.to_token_stream(quote!{ Err(cflp::Error { found: next, expected: #expect, scope: vec![<Self as Scope<#scope>>::scope()] }) });
	let match_expr = if meta.is_wrapped {
		quote!{ Into::<#cmp>::into(__next) }
	} else {
		quote!{ __next }
	};
	quote! {
		let next = src.next();
		match next {
			Some(__next) => if let #p = #match_expr {
				#ok_arm
			} else { #err }
			_ => #err
		}
	}
}

/// Extract values available from matching a [pattern](syn::Pat) in a `match` expression
pub fn value_save_other_get_args(p: &Pat) -> Vec<Ident> {
	match p {
		Pat::Ident(ident) => vec![ident.ident.clone()],
		Pat::Paren(inner) => value_save_other_get_args(&inner.pat),
		Pat::Reference(ident) => value_save_other_get_args(&ident.pat),
		Pat::Slice(slice) => slice.elems.iter().flat_map(value_save_other_get_args).collect(),
		Pat::Struct(struc) => struc.fields.iter().flat_map(|t| value_save_other_get_args(&t.pat)).collect(),
		Pat::Tuple(inner) => inner.elems.iter().flat_map(value_save_other_get_args).collect(),
		Pat::TupleStruct(tstruc) => tstruc.elems.iter().flat_map(value_save_other_get_args).collect(),
		Pat::Type(ty) => value_save_other_get_args(&ty.pat),
		_ => Vec::new()
	}
}

pub(crate) fn value_save_other_expected(p: &Pat) -> TokenStream {
	match p {
		Pat::Lit(lit) => quote!{ #lit },
		Pat::Macro(m) => quote!{ #m },
		Pat::Or(or) => value_save_other_expected(or.cases.first().unwrap()),
		Pat::Paren(inner) => value_save_other_expected(&inner.pat),
		Pat::Path(path) => quote!{ #path },
		Pat::Range(range) => if let Some(ref start) = range.start {
			quote!{ #start }
		} else if let Some(ref end) = range.end {
			quote!{ #end }
		} else {
			quote!{ Default::default() }
		},
		Pat::Slice(inner) => {
			let inner = inner.elems.iter().map(value_save_other_expected);
			quote!{ [#(#inner),*] }
		}
		Pat::Struct(inner) => {
			let fields = Punctuated::<_, Token![,]>::from_iter(inner.fields.iter().map(|t| {
				let val = value_save_other_expected(&t.pat);
				let name = &t.member;
				quote!{ #name: #val }
			}));
			let name = &inner.path;
			quote!{ #name { #fields } }
		}
		Pat::Tuple(paren) => Punctuated::<_, Token![,]>::from_iter(paren.elems.iter().map(value_save_other_expected)).to_token_stream(),
		Pat::TupleStruct(tstruct) => {
			let fields = Punctuated::<_, Token![,]>::from_iter(tstruct.elems.iter().map(value_save_other_expected));
			let name = &tstruct.path;
			quote!{ #name ( #fields ) }
		}
		_ => quote!{ Default::default() }
	}
}

/// ```rust, ignore
/// let src_old = src.clone();
/// match 'l: { v.build() } {
/// 	Ok(t) => Some(t),
/// 	Err(e) => { *src = src_old; None }
/// };
/// ```
fn build_group_option(e: &Value, n: Ident, caller: &Path, return_type: ReturnType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let n_0 = format_ident!("{}_0", n);
	let inner = e.build_save(&n_0, caller, inner_return_type, meta);
	quote!{
		let src_old = src.clone();
		match #ret_lifetime: { #inner } {
			Ok(t) => Some(t),
			Err(e) => { *src = src_old; None }
		}
	}
}

//! # Start and end matches
//!
//! This module contains code for building the first and last parts of the impl when the result is
//! wrapped. This makes calls to both the [saving](crate::build::save) and
//! [non-saving](crate::build::no_save) build functions

use proc_macro2::TokenStream;
use syn::{Expr, Ident, Path, Token, Pat};
use quote::{format_ident, quote, ToTokens};
use syn::punctuated::Punctuated;
use crate::prelude::{Group, ReturnType, Value, PositionType, SaveType, SplitRule, Meta};

impl Value {
	/// Builds a `Value` to a `TokenStream` without saving it
	pub(crate) fn build_no_save_start_end(&self, return_type: ReturnType, position_type: PositionType, meta: &Meta) -> TokenStream {
		match self {
			Value::Single(t) => build_value_single(t, return_type,position_type, meta),
			Value::Call(n) => build_value_call(n, return_type, position_type, meta),
			Value::Save { .. } => unreachable!("{} {}:{}\nValue::Save variant should be inaccessible under a no_save function\n{}", file!(), line!(), column!(), std::backtrace::Backtrace::force_capture()),
			Value::Group(g, _) => {
				match g {
					SplitRule::AllPNM(groups) => {
						let groups = groups.iter().map(|group| group.build_no_save_start_end(return_type, position_type, meta));
						quote!{ #(#groups;)* }
					}
					SplitRule::Single { pre_PNM, group, post_PNM } => {
						let mut out = Vec::new();
						match position_type {
							PositionType::Start => {
								for i in pre_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::Start, meta));
								}
								out.push(group.build_no_save_start_end(return_type, PositionType::Start, meta));
								for i in post_PNM.iter() {
									out.push(i.build_no_save(return_type, meta));
								}
							}
							PositionType::End => {
								for i in pre_PNM.iter() {
									out.push(i.build_no_save(return_type, meta));
								}
								out.push(group.build_no_save_start_end(return_type, PositionType::End, meta));
								for i in post_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::End, meta));
								}
							}
							PositionType::StartEnd => {
								for i in pre_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::Start, meta));
								}
								out.push(group.build_no_save_start_end(return_type, PositionType::StartEnd, meta));
								for i in post_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::End, meta));
								}
							}
						}
						quote!{ #(#out);* }
					}
					SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
						let mut out = Vec::new();
						match position_type {
							PositionType::Start => {
								for i in pre_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::Start, meta));
								}
								out.push(start.build_no_save_start_end(return_type, PositionType::Start, meta));
								for i in middle.iter() {
									out.push(i.build_no_save(return_type, meta));
								}
								out.push(end.build_no_save(return_type, meta));
								for i in post_PNM.iter() {
									out.push(i.build_no_save(return_type, meta));
								}
							}
							PositionType::End => {
								for i in pre_PNM.iter() {
									out.push(i.build_no_save(return_type, meta));
								}
								out.push(start.build_no_save(return_type, meta));
								for i in middle.iter() {
									out.push(i.build_no_save(return_type, meta));
								}
								out.push(end.build_no_save_start_end(return_type, PositionType::End, meta));
								for i in post_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::End, meta));
								}
							}
							PositionType::StartEnd => {
								for i in pre_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::Start, meta));
								}
								out.push(start.build_no_save_start_end(return_type, PositionType::Start, meta));
								for i in middle.iter() {
									out.push(i.build_no_save(return_type, meta));
								}
								out.push(end.build_no_save_start_end(return_type, PositionType::End, meta));
								for i in post_PNM.iter() {
									out.push(i.build_no_save_start_end(return_type, PositionType::End, meta));
								}
							}
						}
						quote!{ #(#out);* }
					}
				}
			}
		}
	}
	
	/// Builds a `Value` to a `TokenStream` and saves it
	pub(crate) fn build_save_start_end(&self, n: &Ident, caller: &Path, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
		match self {
			Value::Single(_) => unreachable!("Value::Single variant should be inaccessible under a save function\n{}", std::backtrace::Backtrace::force_capture()),
			Value::Call(_) => unreachable!("Value::Call variant should be inaccessible under a save function\n{}", std::backtrace::Backtrace::force_capture()),
			Value::Save { group: SaveType::Call(rule), boxed } => build_value_save_call(rule, caller, return_type, *boxed, position, meta),
			Value::Save { group: SaveType::Other { pattern, default }, .. } => build_value_save_other(pattern, default, return_type, position, meta),
			Value::Group(g, _) => {
				let inner_return_type = return_type.set_wrapped(false);
				let mut out = Vec::new();
				let mut k = 0usize;
				match g {
					SplitRule::AllPNM(groups) => {
						for g in groups {
							if g.contains_save() {
								out.push(g.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
								k += 1;
							} else {
								out.push(g.build_no_save_start_end(inner_return_type, position, meta))
							}
						}
					}
					SplitRule::Single { pre_PNM, group, post_PNM } => {
						match position {
							PositionType::Start => {
								for i in pre_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
										k += 1;
									} else {
										out.push(i.build_no_save_start_end(return_type, PositionType::Start, meta));
									}
								}
								if group.contains_save() {
									out.push(group.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, PositionType::Start, meta));
									k += 1;
								} else {
									out.push(group.build_no_save_start_end(return_type, PositionType::Start, meta));
								}
								for i in post_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
									} else {
										out.push(i.build_no_save(return_type, meta));
									}
								}
							}
							PositionType::StartEnd => {
								for i in pre_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
										k += 1;
									} else {
										out.push(i.build_no_save_start_end(return_type, PositionType::Start, meta));
									}
								}
								if group.contains_save() {
									out.push(group.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, PositionType::Start, meta));
									k += 1;
								} else {
									out.push(group.build_no_save_start_end(return_type, PositionType::Start, meta));
								}
								for i in post_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
									} else {
										out.push(i.build_no_save(return_type, meta));
									}
								}
							}
							PositionType::End => {
								for i in pre_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
										k += 1;
									} else {
										out.push(i.build_no_save(return_type, meta));
									}
								}
								if group.contains_save() {
									out.push(group.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, PositionType::End, meta));
									k += 1;
								} else {
									out.push(group.build_no_save_start_end(return_type, PositionType::End, meta));
								}
								for i in post_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, PositionType::End, meta));
									} else {
										out.push(i.build_no_save_start_end(return_type, PositionType::End, meta));
									}
								}
							}
						}
					}
					SplitRule::Other { pre_PNM, start, middle, end, post_PNM } => {
						match position {
							PositionType::Start => {
								for i in pre_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
										k += 1;
									} else {
										out.push(i.build_no_save_start_end(inner_return_type, position, meta))
									}
								}
								if start.contains_save() {
									out.push(start.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
									k += 1;
								} else {
									out.push(start.build_no_save_start_end(inner_return_type, position, meta))
								}
								for i in middle.iter() {
									if i.contains_save() {
										out.push(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, meta))
									}
								}
								if end.contains_save() {
									out.push(end.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
								} else {
									out.push(end.build_no_save(inner_return_type, meta))
								}
								for i in post_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, meta))
									}
								}
							}
							PositionType::End => {
								for i in pre_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, meta))
									}
								}
								if start.contains_save() {
									out.push(start.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
									k += 1;
								} else {
									out.push(start.build_no_save(inner_return_type, meta))
								}
								for i in middle.iter() {
									if i.contains_save() {
										out.push(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, meta))
									}
								}
								if end.contains_save() {
									out.push(end.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
								} else {
									out.push(end.build_no_save_start_end(inner_return_type, position, meta))
								}
								for i in post_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, position, meta));
										k += 1;
									} else {
										out.push(i.build_no_save_start_end(inner_return_type, position, meta))
									}
								}
							}
							PositionType::StartEnd => {
								for i in pre_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, PositionType::Start, meta));
										k += 1;
									} else {
										out.push(i.build_no_save_start_end(inner_return_type, PositionType::Start, meta))
									}
								}
								if start.contains_save() {
									out.push(start.build_save_start_end(&format_ident!("{}_0", n), caller, inner_return_type, position, meta));
									k = 1;
								} else {
									out.push(start.build_no_save_start_end(inner_return_type, position, meta))
								}
								for i in middle.iter() {
									if i.contains_save() {
										out.push(i.build_save(&format_ident!("{}_{}", n, k), caller, inner_return_type, meta));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, meta))
									}
								}
								if end.contains_save() {
									out.push(end.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, PositionType::End, meta));
								} else {
									out.push(end.build_no_save_start_end(inner_return_type, PositionType::End, meta))
								}
								for i in post_PNM.iter() {
									if i.contains_save() {
										out.push(i.build_save_start_end(&format_ident!("{}_{}", n, k), caller, inner_return_type, PositionType::End, meta));
										k += 1;
									} else {
										out.push(i.build_no_save_start_end(inner_return_type, PositionType::End, meta))
									}
								}
							}
						}
					}
				}
				let returned = Punctuated::<_, Token![,]>::from_iter((0..k).map(|i| format_ident!("{}_{}", n, i)));
				match (return_type.is_wrapped(), k) {
					(true, 1) => quote!{ #(#out;)* Ok(#returned) },
					(false, 1) => quote!{ #(#out;)* #returned },
					(true, _) => quote!{ #(#out;)* Ok((#returned)) },
					(false, _) => quote!{ #(#out;)* (#returned) }
				}
			}
		}
	}
}

impl Group {
	/// Builds a `Group` to a `TokenStream` without saving it
	pub(crate) fn build_no_save_start_end(&self, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
		match self {
			Group::Literal(v, _) => v.build_no_save_start_end(return_type, position, meta),
			Group::Kleene(v, _) => build_group_kleene_ns(v, return_type, position, meta),
			Group::Positive(v, _) => build_group_positive_ns(v, return_type, position, meta),
			Group::Option(v, _) => build_group_option_ns(v, return_type, position, meta)
		}
	}
	
	/// Build a `Group` into a [`TokenStream`] where the generated code saves the matched group.
	/// Also updated the positional values of the match
	///
	/// # Arguments
	/// * `n`: Identifier that the save group should use
	/// * `caller`: The name of the type/variant deriving the impl. This is not the same as `Self`
	/// because it may be an enum variant
	/// * `return_type`: How to return. See [`ReturnType`]
	/// * `match_type`: Type the tokens are being compared to
	/// * `map`: Are the input and comparison types the same
	/// * `position`: [`PositionType`] to indicate what position should be written to
	/// * `scope`: Scope/backtrace type
	pub(crate) fn build_save_start_end(&self, n: &Ident, caller: &Path, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
		let mut out = quote!{ let #n = };
		let inner = match self {
			Group::Literal(v, _) => v.build_save_start_end(n, caller, return_type, position, meta),
			Group::Kleene(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let inner = build_group_kleene(v, &n_ident_out, caller, return_type, position, meta);
				quote!{ let mut #n_ident_out = Vec::new(); #inner; #n_ident_out }
			}
			Group::Positive(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let inner = build_group_positive(v, &n_ident_out, caller, return_type, position, meta);
				quote!{ let mut #n_ident_out = Vec::new(); #inner; #n_ident_out }
			}
			Group::Option(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let inner = build_group_option(v, &n_ident_out, caller, return_type, position, meta);
				quote!{ let mut #n_ident_out = None; #inner; #n_ident_out }
			}
		};
		out.extend(quote!{ { #inner } });
		out
	}
}

/// Build a single non-saving match
///
/// Macro code: `TokType::ToMatch`
fn build_value_single(e: &Pat, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let scope = &meta.scope;
	let cmp = &meta.cmp_type;

	let ret_err = return_type.to_token_stream(quote!{ Err(cflp::Error{ expected: #e, found: next, scope: vec![<Self as cflp::Scope<#scope>>::scope()] }) });
	let start_end = match position {
		PositionType::End => quote!{ end = __next.end(); },
		PositionType::Start => quote!{ if !start_set { start = __next.start(); start_set = true; } },
		PositionType::StartEnd => quote!{ if !start_set { start = __next.start(); start_set = true; } end = __next.end(); },
	};
	if meta.map {
		quote! {
			let next = src.next();
			if let Some(__next) = next {
				if Into::<#cmp>::into(__next) != #e {
					#ret_err
				}
				#start_end
			} else { #ret_err }
		}
	} else {
		quote! {
			let next = src.next();
			if let Some(__next) = next {
				if __next != #e {
					#ret_err
				}
				#start_end
			} else { #ret_err }
		}
	}
}

/// Build a non-saving call to another rule
///
/// Macro code: `@OtherNode`
fn build_value_call(e: &Path, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let cmp = &meta.cmp_type;
	let tok = &meta.tok_type;
	let scope = &meta.scope;

	let ret_err = return_type.to_token_stream(quote!{ Err(e) });
	let ok = match position {
		PositionType::End => quote!{ end = ok.end(); },
		PositionType::Start => quote!{ if !start_set { start = ok.start(); } },
		PositionType::StartEnd => quote!{ if !start_set { start = ok.start(); }; end = ok.end(); },
	};
	quote!{
		match <#e as cflp::Parser<#cmp, #tok, #scope, _>::parse(src) {
			Ok(ok) => { #ok }
			Err(mut e) => {
				e.push_scope(<Self as cflp::Scope<#scope>>::scope());
				#ret_err
			}
		}
	}
}

/// Matches the returned value of a rule call and optionally boxes and/or wrap the result in an `Ok`
/// ```, ignore
/// match e::parse(src) {
/// 	Ok(t) => {
/// 		start = t.start();
/// 		end = t.end();
/// 		Ok(Box::new(t))
/// 	},
/// 	Err(e) => return_type.to_token_stream(Err(e))
/// }
/// # ;
/// ```
fn build_value_save_call(e: &Path, caller: &Path, return_type: ReturnType, is_boxed: bool, position: PositionType, meta: &Meta) -> TokenStream {
	let scope = &meta.scope;

	let ok_ret = match (is_boxed, return_type.is_wrapped()) {
		(true, true) => quote!{ Ok(Box::new(t)) },
		(true, false) => quote!{ Box::new(t) },
		(false, true) => quote!{ Ok(t) },
		(false, false) => quote!{ t }
	};
	let pre = match position {
		PositionType::Start => quote!{ if !start_set { start = t.start(); start_set = true } },
		PositionType::End => quote!{ end = t.end(); },
		PositionType::StartEnd => quote!{ if !start_set { start = t.start(); start_set = true } end = t.end(); },
	};
	let ret = return_type.to_token_stream(quote!{Err(e)});
	let ret_overflow = return_type.to_token_stream(quote!{Err(cflp::Error {
					expected: Default::default(),
					found: None,
					scope: vec![<Self as cflp::Scope<#scope>>::scope()]
				})});
	let is_init = position == PositionType::Start || position == PositionType::StartEnd;
	let is_self_call = if is_init {
		e.segments.first().unwrap().ident.to_string() == "Self" || e.segments.first().unwrap().ident.to_string() == caller.segments.first().unwrap().ident.to_string()
	} else { false };
	if is_self_call {
		quote!{
			if !recurse {
				#ret_overflow
			}
			match <#e as cflp::Parser<_, _, _, _>>::parse_with_recursion(src, false) {
				Ok(t) => { #pre #ok_ret }
				Err(e) => #ret
			}
		}
	} else {
		quote!{
			match <#e as cflp::Parser<_, _, _, _>>::parse_with_recursion(src, true) {
				Ok(t) => { #pre #ok_ret }
				Err(e) => #ret
			}
		}
	}
}

/// ```rust, ignore
/// let next = src.next();
/// match next.clone().map(map_fn) {
/// 	Some(next_match @ e_match_arm()) => some_return_value,
/// 	_ => Err(err_inner)
/// };
/// ```
fn build_value_save_other(p: &Pat, default: &Option<Expr>, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let cmp = &meta.cmp_type;
	let scope = &meta.scope;
	let map = meta.map;

	/// Get all the named arguments from a pattern
	fn get_args(p: &Pat) -> Vec<Ident> {
		match p {
			Pat::Ident(ident) => vec![ident.ident.clone()],
			Pat::Paren(inner) => get_args(&inner.pat),
			Pat::Reference(ident) => get_args(&ident.pat),
			Pat::Slice(slice) => slice.elems.iter().flat_map(|t| get_args(t)).collect(),
			Pat::Struct(struc) => struc.fields.iter().flat_map(|t| get_args(&t.pat)).collect(),
			Pat::Tuple(inner) => inner.elems.iter().flat_map(|t| get_args(t)).collect(),
			Pat::TupleStruct(tstruc) => tstruc.elems.iter().flat_map(|t| get_args(t)).collect(),
			Pat::Type(ty) => get_args(&ty.pat),
			_ => Vec::new()
		}
	}
	/// Turns a pattern into a value that would match the pattern. Replaces non-literal values with
	/// `Default::default()` or a better estimate if possible
	fn expected(p: &Pat) -> TokenStream {
		match p {
			Pat::Lit(lit) => quote!{ #lit },
			Pat::Macro(m) => quote!{ #m },
			Pat::Or(or) => expected(or.cases.first().unwrap()),
			Pat::Paren(inner) => expected(&inner.pat),
			Pat::Path(path) => quote!{ #path },
			Pat::Range(range) => if let Some(ref start) = range.start {
				quote!{ #start }
			} else if let Some(ref end) = range.end {
				quote!{ #end }
			} else {
				quote!{ Default::default() }
			},
			Pat::Slice(inner) => {
				let inner = inner.elems.iter().map(|t| expected(t));
				quote!{ [#(#inner),*] }
			}
			Pat::Struct(inner) => {
				let fields = Punctuated::<_, Token![,]>::from_iter(inner.fields.iter().map(|t| {
					let val = expected(&t.pat);
					let name = &t.member;
					quote!{ #name: #val }
				}));
				let name = &inner.path;
				quote!{ #name { #fields } }
			}
			Pat::Tuple(paren) => Punctuated::<_, Token![,]>::from_iter(paren.elems.iter().map(expected)).to_token_stream(),
			Pat::TupleStruct(tstruct) => {
				let fields = Punctuated::<_, Token![,]>::from_iter(tstruct.elems.iter().map(expected));
				let name = &tstruct.path;
				quote!{ #name ( #fields ) }
			}
			_ => quote!{ Default::default() }
		}
	}
	let returned_args = get_args(p);
	
	let ok_arm = match position {
		PositionType::Start => quote!{ if !start_set { start = __next.start() } },
		PositionType::End => quote!{ end = __next.end(); },
		PositionType::StartEnd => quote!{ if !start_set { start = __next.start() } end = __next.end(); },
	};
	let ok_arm = if returned_args.is_empty() {
		if return_type.is_wrapped() {
			quote!{ #ok_arm Ok(__next.clone()) }
		} else {
			quote!{ #ok_arm __next.clone() }
		}
	} else {
		match (returned_args.len(), return_type.is_wrapped()) {
			(1, false) => quote!{ #ok_arm #(#returned_args),* },
			(1, true) => quote!{ #ok_arm Ok(#(#returned_args),*) },
			(_, false) => quote!{ #ok_arm ((#(#returned_args),*)) },
			(_, true) => quote!{ #ok_arm Ok(((#(#returned_args),*))) },
		}
	};
	
	let expect = if let Some(def) = default { def.to_token_stream() } else { expected(p) };
	let err = return_type.to_token_stream(quote!{ Err(cflp::Error { expected: #expect, found: next, scope: vec![<Self as cflp::Scope<#scope>>::scope()] }) });
	
	if map {
		quote! {
			let next = src.next();
			match next {
				Some(__next) => if let #p = Into::<#cmp>::into(__next) {
					#ok_arm
				} else { #err }
				_ => #err
			}
		}
	} else {
		quote! {
			let next = src.next();
			match next {
				Some(__next) => if let #p = __next {
					#ok_arm
				} else { #err }
				_ => #err
			}
		}
	}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_kleene_ns(e: &Value, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let first = e.build_no_save_start_end(inner_return_type, PositionType::Start, meta);
	let last = e.build_no_save_start_end(inner_return_type, PositionType::End, meta);
	let inner = e.build_no_save(inner_return_type, meta);
	match position {
		PositionType::Start => quote!{
			let saved_start_set = start_set;
			let src_old = src.clone();
			if #ret_lifetime: { #first; Ok(()) }.is_err() {
				*src = src_old;
				if !saved_start_set { start_set = false; start = Default::default(); }
			} else {
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
		},
		PositionType::End => quote!{
			loop {
				let end_old = end;
				let src_old = src.clone();
				if #ret_lifetime: {
					#last;
					Ok(())
				}.is_err() {
					*src = src_old;
					end = end_old;
					break;
				}
			}
		},
		PositionType::StartEnd => quote!{
			let saved_start_set = start_set;
			let src_old = src.clone();
			if #ret_lifetime: { #first; Ok(()) }.is_err() {
				*src = src_old;
				if !saved_start_set { start_set = false; start = Default::default(); }
				end = Default::default();
			} else {
				loop {
					let end_old = end;
				let src_old = src.clone();
					if #ret_lifetime: {a
						#last;
						Ok(())
					}.is_err() {
						*src = src_old;
						end = end_old;
						break;
					}
				}
			}
		}
	}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_positive_ns(e: &Value, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	match position {
		PositionType::Start => {
			let first = e.build_no_save_start_end(inner_return_type, position, meta);
			let inner = e.build_no_save(inner_return_type, meta);
			quote!{
				#first;
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
		},
		PositionType::End => {
			let first_end = e.build_no_save_start_end(inner_return_type, PositionType::End, meta);
			let last = e.build_no_save_start_end(inner_return_type, PositionType::End, meta);
			quote!{
				#first_end;
				loop {
					let src_old = src.clone();
					let last_old = last;
					if #ret_lifetime {
						#last;
						Ok(())
					}.is_err() {
						*src = src_old;
						last = last_old;
						break;
					}
				}
			}
		},
		PositionType::StartEnd => {
			let first = e.build_no_save_start_end(inner_return_type, position, meta);
			let last = e.build_no_save_start_end(inner_return_type, PositionType::End, meta);
			quote!{
				#first;
				loop {
					let src_old = src.clone();
					let last_old = last;
					if #ret_lifetime {
						#last;
						Ok(())
					}.is_err() {
						*src = src_old;
						last = last_old;
						break;
					}
				}
			}
		}
	}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_option_ns(e: &Value, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_no_save_start_end(inner_return_type, position, meta);
	let post = match position {
		PositionType::Start => quote!{ start = Default::default(); start_set = false; },
		PositionType::End => quote!{ end = Default::default(); },
		PositionType::StartEnd => quote!{ start = Default::default(); start_set = false; end = Default::default(); }
	};
	quote!{
		let src_old = src.clone();
		if #ret_lifetime: { #inner Ok(()) }.is_err() {
			*src = src_old; #post
		}
	}
}

/// Match as many repetitions of the group as possible
fn build_group_kleene(e: &Value, n: &Ident, caller: &Path, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	match position {
		PositionType::End => {
			let inner = e.build_save_start_end(n, caller, inner_return_type, PositionType::End, meta);
			quote!{
				loop {
					let src_old = src.clone();
					let end_old = end;
					match #ret_lifetime: { #inner } {
						Ok(t) => #n.push(t),
						_ => {
							*src = src_old;
							end = end_old;
							break;
						}
					}
				}
				#n
			}
		},
		PositionType::Start => {
			let first = e.build_save_start_end(n, caller, inner_return_type, PositionType::Start, meta);
			let inner = e.build_save(n, caller, inner_return_type, meta);
			quote!{
				let src_old = src.clone();
				match #ret_lifetime: { #first } {
					Ok(__t) => #n.push(__t),
					_ => {
						*src = src_old;
						if !start_set { start = Default::default() }
					}
				}
				if !#n.is_empty() {
					loop {
						let src_old = src.clone();
						match #ret_lifetime: { #inner } {
							Ok(t) => #n.push(t),
							_ => {
								*src = src_old;
								break;
							}
						}
					}
				}
				#n
			}
		},
		PositionType::StartEnd => {
			let first = e.build_save_start_end(n, caller, inner_return_type, PositionType::StartEnd, meta);
			let inner = e.build_save_start_end(n, caller, inner_return_type, PositionType::End, meta);
			quote!{
				let src_old = src.clone();
				match #ret_lifetime: { #first } {
					Ok(t) => #n.push(t),
					_ => {
						*src = src_old;
						if !start_set { start = Default::default(); }
						end = Default::default();
					}
				}
				if !#n.is_empty() {
					loop {
						let src_old = src.clone();
						let end_old = end;
						match #ret_lifetime: { #inner } {
							Ok(t) => #n.push(t),
							_ => {
								*src = src_old;
								end = end_old;
								break;
							}
						}
					}
				}
				#n
			}
		}
	}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_positive(e: &Value, n: &Ident, caller: &Path, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let first_err = return_type.to_token_stream(quote!{ Err(e) });
	match position {
		PositionType::Start => {
			let first = e.build_save_start_end(n, caller, inner_return_type, PositionType::Start, meta);
			let inner = e.build_save(n, caller, inner_return_type, meta);
			quote!{
				match #ret_lifetime: { #first } {
					Ok(__t) => #n.push(__t),
					Err(e) => #first_err
				}
				loop {
					let src_old = src.clone();
					match #ret_lifetime: { #inner } {
						Ok(__t) => #n.push(__t),
						_ => {
							*src = src_old;
							start = Default::default();
							start_set = false;
							break
						}
					}
				}
				#n
			}
		},
		PositionType::End => {
			let inner = e.build_save_start_end(n, caller, inner_return_type, PositionType::End, meta);
			quote!{
				match #ret_lifetime: { #inner } {
					Ok(__t) => #n.push(__t),
					Err(e) => #first_err
				}
				loop {
					let src_old = src.clone();
					let end_old = end;
					match #ret_lifetime: { #inner } {
						Ok(__t) => #n.push(__t),
						_ => {
							*src = src_old;
							end = end_old;
							break;
						}
					}
				}
				#n
			}
		},
		PositionType::StartEnd => {
			let first = e.build_save_start_end(n, caller, inner_return_type, PositionType::StartEnd, meta);
			let last = e.build_save_start_end(n, caller, inner_return_type, PositionType::End, meta);
			quote!{
				match #ret_lifetime: { #first } {
					Ok(__t) => #n.push(__t),
					Err(e) => #first_err
				}
				loop {
					let src_old = src.clone();
					let end_old = end;
					match #ret_lifetime: { #last } {
						Ok(__t) => #n.push(__t),
						_ => {
							*src = src_old;
							end = end_old;
							break;
						}
					}
				}
				#n
			}
		}
	}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_option(e: &Value, n: &Ident, caller: &Path, return_type: ReturnType, position: PositionType, meta: &Meta) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = if e.contains_save() {
		e.build_save_start_end(n, caller, inner_return_type, position, meta)
	} else {
		e.build_no_save_start_end(inner_return_type, position, meta)
	};
	let post = match position {
		PositionType::Start => quote!{ start_set = false; },
		PositionType::End => quote!{ },
		PositionType::StartEnd => quote!{ start_set = false; }
	};
	quote!{
		let src_old = src.clone();
		match #ret_lifetime: { #inner } {
			Ok(__t) => { #n = Some(__t) }
			_ => {
				*src = src_old; #post
			}
		}
	}
}

//! # Start and end matches
//!
//! This module contains code for building the first and last parts of the impl when the result is
//! wrapped. This makes calls to both the [saving](crate::build::save) and
//! [non-saving](crate::build::no_save) build functions

use proc_macro2::TokenStream;
use syn::{Expr, Type, Ident, Path, Token, Pat, ExprClosure};
use quote::{format_ident, quote, ToTokens};
use syn::punctuated::Punctuated;
use crate::prelude::{Group, ReturnType, Value, PositionType, SaveType, SplitRule};

impl Value {
	/// Builds a `Value` to a `TokenStream` without saving it
	pub(crate) fn build_no_save_start_end(&self, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, position_type: PositionType) -> TokenStream {
		match self {
			Value::Single(t) => build_value_single(t, return_type, map_fn, position_type),
			Value::Call(n) => build_value_call(n, return_type, position_type),
			Value::Save { .. } => unreachable!("Value::Save variant should be inaccessible under a no_save function"),
			Value::Group(g, _) => {
				match g {
					SplitRule::Single(g) => g.build_no_save_start_end(return_type, match_type, map_fn, position_type),
					SplitRule::Other { start, middle, end } => {
						let mut out = Vec::new();
						match position_type {
							PositionType::Start => {
								out.push(start.build_no_save_start_end(return_type, match_type, map_fn, PositionType::Start));
								for i in middle.iter() {
									out.push(i.build_no_save(return_type, match_type, map_fn));
								}
								out.push(end.build_no_save(return_type, match_type, map_fn))
							}
							PositionType::End => {
								out.push(start.build_no_save(return_type, match_type, map_fn));
								for i in middle.iter() {
									out.push(i.build_no_save(return_type, match_type, map_fn));
								}
								out.push(end.build_no_save_start_end(return_type, match_type, map_fn, PositionType::End))
							}
							PositionType::StartEnd => {
								out.push(start.build_no_save_start_end(return_type, match_type, map_fn, PositionType::Start));
								for i in middle.iter() {
									out.push(i.build_no_save(return_type, match_type, map_fn));
								}
								out.push(end.build_no_save_start_end(return_type, match_type, map_fn, PositionType::End))
							}
						}
						quote!{ #(#out);* }
					}
				}
			}
		}
	}
	
	/// Builds a `Value` to a `TokenStream` and saves it
	pub(crate) fn build_save_start_end(&self, n: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, wrapped: bool, position: PositionType) -> TokenStream {
		match self {
			Value::Single(_) => unreachable!("Value::Single variant should be inaccessible under a save function"),
			Value::Call(_) => unreachable!("Value::Call variant should be inaccessible under a save function"),
			Value::Save { group: SaveType::Call(rule), boxed } => build_value_save_call(rule, return_type, *boxed, position),
			Value::Save { group: SaveType::Other(pat), .. } => build_value_save_other(pat, return_type, map_fn, position),
			Value::Group(g, _) => {
				let inner_return_type = return_type.set_wrapped(false);
				let mut out = Vec::new();
				let mut k = 0usize;
				match g {
					SplitRule::Single(g) => out.push(g.build_save_start_end(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped, position)),
					SplitRule::Other { start, middle, end } => {
						match position {
							PositionType::Start => {
								if start.contains_save() {
									out.push(start.build_save_start_end(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped, position));
									k = 1;
								} else {
									out.push(start.build_no_save_start_end(inner_return_type, match_type, map_fn, position))
								}
								for i in middle.iter() {
									if i.contains_save() {
										out.push(i.build_save(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, match_type, map_fn))
									}
								}
								if end.contains_save() {
									out.push(end.build_save(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped));
								} else {
									out.push(end.build_no_save(inner_return_type, match_type, map_fn))
								}
							}
							PositionType::End => {
								if start.contains_save() {
									out.push(start.build_save(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped));
									k = 1;
								} else {
									out.push(start.build_no_save(inner_return_type, match_type, map_fn))
								}
								for i in middle.iter() {
									if i.contains_save() {
										out.push(i.build_save(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, match_type, map_fn))
									}
								}
								if end.contains_save() {
									out.push(end.build_save_start_end(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped, position));
								} else {
									out.push(end.build_no_save_start_end(inner_return_type, match_type, map_fn, position))
								}
							}
							PositionType::StartEnd => {
								if start.contains_save() {
									out.push(start.build_save_start_end(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped, position));
									k = 1;
								} else {
									out.push(start.build_no_save_start_end(inner_return_type, match_type, map_fn, position))
								}
								for i in middle.iter() {
									if i.contains_save() {
										out.push(i.build_save(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped));
										k += 1;
									} else {
										out.push(i.build_no_save(inner_return_type, match_type, map_fn))
									}
								}
								if end.contains_save() {
									out.push(end.build_save_start_end(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped, position));
								} else {
									out.push(end.build_no_save_start_end(inner_return_type, match_type, map_fn, position))
								}
							}
						}
					}
				}
				let returned = Punctuated::<_, Token![,]>::from_iter((0..=k).map(|i| format_ident!("{}_{}", n, i)));
				match (return_type.is_wrapped(), k) {
					(true, 0) => quote!{ #(#out;)* Ok(#returned) },
					(false, 0) => quote!{ #(#out;)* #returned },
					(true, _) => quote!{ #(#out;)* Ok((#returned)) },
					(false, _) => quote!{ #(#out;)* (#returned) }
				}
			}
		}
	}
}

impl Group {
	/// Builds a `Group` to a `TokenStream` without saving it
	pub(crate) fn build_no_save_start_end(&self, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
		match self {
			Group::Literal(v, _) => v.build_no_save_start_end(return_type, match_type, map_fn, position),
			Group::Kleene(v, _) => build_group_kleene_ns(v, return_type, match_type, map_fn, position),
			Group::Positive(v, _) => build_group_positive_ns(v, return_type, match_type, map_fn, position),
			Group::Option(v, _) => build_group_option_ns(v, return_type, match_type, map_fn, position)
		}
	}
	
	/// Builds a `Group` to a `TokenStream` and saves it
	pub(crate) fn build_save_start_end(&self, n: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, wrapped: bool, position: PositionType) -> TokenStream {
		let mut out = quote!{ let #n = };
		let inner = match self {
			Group::Literal(v, _) => v.build_save_start_end(n, caller, return_type, match_type, map_fn, wrapped, position),
			Group::Kleene(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let inner = build_group_kleene(v, n_ident_out.clone(), caller, return_type, match_type, map_fn, wrapped, position);
				quote!{ let mut #n_ident_out = Vec::new(); #inner }
			}
			Group::Positive(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let inner = build_group_positive(v, return_type, match_type, map_fn, position);
				quote!{ let mut #n_ident_out = Vec::new(); #inner }
			}
			Group::Option(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let inner = build_group_option(v, return_type, match_type, map_fn, position);
				quote!{ let mut #n_ident_out = Vec::new(); #inner }
			}
		};
		out.extend(quote!{ { #inner } });
		out
	}
}

/// ```rust
/// let next = src.next();
/// if let Some(next_unwrapped) = next {
/// 	if (map_fn)(next_unwrapped) != e {
/// 		return_type.to_token_stream(cflp::Error{expected: e.to_token_stream(), found: next})
/// 	}
/// 	// At least one of the following two are included
/// 	start = next_unwrapped.start();
/// 	end = next_unwrapped.end()
/// } else {
/// 	return_type.to_token_stream(cflp::Error{expected: e.to_token_stream(), found: next})
/// }
/// ```
fn build_value_single(e: &Expr, return_type: ReturnType, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
	let ret_err = return_type.to_token_stream(quote!{ Err(cflp::Error{ expected: #e, found: next }) });
	let start_end = match position {
		PositionType::Start => quote!{ start = __next.start(); },
		PositionType::End => quote!{ end = __next.end(); },
		PositionType::StartEnd => quote!{ start = __next.start(); end = __next.end(); }
	};
	if let Some(umap_fn) = map_fn {
		quote! {
			let next = src.next();
			if let Some(__next) = next {
				if (#umap_fn)(__next) != #e {
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

/// ```rust
/// match e::parse(src) {
/// 	Ok(ok) => {
/// 		start = ok.start();
/// 		end = ok.end();
/// 	}
/// 	Err(e) => return_type.to_token_stream(Err(e))
/// }
/// ```
fn build_value_call(e: &Ident, return_type: ReturnType, position: PositionType) -> TokenStream {
	let ret_err = return_type.to_token_stream(quote!{ Err(e) });
	let ok = match position {
		PositionType::Start => quote!{ start = ok.start(); },
		PositionType::End => quote!{ end = ok.end(); },
		PositionType::StartEnd => quote!{ start = ok.start(); end = ok.end(); }
	};
	quote!{
		match #e::parse(src) {
			Ok(ok) => { #ok }
			Err(e) => #ret_err
		}
	}
}

/// Matches the returned value of a rule call and optionally boxes and/or wrap the result in an `Ok`
/// ```
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
fn build_value_save_call(e: &Path, return_type: ReturnType, is_boxed: bool, position: PositionType) -> TokenStream {
	let ok_ret = match (is_boxed, return_type.is_wrapped()) {
		(true, true) => quote!{ Ok(Box::new(t)) },
		(true, false) => quote!{ Box::new(t) },
		(false, true) => quote!{ Ok(t) },
		(false, false) => quote!{ t }
	};
	let pre = match position {
		PositionType::Start => quote!{ start = t.start(); },
		PositionType::End => quote!{ end = t.end(); },
		PositionType::StartEnd => quote!{ start = t.start(); end = t.end(); }
	};
	let ret = return_type.to_token_stream(quote!{Err(e)});
	quote!{
		match #e::parse(src) {
			Ok(t) => { #pre #ok_ret }
			Err(e) => #ret
		}
	}
}

/// ```rust
/// let next = src.next();
/// match next.clone().map(map_fn) {
/// 	Some(next_match @ e_match_arm()) => some_return_value,
/// 	_ => Err(err_inner)
/// };
/// ```
fn build_value_save_other(p: &Pat, return_type: ReturnType, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
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
		PositionType::Start => quote!{ start = __next.start(); },
		PositionType::End => quote!{ end = __next.end(); },
		PositionType::StartEnd => quote!{ start = __next.start(); end = __next.end(); }
	};
	let ok_arm = if returned_args.is_empty() {
		if return_type.is_wrapped() {
			quote!{ #ok_arm Ok(__next_match) }
		} else {
			quote!{ #ok_arm __next_match }
		}
	} else {
		match (returned_args.len(), return_type.is_wrapped()) {
			(1, false) => quote!{ #ok_arm #(#returned_args),* },
			(1, true) => quote!{ #ok_arm Ok(#(#returned_args),*) },
			(_, false) => quote!{ #ok_arm (#(#returned_args),*) },
			(_, true) => quote!{ #ok_arm Ok((#(#returned_args),*)) },
		}
	};
	
	let expect = expected(p);
	let err = return_type.to_token_stream(quote!{ Err(cflp::Error { expected: #expect, found: next }) });
	if let Some(umap_fn) = map_fn {
		quote! {
			let next = src.next();
			match next {
				Some(__next) => if let #p = (#umap_fn)(__next) {
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
fn build_group_kleene_ns(e: &Value, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let first = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::Start);
	let last = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::End);
	let inner = e.build_no_save(inner_return_type, match_type, map_fn);
	match position {
		PositionType::Start => quote!{
			let src_old = src.clone();
			if #ret_lifetime { #first; Ok(()) }.is_err() {
				*src = src_old;
				start = Default::default();
			} else {
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
		PositionType::End => quote!{
			let end_old = Default::default();
			loop {
				let end_old = end;
				let src_old = src.clone();
				if #ret_lifetime {
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
			let src_old = src.clone();
			if #ret_lifetime { #first; Ok(()) }.is_err() {
				*src = src_old;
				start = Default::default();
				end = Default::default();
			} else {
				loop {
					let end_old = end;
				let src_old = src.clone();
					if #ret_lifetime {
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
fn build_group_positive_ns(e: &Value, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	match position {
		PositionType::Start => {
			let first = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::Start);
			let inner = e.build_no_save(inner_return_type, match_type, map_fn);
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
			let first_end = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::End);
			let last = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::End);
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
			let first = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::StartEnd);
			let last = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::End);
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
fn build_group_option_ns(e: &Value, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_no_save_start_end(inner_return_type, match_type, map_fn, position);
	let post = match position {
		PositionType::Start => quote!{ start = Default::default(); },
		PositionType::End => quote!{ end = Default::default(); },
		PositionType::StartEnd => quote!{ start = Default::default(); end = Default::default(); }
	};
	quote!{
		let src_old = src.clone();
		if #ret_lifetime: { #inner Ok(()) }.is_err() {
			*src = src_old; #post
		}
	}
}

/// Match as many repetitions of the group as possible. Once matching a repetition fails,
fn build_group_kleene(e: &Value, n: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, wrapped: bool, position: PositionType) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	match position {
		PositionType::Start => {
			let first = e.build_save_start_end(n.clone(), caller, inner_return_type, match_type, map_fn, wrapped, PositionType::Start);
			let inner = e.build_save(n.clone(), caller, inner_return_type, match_type, map_fn, wrapped);
			quote!{
				let src_old = src.clone();
				match #ret_lifetime: { #first } {
					Ok(t) => #n.push(t),
					_ => {
						*src = src_old;
						start = Default::default()
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
		PositionType::End => {
			let inner = e.build_save_start_end(n.clone(), caller, inner_return_type, match_type, map_fn, wrapped, PositionType::End);
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
		PositionType::StartEnd => {
			let first = e.build_save_start_end(n.clone(), caller, inner_return_type, match_type, map_fn, wrapped, PositionType::StartEnd);
			let inner = e.build_save_start_end(n.clone(), caller, inner_return_type, match_type, map_fn, wrapped, PositionType::End);
			quote!{
				let src_old = src.clone();
				match #ret_lifetime: { #first } {
					Ok(t) => #n.push(t),
					_ => {
						*src = src_old;
						start = Default::default();
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
fn build_group_positive(e: &Value, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	match position {
		PositionType::Start => {
			let first = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::Start);
			let inner = e.build_no_save(inner_return_type, match_type, map_fn);
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
			let first_end = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::End);
			let last = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::End);
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
			let first = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::StartEnd);
			let last = e.build_no_save_start_end(inner_return_type, match_type, map_fn, PositionType::End);
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
fn build_group_option(e: &Value, return_type: ReturnType, match_type: &Type, map_fn: &Option<ExprClosure>, position: PositionType) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_no_save_start_end(inner_return_type, match_type, map_fn, position);
	let post = match position {
		PositionType::Start => quote!{ start = Default::default(); },
		PositionType::End => quote!{ end = Default::default(); },
		PositionType::StartEnd => quote!{ start = Default::default(); end = Default::default(); }
	};
	quote!{
		let src_old = src.clone();
		if #ret_lifetime: { #inner Ok(()) }.is_err() {
			*src = src_old; #post
		}
	}
}

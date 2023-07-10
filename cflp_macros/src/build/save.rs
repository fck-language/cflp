#[allow(rustdoc::invalid_rust_codeblocks)]

use std::backtrace::Backtrace;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{ExprClosure, Ident, Pat, Path, Token, Type};
use syn::punctuated::Punctuated;
use crate::prelude::{Value, Group, ReturnType, SaveType, SplitRule};

impl Value {
	/// Builds a `Value` to a `TokenStream` and saves it
	pub(crate) fn build_save(&self, n: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &ExprClosure, wrapped: bool) -> TokenStream {
		match self {
			Value::Single(_) => unreachable!("Value::Single variant should be inaccessible under a save function\n{}", Backtrace::force_capture()),
			Value::Call(_) => unreachable!("Value::Call variant should be inaccessible under a save function\n{}", Backtrace::force_capture()),
			Value::Save { group: SaveType::Call(rule), boxed } => build_value_save_call(rule, return_type, *boxed),
			Value::Save { group: SaveType::Other(pat), .. } => build_value_save_other(pat, return_type),
			Value::Group(g, _) => g.build_save(n, caller, return_type, match_type, map_fn, wrapped)
		}
	}
}

impl Group {
	/// Builds a `Group` to a `TokenStream` and saves it
	pub(crate) fn build_save(&self, n: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &ExprClosure, wrapped: bool) -> TokenStream {
		let mut out = quote!{ let #n = };
		let inner = match self {
			Group::Literal(v, _) => v.build_save(n, caller, return_type, match_type, map_fn, wrapped),
			Group::Kleene(v, _) => {
				let n_ident_out = format_ident!("{}_out", n);
				let mut out = quote!{ let mut #n_ident_out = Vec::new(); };
				out.extend(kleene_inner(v, n.clone(), n_ident_out, caller, return_type, match_type, map_fn, wrapped));
				out
			}
			Group::Positive(v, _) => {
				// a+ == a a*
				let mut out = Group::Literal(v.clone(), true).build_save(format_ident!("{}_0", n), caller, return_type, match_type, map_fn, wrapped);
				let n_ident_out = format_ident!("{}_out", n);
				let n_ident0 = format_ident!("{}_0", n);
				out.extend(quote!{let mut #n_ident_out = vec![#n_ident0]; });
				out.extend(kleene_inner(v, n.clone(), n_ident_out, caller, return_type, match_type, map_fn, wrapped));
				out
			}
			Group::Option(v, _) => build_group_option(v, n.clone(), caller, return_type, match_type, map_fn, wrapped)
		};
		out.extend(quote!( { #inner } ));
		out
	}
}

impl SplitRule {
	/// Builds a `Value` to a `TokenStream` and saves it
	pub(crate) fn build_save(&self, n: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &ExprClosure, wrapped: bool) -> TokenStream {
		let inner_return_type = return_type.set_wrapped(false);
		let mut out = TokenStream::new();
		let mut k: usize;
		match self {
			SplitRule::Single(inner) => {
				if inner.contains_save() {
					out.extend(inner.build_save(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped));
					k = 1
				} else {
					out.extend(inner.build_no_save(inner_return_type, match_type, map_fn));
					k = 0
				}
			}
			SplitRule::Other { start, middle, end } => {
				if start.contains_save() {
					out.extend(start.build_save(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped));
					k = 1
				} else {
					out.extend(start.build_no_save(inner_return_type, match_type, map_fn));
					k = 0
				}
				for i in middle.iter() {
					if i.contains_save() {
						out.extend(i.build_save(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped));
						k += 1
					} else {
						out.extend(i.build_no_save(inner_return_type, match_type, map_fn));
					}
				}
				if end.contains_save() {
					out.extend(end.build_save(format_ident!("{}_{}", n, k), caller, inner_return_type, match_type, map_fn, wrapped));
					k += 1
				} else {
					out.extend(end.build_no_save(inner_return_type, match_type, map_fn));
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
fn kleene_inner(v: &Value, n: Ident, out: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &ExprClosure, wrapped: bool) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let lifetime = inner_return_type.get_lifetime();
	let match_inner = v.build_save(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped);
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
fn build_value_save_call(e: &Path, return_type: ReturnType, is_boxed: bool) -> TokenStream {
	let ok_ret = match (is_boxed, return_type.is_wrapped()) {
		(true, true) => quote!{ Ok(NodeWrapper{ node: Box::new(t.node), start: t.start, end: t.end }) },
		(true, false) => quote!{ NodeWrapper{ node: Box::new(t.node), start: t.start, end: t.end } },
		(false, true) => quote!{ Ok(t) },
		(false, false) => quote!{ t }
	};
	let ret = return_type.to_token_stream(quote!{Err(e)});
	quote!{
		match #e::parse(src) {
			Ok(t) => #ok_ret,
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
fn build_value_save_other(p: &Pat, return_type: ReturnType) -> TokenStream {
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
	
	let ok_arm = if returned_args.is_empty() {
		if return_type.is_wrapped() {
			quote!{ Some(next_match @ #p) => Ok(next_match) }
		} else {
			quote!{ Some(next_match @ #p) => next_match }
		}
	} else {
		match (returned_args.len(), return_type.is_wrapped()) {
			(1, false) => quote!{ Some(#p) => #(#returned_args.clone()),* },
			(1, true) => quote!{ Some(#p) => Ok(#(#returned_args.clone()),*) },
			(_, false) => quote!{ Some(#p) => (#(#returned_args.clone()),*) },
			(_, true) => quote!{ Some(#p) => Ok((#(#returned_args.clone()),*)) },
		}
	};
	
	let err = return_type.to_token_stream(expected(p));
	quote!{
		let next = src.next();
		match next {
			#ok_arm,
			_ => #err
		}
	}
}

/// ```rust
/// let src_old = src.clone();
/// match 'l: { v.build() } {
/// 	Ok(t) => Some(t),
/// 	Err(e) => { *src = src_old; None }
/// };
/// ```
fn build_group_option(e: &Value, n: Ident, caller: &Path, return_type: ReturnType, match_type: &Type, map_fn: &ExprClosure, wrapped: bool) -> TokenStream {
	let inner_return_type = return_type.new_lifetime(true);
	let ret_lifetime = inner_return_type.get_lifetime();
	let inner = e.build_save(format_ident!("{}_0", n), caller, inner_return_type, match_type, map_fn, wrapped);
	quote!{
		let src_old = src.clone();
		match #ret_lifetime: { #inner } {
			Ok(t) => Some(t),
			Err(e) => { *src = src_old; None }
		}
	}
}

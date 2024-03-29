#![doc=include_str!("../README.md")]
#![cfg_attr(
    docs,
    feature(doc_auto_cfg),
    deny(rustdoc::broken_intra_doc_links, missing_docs)
)]
#![cfg_attr(
    not(docs),
    warn(rustdoc::broken_intra_doc_links, missing_docs)
)]
mod prelude;
mod build;
mod lifetimes;

use proc_macro::TokenStream as pmTS;
use proc_macro2::{Ident, Span, TokenStream};
use std::collections::HashSet;
use quote::{format_ident, quote, ToTokens};
use syn::{parse, ItemEnum, ItemStruct, Lifetime, Error, Token, Path, PathSegment, punctuated::Punctuated, spanned::Spanned, Fields, PathArguments, AngleBracketedGenericArguments, GenericParam, GenericArgument, Variant};
use crate::{
	lifetimes::Lifetimes,
	prelude::{Meta, StructParserAttribute, MacroInner, RuleInner, Rules, ReturnType}
};
use crate::build::build_match_arm_err;
use crate::prelude::{Group, RuleInnerMatch, SplitRule};

/// Derive `PartialEq` for references so that `&Self: PartialEq<Self>`:
/// ```
/// # #[derive(PartialEq)]
/// # struct Example{}
/// impl PartialEq<Example> for &Example {
/// 	fn eq(&self, other: &Example) -> bool {
/// 		self == other
///		}
/// }
/// ```
///
/// Requires that `Self: PartialEq<Self>`
#[proc_macro_derive(PartialEqRef)]
pub fn derive_partial_eq_ref(raw_item: pmTS) -> pmTS {
	let name = if let Ok(item) = parse::<ItemStruct>(raw_item.clone()) {
		item.ident
	} else {
		match parse::<ItemEnum>(raw_item.clone()) {
			Ok(item) => item.ident,
			Err(err) => return pmTS::from(err.to_compile_error())
		}
	};
	pmTS::from(quote! {
		impl PartialEq<#name> for &#name {
			fn eq(&self, other: &#name) -> bool {
				self == other
			}
		}
	})
}

/// Derive macro for `cflp::Parser`
#[proc_macro_derive(Parser, attributes(parser))]
pub fn derive_parser(item: pmTS) -> pmTS {
	let input = match get_rules(item) {
		Ok(ok) => ok,
		Err(err) => return pmTS::from(err.to_compile_error().to_token_stream())
	};
	pmTS::from(build_impl(input.rules, input.meta))
}

/// Get the arguments supplied in the `#[parser(...)]` helper attribute on te item deriving `Parser`
fn get_rules(item: pmTS) -> Result<MacroInner, Error> {
	if let Ok(item) = parse::<ItemStruct>(item.clone()) {
		// match: #[parser(iter_type, cmp_type, tok_map, is_wrapped; match)]
		if let Some(attr) = item.attrs.iter().position(|attr| attr.path().get_ident().map(|i| i.to_string()) == Some("parser".to_string())) {
			let attr = &item.attrs[attr];
			let mut out = attr.parse_args::<StructParserAttribute>()?;
			let _self_arguments = if item.generics.lt_token.is_some() {
				let mut args = Vec::new();
				for i in item.generics.params.iter() {
					match i {
						GenericParam::Lifetime(l) => args.push(GenericArgument::Lifetime(l.lifetime.clone())),
						GenericParam::Type(c) => return Err(Error::new(c.span(), "Parser cannot be derived for types with type generics")),
						GenericParam::Const(c) => return Err(Error::new(c.span(), "Parser cannot be derived for types with constant generics"))
					}
				}
				PathArguments::AngleBracketed(AngleBracketedGenericArguments {
					colon2_token: None,
					lt_token: Default::default(),
					args: Punctuated::from_iter(args),
					gt_token: Default::default(),
				})
			} else { PathArguments::None };
			out.meta._self = PathSegment {
				ident: item.ident.clone(),
				arguments: _self_arguments
			};
			let rules = match &item.fields {
				Fields::Named(fields) => RuleInnerMatch::Named(
					fields.named.iter().map(|t| t.ident.clone().unwrap()).collect(),
					out.rule
				),
				Fields::Unnamed(_) => RuleInnerMatch::Unnamed(out.rule),
				Fields::Unit => return Err(Error::new(item.span(), "Cannot derive the Parser trait on a unit struct")),
			};
			let matches = rules.count_matches();
			if matches != item.fields.len() {
				return Err(Error::new(item.span(), format!("Attribute number of save groups does not match the number of fields: {}(Fields) != {}(Saves)", item.fields.len(), matches)))
			}
			Ok(MacroInner {
				meta: out.meta, rules: Rules::Single(RuleInner {
					name: Path::from(format_ident!("Self")),
					inner: rules
				}),
			})
		} else {
			return Err(Error::new(item.span(), "Parser derive requires a #[parser(...)] attribute"))
		}
	} else {
		let mut item = parse::<ItemEnum>(item.clone())?;
		let attr = if let Some(attr) = item.attrs.iter().position(|attr| attr.path().get_ident().map(|i| i.to_string()) == Some("parser".to_string())) {
			item.attrs.remove(attr)
		} else {
			return Err(Error::new(item.span(), "Parser derive requires a #[parser(...)] attribute"))
		};
		let mut meta = attr.parse_args::<Meta>()?;
		let _self_arguments = if item.generics.lt_token.is_some() {
			let mut args = Vec::new();
			for i in item.generics.params.iter() {
				match i {
					GenericParam::Lifetime(l) => args.push(GenericArgument::Lifetime(l.lifetime.clone())),
					GenericParam::Type(c) => return Err(Error::new(c.span(), "Parser cannot be derived for types with type generics")),
					GenericParam::Const(c) => return Err(Error::new(c.span(), "Parser cannot be derived for types with constant generics"))
				}
			}
			PathArguments::AngleBracketed(AngleBracketedGenericArguments {
				colon2_token: None,
				lt_token: Default::default(),
				args: Punctuated::from_iter(args),
				gt_token: Default::default(),
			})
		} else { PathArguments::None };
		meta._self = PathSegment {
			ident: item.ident.clone(),
			arguments: _self_arguments
		};
		// for each variant, parse the #[parser(...)] attribute as a RuleInnerMatch and add it to an accumulator
		let mut rules = Vec::new();
		for variant in item.variants.iter() {
			if let Some(p) = variant.attrs.iter().position(|t| t.path().get_ident().map(|i| i.to_string()) == Some("parser".to_string())) {
				let attr = &variant.attrs[p];
				let name = Path {
					leading_colon: None,
					segments: Punctuated::<_, Token![::]>::from_iter([PathSegment::from(item.ident.clone()), PathSegment::from(variant.ident.clone())]),
				};
				let inner = attr.parse_args_with(Punctuated::<Group, Token![,]>::parse_terminated)?;
				let mut inner = inner.iter().map(Clone::clone).collect::<Vec<_>>();
				let inner = if inner.len() == 1 {
					SplitRule::Single(Box::new(inner.remove(0)))
				} else {
					let last = inner.pop().unwrap().clone();
					let first = inner.remove(0).clone();
					SplitRule::Other { start: Box::new(first), end: Box::new(last), middle: inner.iter().map(Clone::clone).collect() }
				};
				let inner = match &variant.fields {
					Fields::Named(fields) => RuleInnerMatch::Named(
						fields.named.iter().map(|f| f.ident.clone().unwrap()).collect(),
						inner
					),
					_ => RuleInnerMatch::Unnamed(inner)
				};
				let inner_matches = inner.count_matches();
				if inner_matches != variant.fields.len() {
					return Err(Error::new(variant.span(), format!("Variant number of save groups ({}) does not match the number of fields of the variant ({})", inner_matches, variant.fields.len())))
				}
				rules.push(RuleInner { name, inner });
			} else {
				return Err(Error::new(variant.span(), "Enum variant must have a #[parser(...)] attribute macro to define the variant rule"))
			}
		}
		if rules.len() == 0 {
			Err(Error::new(item.span(), "Enum must have at least one field"))
		} else {
			let f = rules.remove(0);
			Ok(MacroInner { meta, rules: Rules::Multiple { first: f, rem: rules } })
		}
	}
}

/// Build the impl for the `Parser` trait
fn build_impl(r: Rules, meta: Meta) -> TokenStream {
	let mut lifetimes = HashSet::new();
	r.lifetimes(&meta.cmp_type, &mut lifetimes);
	
	let mut impl_lifetimes = lifetimes.clone();
	impl_lifetimes.remove(&Lifetime::new("'a", Span::mixed_site()));
	let impl_lifetimes = Punctuated::<_, Token![,]>::from_iter(impl_lifetimes);
	let _self = &meta._self;
	let ret_type = if let Some(ty) = &meta.wrapped {
		quote!{ cflp::NodeWrapper<#_self, #ty> }
	} else {
		quote!{ Self }
	};
	let lifetimes = Punctuated::<_, Token![,]>::from_iter(lifetimes);
	let _self = if !lifetimes.is_empty() {
		let _self = meta._self;
		quote!{ #_self<#lifetimes> }
	} else { meta._self.to_token_stream() };
	let inner_match_stream = match r {
		Rules::Single(inner) => {
			let (rule_inner, return_args) = inner.build(ReturnType::Function, &meta.cmp_type, meta.map, meta.wrapped.is_some());
			if meta.wrapped.is_some() {
				quote!{
					use cflp::NodeData;
					let mut start = Default::default(); let mut end = Default::default();
					#rule_inner;
					return Ok(cflp::NodeWrapper{ node: Self #return_args, start, end })
				}
			} else {
				quote!{
					#rule_inner;
					return Ok(Self #return_args)
				}
			}
		}
		Rules::Multiple { first, mut rem } => {
			// split into simple (single ident) and others
			let first_clone = first.clone();
			let mut single_match_rules = if let Some(t) = first_clone.is_singular() {
				vec![(0, t)]
			} else { vec![] };
			for (n, i) in rem.iter().enumerate() {
				if let Some(t) = i.is_singular() { single_match_rules.push((n + 1, t)) }
			}
			if single_match_rules.len() == rem.len() + 1 {
				// all rules are simple match rules
				let (_, (_, first_arm, _)) = single_match_rules.first().unwrap();
				let (err, err_wrapped) = build_match_arm_err(first_arm);
				let mut arms = Vec::new();
				for (_, (rule, pattern, saves)) in single_match_rules {
					arms.push(rule.match_arm(pattern, meta.wrapped.is_some(), saves));
				}
				let pre = if let Some(pos_type) = &meta.wrapped {
					let tok_type = &meta.tok_type;
					quote!{ let start = <#tok_type as cflp::NodeData<#pos_type>>::start(t_unwrapped); let end = <#tok_type as cflp::NodeData<#pos_type>>::end(t_unwrapped); }
				} else { quote!{} };
				let inner_match_expr = if meta.map {
					let cmp_type = &meta.cmp_type;
					quote!{ Into::<#cmp_type>::into(t_unwrapped) }
				} else { quote!{ t_unwrapped } };
				quote!{
					match src.next() {
						Some(t_unwrapped) => {
							#pre
							match #inner_match_expr {
								#(#arms,)*
								#err_wrapped
							}
						}
						#err
					}
				}
			} else {
				let inner_return_rule = ReturnType::Lifetime(0, false);
				let lifetime = inner_return_rule.get_lifetime();
				let (pre, ok) = if let Some(pos_type) = &meta.wrapped {
					(
						quote!{
							use cflp::NodeData;
							let mut start = <#pos_type as Default>::default();
							let mut end = <#pos_type as Default>::default();
						}, quote!{ cflp::NodeWrapper { node: t, start, end } }
					)
				} else { (quote!{}, quote!{ t }) };
				let first_match = if single_match_rules.len() > 1 {
					let (_, (_, first_arm, _)) = single_match_rules.first().unwrap();
					let (err, err_wrapped) = build_match_arm_err(first_arm);
					let mut arms = Vec::new();
					for (_, (rule, pattern, saves)) in single_match_rules.iter() {
						arms.push(rule.match_arm(pattern.clone(), meta.wrapped.is_some(), *saves));
					}
					let pre_match = if let Some(pos_type) = &meta.wrapped {
						let tok_type = &meta.tok_type;
						quote!{ let start = <#tok_type as cflp::NodeData<#pos_type>>::start(t_unwrapped); let end = <#tok_type as cflp::NodeData<#pos_type>>::end(t_unwrapped); }
					} else { quote!{} };
					let inner_match_expr = if meta.map {
						let cmp_type = &meta.cmp_type;
						quote!{ Into::<#cmp_type>::into(t_unwrapped) }
					} else { quote!{ t_unwrapped } };
					let ret = quote!{
						match match src.next() {
							Some(t_unwrapped) => {
								#pre_match
								match #inner_match_expr {
									#(#arms,)*
									#err_wrapped
								}
							}
							#err
						} {
							Ok(t) => return Ok(t),
							Err(e) => {
								first_err = e;
								*src = src_old;
							}
						}
					};
					let (first_simple_position, _) = single_match_rules.remove(0);
					let mut new_rem = if first_simple_position != 0 {
						vec![first]
					} else { Vec::new() };
					let mut j = first_simple_position;
					for (i, _) in single_match_rules {
						if i > j + 1 {
							new_rem.extend_from_slice(&rem[j..i])
						}
						j = i;
					}
					if rem.len() > j {
						new_rem.extend_from_slice(&rem[j..])
					}
					rem = new_rem;
					ret
				} else {
					let (mut rule_inner, return_args) = first.build(inner_return_rule, &meta.cmp_type, meta.map, meta.wrapped.is_some());
					let name = &first.name;
					rule_inner.extend(quote!{; break #lifetime Ok(#name #return_args)});
					
					quote!{
						match #lifetime: { #rule_inner } {
							Ok(t) => return Ok(#ok),
							Err(e) => {
								first_err = e;
								*src = src_old;
							}
						}
					}
				};
				
				let mut local_out = quote!{
					#pre
					let first_err;
					let src_old = src.clone();
					#first_match
				};
				for rule in rem.iter() {
					let (mut rule_inner, return_args) = rule.build(inner_return_rule, &meta.cmp_type, meta.map, meta.wrapped.is_some());
					let name = &rule.name;
					rule_inner.extend(quote!{
						; break #lifetime Ok(#name #return_args)
					});
					let lifetime = inner_return_rule.get_lifetime();
					local_out.extend(quote!{
						let src_old = src.clone();
						match #lifetime: { #rule_inner } {
							Ok(t) => return Ok(#ok),
							Err(_) => *src = src_old
						}
					});
				}
				local_out.extend(quote!{ return Err(first_err) });
				local_out
			}
		}
	};
	let tok_type = meta.tok_type;
	let cmp_type = meta.cmp_type;
	quote!{
		#[automatically_derived]
		impl <'a, #impl_lifetimes> cflp::Parser<&'a #tok_type, #cmp_type, #ret_type> for #_self {
			fn parse_with_recursion<T: Iterator<Item=&'a #tok_type> + Clone>(src: &mut T, recurse: bool) -> Result<#ret_type, cflp::Error<&'a #tok_type, #cmp_type>> {
				#inner_match_stream
			}
		}
	}
}

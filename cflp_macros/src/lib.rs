//! # cflp macros
//!
//! Derive macro for `cflp::Parser` trait
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
use proc_macro2::{Span, TokenStream};
use std::collections::HashSet;
use quote::{format_ident, quote, ToTokens};
use syn::{parse, ItemEnum, ItemStruct, Lifetime, Error, Token, Path, PathSegment, punctuated::Punctuated, spanned::Spanned, Fields};
use crate::{
	lifetimes::Lifetimes,
	prelude::{Meta, StructParserAttribute, MacroInner, RuleInner, Rules, ReturnType}
};
use crate::prelude::{Group, RuleInnerMatch, SplitRule};

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
			out.meta._self = item.ident.clone();
			let rules = match &item.fields {
				Fields::Named(fields) => RuleInnerMatch::Named(
					fields.named.iter().map(|t| t.ident.clone().unwrap()).collect(),
					out.rule
				),
				Fields::Unnamed(_) => RuleInnerMatch::Unnamed(out.rule),
				Fields::Unit => return Err(Error::new(item.span(), "Cannot derive the Parser trait on a unit struct")),
			};
			if rules.count_matches() != item.fields.len() {
				return Err(Error::new(attr.span(), "Attribute number of save groups does not match the number of fields"))
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
		meta._self = item.ident.clone();
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
				let mut inner = inner.iter().collect::<Vec<_>>();
				let inner = if inner.len() == 1 {
					SplitRule::Single(Box::new(inner[0].clone()))
				} else {
					let last = inner.pop().unwrap().clone();
					let first = inner.remove(0).clone();
					SplitRule::Other { start: Box::new(first), end: Box::new(last), middle: inner.iter().map(|t| t.clone().clone()).collect() }
				};
				let inner = match &variant.fields {
					Fields::Named(fields) => RuleInnerMatch::Named(
						fields.named.iter().map(|f| f.ident.clone().unwrap()).collect(),
						inner
					),
					_ => RuleInnerMatch::Unnamed(inner)
				};
				if inner.count_matches() != variant.fields.len() {
					return Err(Error::new(variant.span(), "Variant number of save groups does not match the number of fields of the variant"))
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
			let (rule_inner, return_args) = inner.build(ReturnType::Function, &meta.cmp_type, &meta.map_fn, meta.wrapped.is_some());
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
		Rules::Multiple { first, rem } => {
			let inner_return_rule = ReturnType::Lifetime(0, false);
			let (mut rule_inner, return_args) = first.build(inner_return_rule, &meta.cmp_type, &meta.map_fn, meta.wrapped.is_some());
			
			let lifetime = inner_return_rule.get_lifetime();
			let name = &first.name;
			rule_inner.extend(quote!{; break #lifetime Ok(#name #return_args)});
			
			let (pre, ok) = if meta.wrapped.is_some() {
				(
					quote!{
						use cflp::NodeData;
						let mut start = Default::default();
						let mut end = Default::default();
					}, quote!{
						NodeWrapper { node: t, start, end }
					}
				)
			} else { (quote!{}, quote!{ t }) };
			let mut local_out = quote!{
				#pre
				let first_err;
				let src_old = src.clone();
				match #lifetime: { #rule_inner } {
					Ok(t) => return Ok(#ok),
					Err(e) => {
						first_err = e;
						*src = src_old;
					}
				}
			};
			for rule in rem.iter() {
				let (mut rule_inner, return_args) = rule.build(inner_return_rule, &meta.cmp_type, &meta.map_fn, meta.wrapped.is_some());
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
			local_out.extend(quote!{return Err(first_err)});
			local_out
		}
	};
	let tok_type = meta.tok_type;
	let cmp_type = meta.cmp_type;
	quote!{
		#[automatically_derived]
		impl <'a, #impl_lifetimes> cflp::Parser<&'a #tok_type, #cmp_type, #ret_type> for #_self {
			fn parse<T: Iterator<Item=&'a #tok_type> + Clone>(src: &mut T) -> Result<#ret_type, cflp::Error<&'a #tok_type, #cmp_type>> {
				
				#inner_match_stream
			}
		}
	}
}

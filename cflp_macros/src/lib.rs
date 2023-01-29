mod prelude;
mod parser;
mod build;
mod saving;
mod lifetimes;

use proc_macro::{Delimiter, TokenStream, Spacing, Span};
use std::collections::HashSet;
use quote::ToTokens;
use syn::{parse_macro_input, Visibility, parse, ItemEnum, ItemStruct, Type, Lifetime, parse2};
use syn::spanned::Spanned;
use crate::lifetimes::Lifetimes;
use crate::prelude::{MacroInner, ReturnType, Rule, RuleInner, RuleInnerEnum};
use crate::prelude::no_types::{MacroInnerNoGen, MacroInnerAttr, MacroInnerAttrMeta};

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))}; }
macro_rules! puncj { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Joint))}; }

/// This macro generates types and implements the `Parser` trait for those types.\
/// For help on the macro usage, see the [documentation](https://fck-language.github.io/cflp)
#[proc_macro]
pub fn rule(t: TokenStream) -> TokenStream {
	let data = parse_macro_input!(t as MacroInner);
	let mut out = TokenStream::from_iter(vec![
		ident!("use"), ident!("cflp"), puncj!(':'), punc!(':'), ident!("Parser"), punc!(';')]);
	for r in data.rules.0.iter() {
		out.extend(build_type(r, &data.meta.struct_vis, &data.meta.comp_type, data.meta.derived_traits.clone()));
		out.extend(build_impl(
			r, &data.meta.tok_type,
			&data.meta.comp_type,
			&TokenStream::from(data.meta.map_fn.to_token_stream())
		))
	}
	out
}

/// This macro implements the `Parser` trait for the given rules. It does not
/// generate any types.\
/// For help on the macro usage, see the [documentation](https://fck-language.github.io/cflp)
#[proc_macro]
pub fn rule_no_types(t: TokenStream) -> TokenStream {
	let data = parse_macro_input!(t as MacroInnerNoGen);
	let mut out = TokenStream::from_iter(vec![
		ident!("use"), ident!("cflp"), puncj!(':'), punc!(':'), ident!("Parser"), punc!(';')]);
	let map_fn = TokenStream::from(data.meta.map_fn.to_token_stream());
	for r in data.rules.0.iter() {
		out.extend(build_impl(r, &data.meta.tok_type, &data.meta.comp_type, &map_fn))
	}
	out
}

/// Attribute macro to generate a `Parser` impl for the given type.\
/// For help on the macro usage, see the [documentation](https://fck-language.github.io/cflp)
#[proc_macro_attribute]
pub fn parser(attrs: TokenStream, item: TokenStream) -> TokenStream {
	// check for enum or struct
	let (meta, rule, mut item) = if let Ok(i) = parse::<ItemStruct>(item.clone()) {
		let macro_inner = parse_macro_input!(attrs as MacroInnerAttr);
		if let RuleInnerEnum::Multiple(_) = macro_inner.rule {
			return TokenStream::from(syn::Error::new(i.span(), "Cannot apply a rule with multiple variants to a struct").to_compile_error())
		}
		(macro_inner.meta, Rule { name: i.ident, inner: macro_inner.rule }, item)
	} else {
		let i = match parse::<ItemEnum>(item.clone()) {
			Ok(i) => i,
			Err(e) => return TokenStream::from(e.to_compile_error())
		};
		let mut new_item = i.clone();
		new_item.variants.clear();
		let meta = parse_macro_input!(attrs as MacroInnerAttrMeta);
		let mut rules = Vec::new();
		for mut variant in i.variants {
			if let Some(p) = variant.attrs.iter().position(|t| t.path.get_ident() == Some(&syn::Ident::new("parser", Span::mixed_site().into()))) {
				let attr = variant.attrs.remove(p);
				match parse2::<RuleInner>(attr.tokens.clone()) {
					Ok(mut r) => {
						r.name = Some(variant.ident.clone());
						rules.push(r)
					}
					Err(e) => return TokenStream::from(e.to_compile_error())
				}
				new_item.variants.push(variant);
			} else {
				return TokenStream::from(syn::Error::new(variant.span(), "Enum variant must have a parser attribute macro").to_compile_error())
			}
		}
		(meta, Rule { name: i.ident, inner: RuleInnerEnum::Multiple(rules) }, TokenStream::from(new_item.to_token_stream()))
	};
	let map_fn = TokenStream::from(meta.map_fn.to_token_stream());
	item.extend(build_impl(&rule, &meta.tok_type, &meta.comp_type, &map_fn));
	item
}

fn build_type(r: &Rule, vis: &Visibility, comp: &Type, traits: TokenStream) -> TokenStream {
	let mut out = if traits.is_empty() {
		TokenStream::new()
	} else {
		TokenStream::from_iter(vec![
			punc!('#'), group!(Delimiter::Bracket, vec![
				ident!("derive"), group!(Delimiter::Parenthesis; traits.clone())
			]),
		])
	};
	out.extend(r.type_gen(comp, vis));
	out
}

fn build_impl(r: &Rule, tok: &Type, comp_type: &Type, map_fn: &TokenStream) -> TokenStream {
	let mut lifetimes = HashSet::new();
	r.lifetimes(comp_type, &mut lifetimes);
	let mut impl_lifetimes = lifetimes.clone();
	impl_lifetimes.remove(&Lifetime::new("'a", Span::mixed_site().into()));
	let mut out = TokenStream::from_iter(vec![
		ident!("impl"), punc!('<'), puncj!('\''), ident!("a")
	]);
	for l in impl_lifetimes {
		out.extend(Some(punc!(',')));
		out.extend(TokenStream::from(l.to_token_stream()));
	}
	out.extend(vec![
		punc!('>'), ident!("Parser"), punc!('<'), punc!('&'), puncj!('\''), ident!("a")
	]);
	out.extend(TokenStream::from(tok.to_token_stream()));
	out.extend(Some(punc!(',')));
	out.extend(TokenStream::from(comp_type.to_token_stream()));
	out.extend(vec![punc!('>'), ident!("for")]);
	out.extend(TokenStream::from(r.name.to_token_stream()));
	if !lifetimes.is_empty() {
		out.extend(Some(punc!('<')));
		for l in lifetimes {
			out.extend(TokenStream::from(l.to_token_stream()));
			out.extend(Some(punc!(',')));
		}
		out.extend(Some(punc!('>')));
	}
	let inner_match_stream = match &r.inner {
		RuleInnerEnum::Single(inner) => {
			let (mut rule_inner, return_args) = inner.build(&r.name, ReturnType::Function, comp_type, map_fn);
			rule_inner.extend(vec![
				ident!("return"), ident!("Ok"), group!(Delimiter::Parenthesis, vec![
					ident!("Self"), return_args
				]),
			]);
			rule_inner
		}
		RuleInnerEnum::Multiple(inners) => {
			let inner_return_rule = ReturnType::Lifetime(0, false);
			let mut iter = inners.iter().enumerate();
			let (count, rule) = iter.next().unwrap();
			let (mut rule_inner, return_args) = rule.build(&r.name, inner_return_rule, comp_type, map_fn);
			rule_inner.extend(vec![
				punc!(';'), ident!("break"),
			]);
			rule_inner.extend(inner_return_rule.get_lifetime());
			
			// expression inside the final `break Ok` statement
			let mut ok_inner = TokenStream::from_iter(vec![
				ident!("Self"), puncj!(':'), punc!(':'),
			]);
			if let Some(name) = &rule.name {
				ok_inner.extend(TokenStream::from(name.to_token_stream()))
			} else {
				ok_inner.extend(Some(ident!(&*format!("Var{}", count + 1))))
			}
			ok_inner.extend(Some(return_args));
			
			rule_inner.extend(vec![
				ident!("Ok"), group!(Delimiter::Parenthesis; ok_inner),
			]);
			let mut local_out = TokenStream::from_iter(vec![
				ident!("let"), ident!("first_err"), punc!(';'),
				ident!("let"), ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';'),
				ident!("match"),
			]);
			local_out.extend(inner_return_rule.get_lifetime());
			local_out.extend(TokenStream::from_iter(vec![
				punc!(':'), group!(Delimiter::Brace; rule_inner), group!(Delimiter::Brace, vec![
					ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), puncj!('='), punc!('>'), ident!("return"), ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), punc!(','),
					ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("e"))), puncj!('='), punc!('>'), group!(Delimiter::Brace, vec![
						ident!("first_err"), punc!('='), ident!("e"), punc!(';'),
						punc!('*'), ident!("src"), punc!('='), ident!("src_old")
					])
				]),
			]));
			for (count, rule) in iter {
				// let src_old = src.clone();
				// match loop { $rule_inner break Ok(Self::Var${count+1}(return_args) } {
				//     Ok(t) => return Ok(t),
				//     Err(_) => *src = src_old
				// }
				local_out.extend(vec![
					ident!("let"), ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';'),
					ident!("match"),
				]);
				local_out.extend(inner_return_rule.get_lifetime());
				let (mut rule_inner, return_args) = rule.build(&r.name, inner_return_rule, comp_type, map_fn);
				rule_inner.extend(vec![
					punc!(';'), ident!("break"),
				]);
				rule_inner.extend(inner_return_rule.get_lifetime());
				let mut ok_inner = TokenStream::from_iter(vec![ident!("Self"), puncj!(':'), punc!(':')]);
				if let Some(name) = &rule.name {
					ok_inner.extend(TokenStream::from(name.to_token_stream()))
				} else {
					ok_inner.extend(Some(ident!(&*format!("Var{}", count + 1))))
				}
				ok_inner.extend(Some(return_args));
				rule_inner.extend(vec![
					ident!("Ok"), group!(Delimiter::Parenthesis; ok_inner)
				]);
				local_out.extend(TokenStream::from_iter(vec![
					punc!(':'), group!(Delimiter::Brace; rule_inner), group!(Delimiter::Brace, vec![
						ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), puncj!('='), punc!('>'), ident!("return"), ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), punc!(','),
						ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("_"))), puncj!('='), punc!('>'), punc!('*'), ident!("src"), punc!('='), ident!("src_old")
					]),
				]));
			}
			local_out.extend(vec![
				ident!("return"), ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("first_err"))),
			]);
			local_out
		}
	};
	let mut impl_fn = TokenStream::from_iter(vec![
		ident!("fn"), ident!("parse"), punc!('<'),
		ident!("T"), punc!(':'), ident!("Iterator"), punc!('<'), ident!("Item"), punc!('='),
		punc!('&'), puncj!('\''), ident!("a"),
	]);
	impl_fn.extend(TokenStream::from(tok.to_token_stream()));
	impl_fn.extend(vec![
		punc!('>'), punc!('+'), ident!("Clone"), punc!('>'), group!(Delimiter::Parenthesis, vec![
		ident!("src"), punc!(':'), punc!('&'), ident!("mut"), ident!("T")
	]), puncj!('-'), punc!('>'), ident!("Result"), punc!('<'), ident!("Self"), punc!(','),
		ident!("cflp"), puncj!(':'), punc!(':'), ident!("Error"), punc!('<'), punc!('&'), puncj!('\''), ident!("a"),
	]);
	impl_fn.extend(TokenStream::from(tok.to_token_stream()));
	impl_fn.extend(Some(punc!(',')));
	impl_fn.extend(TokenStream::from(comp_type.to_token_stream()));
	impl_fn.extend(vec![punc!('>'), punc!('>'), group!(Delimiter::Brace; inner_match_stream)]);
	out.extend(Some(group!(Delimiter::Brace; impl_fn)));
	out
}

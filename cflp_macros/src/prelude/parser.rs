//! # Parser impls
//!
//! [`Parse`](Parse) impls for types in [`mod.rs`](crate::prelude)

use proc_macro2::{Ident, Span};
use syn::parse::{Parse, ParseStream};
use syn::{bracketed, parenthesized, Token};
use syn::punctuated::Punctuated;
use crate::prelude::{Meta, Value, Group, StructParserAttribute, SaveType, SplitRule};

impl Parse for Meta {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let tok_type = input.parse()?;
		input.parse::<Token![,]>()?;
		let cmp_type = input.parse()?;
		input.parse::<Token![,]>()?;
		let map_fn = input.parse()?;
		let wrapped = if input.peek(Token![,]) {
			input.parse::<Token![,]>()?;
			Some(input.parse()?)
		} else { None };
		Ok(Self { tok_type, cmp_type, map_fn, wrapped, _self: Ident::new("Self", Span::mixed_site()) })
	}
}

impl Parse for StructParserAttribute {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let meta = input.parse()?;
		input.parse::<Token![;]>()?;
		let rule = Punctuated::<Group, Token![,]>::parse_separated_nonempty(input)?;
		let mut rule_vec: Vec<_> = rule.iter().map(|t| t.clone()).collect();
		let rule = if rule.len() == 1 {
			SplitRule::Single(Box::new(rule_vec.pop().unwrap()))
		} else {
			let end = rule_vec.pop().unwrap();
			let start = rule_vec.remove(0);
			SplitRule::Other { start: Box::new(start), end: Box::new(end), middle: rule_vec }
		};
		Ok(Self { meta, rule })
	}
}

impl Parse for Value {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(syn::token::Paren) {
			let content;
			parenthesized!(content in input);
			let inner = content.parse_terminated(Group::parse, Token![,])?;
			if input.is_empty() { return Err(syn::Error::new(input.span(), "Group must contain at least one element")) }
			let has_ret = inner.iter().map(|t| t.contains_save()).position(|t| t).is_some();
			let mut inner: Vec<_> = inner.iter().map(|t| t.clone()).collect();
			let inner = if inner.len() == 1 {
				SplitRule::Single(Box::new(inner.pop().unwrap()))
			} else {
				let end = inner.pop().unwrap();
				let start = inner.remove(0);
				SplitRule::Other { start: Box::new(start), end: Box::new(end), middle: inner }
			};
			Ok(Value::Group(inner, has_ret))
		} else if input.peek(syn::token::Bracket) {
			let content;
			bracketed!(content in input);
			if content.peek(syn::token::Bracket) {
				let inner_content;
				bracketed!(inner_content in content);
				Ok(Value::Save {
					group: inner_content.parse()?,
					boxed: true,
				})
			} else {
				Ok(Value::Save {
					group: content.parse()?,
					boxed: false,
				})
			}
		} else if input.peek(Token![@]) {
			input.parse::<Token![@]>()?;
			Ok(Value::Call(input.parse()?))
		} else {
			Ok(Value::Single(input.parse()?))
		}
	}
}

impl Parse for Group {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let inner: Value = input.parse()?;
		let contains_save = inner.contains_save();
		if input.peek(Token![*]) {
			input.parse::<Token![*]>()?;
			Ok(Group::Kleene(inner, contains_save))
		} else if input.peek(Token![+]) {
			input.parse::<Token![+]>()?;
			Ok(Group::Positive(inner, contains_save))
		} else if input.peek(Token![?]) {
			input.parse::<Token![?]>()?;
			Ok(Group::Option(inner, contains_save))
		} else {
			Ok(Group::Literal(inner, contains_save))
		}
		
	}
}

impl Parse for SaveType {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(Token![@]) {
			input.parse::<Token![@]>()?;
			Ok(Self::Call(input.parse()?))
		} else {
			Ok(Self::Other(syn::Pat::parse_single(input)?))
		}
	}
}

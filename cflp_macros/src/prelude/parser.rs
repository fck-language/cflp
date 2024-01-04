//! # Parser impls
//!
//! [`Parse`](syn::Parse) impls for types in [`mod.rs`](crate::prelude)

use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{bracketed, parenthesized, Pat, PathSegment, Token, Type};
use syn::punctuated::Punctuated;
use crate::prelude::{Meta, Value, Group, StructParserAttribute, SaveType};

impl Parse for Meta {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let tok_type: Type = input.parse()?;
		input.parse::<Token![,]>()?;
		let cmp_type: Type = input.parse()?;
		input.parse::<Token![,]>()?;
		let scope: Type = input.parse()?;
		let map = tok_type.to_token_stream().to_string() != cmp_type.to_token_stream().to_string();
		
		let wrapped = if input.peek(Token![,]) {
			input.parse::<Token![,]>()?;
			Some(input.parse()?)
		} else { None };
		Ok(Self { tok_type, cmp_type, scope, map, is_wrapped: wrapped.is_some(), wrapped, _self: PathSegment::from(Ident::new("Self", Span::mixed_site())), where_stms: None })
	}
}

impl Parse for StructParserAttribute {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let meta = input.parse()?;
		input.parse::<Token![;]>()?;
		let rule = Punctuated::<Group, Token![,]>::parse_separated_nonempty(input)?;
		let rule_vec: Vec<_> = rule.iter().map(Clone::clone).collect();
		Ok(Self { meta, rule: rule_vec.into() })
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
			let inner: Vec<_> = inner.iter().map(Clone::clone).collect();
			Ok(Value::Group(inner.into(), has_ret))
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
			Ok(Value::Single(Pat::parse_multi(input)?))
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
			let pattern = Pat::parse_multi(input)?;
			let default = if input.peek(Token![;]) {
				input.parse::<Token![;]>()?;
				Some(input.parse()?)
			} else { None };
			Ok(Self::Other { pattern, default })
		}
	}
}

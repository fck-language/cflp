//! [`Parse`][syn::parse::Parse] struct impls for [Group], [MacroInner], [Meta], [Rule], [Rules],
//! and [Value]

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{bracketed, Expr, Ident, parenthesized};
use crate::prelude::{Group, MacroInner, Meta, Rule, Rules, SaveType, SaveUnwrapType, Value};
use syn::parse::{Parse, ParseStream};
use syn::Token;

impl Parse for MacroInner {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let meta = input.parse()?;
		let rules = input.parse()?;
		Ok(Self { meta, rules })
	}
}

impl Parse for Meta {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let content;
		parenthesized!(content in input);
		macro_rules! parse_next {
		    ($e: ident) => {let $e = content.parse()?;};
		    ($e: ty) => {content.parse::<$e>()?;};
		}
		parse_next!(struct_vis);
		parse_next!(Token![,]);
		parse_next!(tok_type);
		parse_next!(Token![,]);
		parse_next!(comp_type);
		parse_next!(Token![,]);
		parse_next!(map_fn);
		let derived_traits = if content.parse::<Token![,]>().is_ok() {
			let trait_content;
			parenthesized!(trait_content in content);
			let trait_content = trait_content.parse_terminated::<_, Token![,]>(Ident::parse)?;
			TokenStream::from(trait_content.to_token_stream())
		} else {
			TokenStream::new()
		};
		Ok(Self { struct_vis, tok_type, comp_type, map_fn, derived_traits })
	}
}

impl Parse for Rule {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let content;
		parenthesized!(content in input);
		let name = content.parse()?;
		content.parse::<Token![;]>()?;
		let mut inner = vec![content.parse()?];
		if !content.is_empty() { content.parse::<Token![,]>()?; }
		loop {
			if content.is_empty() { break }
			inner.push(content.parse()?);
			if content.is_empty() { break }
			content.parse::<Token![,]>()?;
		}
		Ok(Self { name, inner })
	}
}

impl Parse for Rules {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let mut out = Vec::new();
		loop {
			if input.is_empty() { break }
			out.push(input.parse()?)
		}
		Ok(Self(out))
	}
}

impl Parse for Value {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(syn::token::Paren) {
			let content;
			parenthesized!(content in input);
			let inner = content.parse_terminated::<_, Token![,]>(Group::parse)?;
			let has_ret = inner.iter().map(|t| t.contains_save()).position(|t| t).is_some();
			let inner = inner.iter().map(|t| t.clone());
			Ok(Value::Group(inner.collect(), has_ret))
		} else if input.peek(syn::token::Bracket) {
			let content;
			bracketed!(content in input);
			Ok(Value::Save(content.parse()?))
		} else if input.peek(Token![@]) {
			input.parse::<Token![@]>()?;
			Ok(Value::Call(input.parse()?))
		} else {
			Ok(Value::Single(input.parse()?))
		}
	}
}

impl Parse for SaveType {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(Token![@]) {
			input.parse::<Token![@]>()?;
			return Ok(SaveType::Call(input.parse()?))
		}
		let save_type = input.parse::<Expr>()?;
		if input.peek(Token![;]) {
			input.parse::<Token![;]>()?;
			let inner = input.parse_terminated::<_, Token![,]>(SaveUnwrapType::parse)?;
			Ok(SaveType::Unwrapping(save_type, inner.iter().map(|t| t.clone()).collect()))
		} else {
			Ok(SaveType::Literal(save_type))
		}
	}
}

impl Parse for SaveUnwrapType {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(Token![_]) {
			input.parse::<Token![_]>()?;
			Ok(Self::Ignore)
		} else {
			Ok(Self::Use(input.parse()?))
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

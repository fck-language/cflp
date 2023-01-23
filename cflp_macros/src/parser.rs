//! [`Parse`][syn::parse::Parse] struct impls for [Group], [MacroInner], [Meta], [Rule], [Rules],
//! and [Value]

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{bracketed, Expr, Ident, parenthesized};
use crate::prelude::{Group, MacroInner, Meta, Rule, RuleInner, RuleInnerEnum, Rules, SaveType, SaveUnwrapType, Value};
use syn::parse::{Parse, ParseStream};
use syn::Token;
use crate::prelude::no_types::{MacroInnerAttr, MacroInnerNoGen, MetaNoGen};

impl Parse for MacroInner {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let meta = input.parse()?;
		let rules = input.parse()?;
		Ok(Self { meta, rules })
	}
}

impl Parse for MacroInnerNoGen {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let meta = input.parse()?;
		let rules = input.parse()?;
		Ok(Self { meta, rules })
	}
}

impl Parse for MacroInnerAttr {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let tok_type = input.parse()?;
		input.parse::<Token![,]>()?;
		let comp_type = input.parse()?;
		input.parse::<Token![,]>()?;
		let map_fn = input.parse()?;
		input.parse::<Token![;]>()?;
		let rule = input.parse()?;
		Ok(Self { tok_type, comp_type, map_fn, rule })
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

impl Parse for MetaNoGen {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let content;
		parenthesized!(content in input);
		macro_rules! parse_next {
		    ($e: ident) => {let $e = content.parse()?;};
		    ($e: ty) => {content.parse::<$e>()?;};
		}
		parse_next!(tok_type);
		parse_next!(Token![,]);
		parse_next!(comp_type);
		parse_next!(Token![,]);
		parse_next!(map_fn);
		Ok(Self { tok_type, comp_type, map_fn })
	}
}

impl Parse for Rule {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let content;
		parenthesized!(content in input);
		let name = content.parse()?;
		content.parse::<Token![;]>()?;
		Ok(Self { name, inner: content.parse()? })
	}
}

impl Parse for RuleInner {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let name = if input.peek2(Token![=>]) {
			let name = input.parse()?;
			input.parse::<Token![=>]>()?;
			name
		} else {
			None
		};
		let mut inner = vec![input.parse()?];
		if !input.is_empty() && !input.peek(Token![;]) { input.parse::<Token![,]>()?; }
		loop {
			if input.is_empty() || input.peek(Token![;]) { break }
			inner.push(input.parse()?);
			if input.is_empty() || input.peek(Token![;]) { break }
			input.parse::<Token![,]>()?;
		}
		Ok(Self{ name, inner })
	}
}

impl Parse for RuleInnerEnum {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		let inner = input.parse()?;
		if input.is_empty() {
			Ok(Self::Single(inner))
		} else {
			let mut inner_all = vec![inner];
			loop {
				if input.is_empty() { break }
				input.parse::<Token![;]>()?;
				if input.is_empty() { break }
				inner_all.push(input.parse()?);
			}
			Ok(Self::Multiple(inner_all))
		}
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

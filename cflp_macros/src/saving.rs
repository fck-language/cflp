//! Save rule inner types and `Parser` impls

use proc_macro::{TokenStream, TokenTree};
use quote::ToTokens;
use syn::{Ident, Lifetime, Lit, Token, Type};
use syn::parse::{Parse, ParseStream};

#[derive(Clone)]
pub(crate) enum SaveType {
	Call(Ident, Vec<Lifetime>),
	Match(Type, Vec<MatchArg>)
}

#[derive(Clone)]
pub(crate) enum MatchArg {
	Literal(Lit),
	Ignore,
	Type(Type)
}

impl MatchArg {
	pub fn is_type(&self) -> bool {
		if let MatchArg::Type(_) = self { true } else { false }
	}
	
	pub fn inner_match_arm(&self, arg_index: &mut u8, returned_args: &mut Vec<TokenTree>) -> TokenStream {
		macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, proc_macro::Span::mixed_site()))}; }
		macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, proc_macro::Spacing::Alone))}; }
		match self {
			MatchArg::Literal(e) => TokenStream::from(e.to_token_stream()),
			MatchArg::Ignore => TokenStream::from(ident!("_")),
			MatchArg::Type(_) => {
				let out = ident!(&*format!("m_{}", arg_index));
				*arg_index += 1;
				returned_args.extend(vec![out.clone(), punc!(',')]);
				TokenStream::from(out)
			}
		}
	}
}

impl Parse for SaveType {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(Token![@]) {
			input.parse::<Token![@]>()?;
			let ident = input.parse()?;
			if input.is_empty() {
				Ok(Self::Call(ident, Vec::new()))
			} else {
				let mut lifetimes = Vec::new();
				input.parse::<Token![<]>()?;
				while !input.peek(Token![>]) {
					lifetimes.push(input.parse()?);
					if input.peek(Token![>]) { break }
					input.parse::<Token![,]>()?;
				}
				input.parse::<Token![>]>()?;
				Ok(Self::Call(ident, lifetimes))
			}
		} else {
			let ident = input.parse()?;
			let args = if input.peek(Token![;]) {
				input.parse::<Token![;]>()?;
				input.parse_terminated::<_, Token![,]>(MatchArg::parse)?.iter().map(|t| t.clone()).collect()
			} else {
				Vec::new()
			};
			Ok(Self::Match(ident, args))
		}
	}
}

impl Parse for MatchArg {
	fn parse(input: ParseStream) -> syn::Result<Self> {
		if input.peek(Token![_]) {
			input.parse::<Token![_]>()?;
			return Ok(Self::Ignore)
		}
		let previous = input.clone();
		if let Ok(e) = input.parse::<Lit>() {
			Ok(Self::Literal(e))
		} else {
			Ok(Self::Type(previous.parse()?))
		}
	}
}

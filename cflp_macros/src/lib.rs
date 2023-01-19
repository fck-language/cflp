mod prelude;
mod parser;
mod build;

use proc_macro::{Delimiter, TokenStream, Spacing, Span};
use quote::ToTokens;
use syn::parse_macro_input;
use crate::prelude::MacroInner;

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))}; }
macro_rules! puncj { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Joint))}; }

/// This macro generates parser structs that can be used to generate an AST from a
/// context-free-grammar. For how to use this, you should read the book
#[proc_macro]
pub fn rule(t: TokenStream) -> TokenStream {
    let inner = parse_macro_input!(t as MacroInner);
	// impl_fn is the function header for Parser::parse
	let mut impl_fn = TokenStream::from_iter(vec![
		ident!("fn"), ident!("parse"), punc!('<'),
		ident!("T"), punc!(':'), ident!("Iterator"), punc!('<'), ident!("Item"), punc!('='),
		punc!('&'), puncj!('\''), ident!("a")
	]);
	impl_fn.extend(TokenStream::from(inner.meta.tok_type.to_token_stream()));
	impl_fn.extend(vec![
		punc!('>'), punc!('+'), ident!("Clone"), punc!('>'), group!(Delimiter::Parenthesis, vec![
			ident!("mut"), ident!("src"), punc!(':'), punc!('&'), ident!("mut"), ident!("T")
		]), puncj!('-'), punc!('>'), ident!("Result"), punc!('<'), ident!("Self"), punc!(','),
		ident!("cflp"), puncj!(':'), punc!(':'), ident!("Error"), punc!('<'), punc!('&'), puncj!('\''), ident!("a")
	]);
	impl_fn.extend(TokenStream::from(inner.meta.tok_type.to_token_stream()));
	impl_fn.extend(Some(punc!(',')));
	impl_fn.extend(TokenStream::from(inner.meta.comp_type.to_token_stream()));
	impl_fn.extend(vec![punc!('>'), punc!('>')]);
	
    let mut out = TokenStream::from_iter(vec![
		ident!("use"), ident!("cflp"), puncj!(':'), punc!(':'), ident!("Parser"), punc!(';')]);
    // iterate through each rule and generate the struct and Parser impl
	let comp_type = TokenStream::from(inner.meta.comp_type.to_token_stream());
	let map_fn = TokenStream::from(inner.meta.map_fn.to_token_stream());
    for rule in inner.rules.0 {
		// struct gen
		out.extend(vec![
			punc!('#'), group!(Delimiter::Bracket, vec![
				ident!("derive"), group!(Delimiter::Parenthesis, vec![
					inner.meta.derived_traits.clone()
				])
			])
		]);
        out.extend(TokenStream::from(inner.meta.struct_vis.to_token_stream()));
        out.extend(Some(ident!("struct")));
        out.extend(TokenStream::from(rule.name.to_token_stream()));
        out.extend(rule.return_type(&comp_type));
        out.extend(Some(punc!(';')));
		
		// Parser impl
        out.extend(vec![
			ident!("impl"), punc!('<'), puncj!('\''), ident!("a"), punc!('>'), ident!("Parser"), punc!('<'), punc!('&'), puncj!('\''), ident!("a")
		]);
		out.extend(TokenStream::from(inner.meta.tok_type.to_token_stream()));
		out.extend(Some(punc!(',')));
		out.extend(TokenStream::from(inner.meta.comp_type.to_token_stream()));
		out.extend(vec![punc!('>'), ident!("for")]);
        out.extend(TokenStream::from(rule.name.to_token_stream()));
		let mut innner_token_stream = TokenStream::new();
		let mut final_return = Vec::new();
		// extend the Parser::parse inner group by group
		for (k, i) in rule.inner.iter().enumerate() {
			if i.contains_save() {
				final_return.extend(vec![ident!(&*format!("v_{}", k)), punc!(',')]);
				innner_token_stream.extend(i.build_save(format!("v_{}", k), false, &comp_type, &map_fn));
			} else {
				innner_token_stream.extend(i.build_no_save(false, &comp_type));
			}
			innner_token_stream.extend(Some(punc!(';')));
		}
		final_return.pop();
		innner_token_stream.extend(vec![
			ident!("return"), ident!("Ok"), group!(Delimiter::Parenthesis, vec![
				ident!("Self"), group!(Delimiter::Parenthesis, final_return)
			])
		]);
		let mut inner_impl_fn = impl_fn.clone();
		inner_impl_fn.extend(Some(group!(Delimiter::Brace, innner_token_stream)));
		out.extend(Some(group!(Delimiter::Brace; inner_impl_fn)));
    }
    out
}

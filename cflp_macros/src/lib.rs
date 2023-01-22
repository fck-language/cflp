mod prelude;
mod parser;
mod build;

use proc_macro::{Delimiter, TokenStream, Spacing, Span};
use quote::ToTokens;
use syn::parse_macro_input;
use crate::prelude::{MacroInner, ReturnType, RuleInnerEnum};

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
			ident!("src"), punc!(':'), punc!('&'), ident!("mut"), ident!("T")
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
        out.extend(rule.generate_type(&inner.meta.struct_vis, &comp_type));
		
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
		// extend the Parser::parse inner group by group
		match rule.inner {
			RuleInnerEnum::Single(inner) => {
				let (rule_inner, return_args) = inner.build(ReturnType::Function, &comp_type, &map_fn);
				innner_token_stream.extend(rule_inner);
				innner_token_stream.extend(vec![
					ident!("return"), ident!("Ok"), group!(Delimiter::Parenthesis, vec![
						ident!("Self"), return_args
					])
				]);
			}
			RuleInnerEnum::Multiple(inners) => {
				let inner_return_rule = ReturnType::Lifetime(0, false);
				let mut iter = inners.iter().enumerate();
				let (count, rule) = iter.next().unwrap();
				let (mut rule_inner, return_args) = rule.build(inner_return_rule, &comp_type, &map_fn);
				rule_inner.extend(vec![
					punc!(';'), ident!("break")
				]);
				rule_inner.extend(inner_return_rule.get_lifetime());
				
				// expression inside the final `break Ok` statement
				let mut ok_inner = TokenStream::from_iter(vec![
					ident!("Self"), puncj!(':'), punc!(':')
				]);
				if let Some(name) = &rule.name {
					ok_inner.extend(TokenStream::from(name.to_token_stream()))
				} else {
					ok_inner.extend(Some(ident!(&*format!("Var{}", count + 1))))
				}
				ok_inner.extend(Some(return_args));
				
				rule_inner.extend(vec![
					ident!("Ok"), group!(Delimiter::Parenthesis; ok_inner)
				]);
				let mut out = TokenStream::from_iter(vec![
					ident!("let"), ident!("first_err"), punc!(';'),
					ident!("let"), ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';'),
					ident!("match")
				]);
				out.extend(inner_return_rule.get_lifetime());
				out.extend(TokenStream::from_iter(vec![
					punc!(':'), group!(Delimiter::Brace; rule_inner), group!(Delimiter::Brace, vec![
						ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), puncj!('='), punc!('>'), ident!("return"), ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), punc!(','),
						ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("e"))), puncj!('='), punc!('>'), group!(Delimiter::Brace, vec![
							ident!("first_err"), punc!('='), ident!("e"), punc!(';'),
							punc!('*'), ident!("src"), punc!('='), ident!("src_old")
						])
					])
				]));
				for (count, rule) in iter {
					// let src_old = src.clone();
					// match loop { $rule_inner break Ok(Self::Var${count+1}(return_args) } {
					//     Ok(t) => return Ok(t),
					//     Err(_) => *src = src_old
					// }
					out.extend(vec![
						ident!("let"), ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';'),
						ident!("match")
					]);
					out.extend(inner_return_rule.get_lifetime());
					let (mut rule_inner, return_args) = rule.build(inner_return_rule, &comp_type, &map_fn);
					rule_inner.extend(vec![
						punc!(';'), ident!("break")
					]);
					rule_inner.extend(inner_return_rule.get_lifetime());
					rule_inner.extend(vec![
						ident!("Ok"), group!(Delimiter::Parenthesis, vec![
							ident!("Self"), puncj!(':'), punc!(':'), ident!(&*format!("Var{}", count + 1)), return_args
						])
					]);
					out.extend(TokenStream::from_iter(vec![
						punc!(':'), group!(Delimiter::Brace; rule_inner), group!(Delimiter::Brace, vec![
							ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), puncj!('='), punc!('>'), ident!("return"), ident!("Ok"), group!(Delimiter::Parenthesis, Some(ident!("t"))), punc!(','),
							ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("_"))), puncj!('='), punc!('>'), punc!('*'), ident!("src"), punc!('='), ident!("src_old")
						])
					]));
				}
				out.extend(vec![
					ident!("return"), ident!("Err"), group!(Delimiter::Parenthesis, Some(ident!("first_err")))
				]);
				innner_token_stream.extend(out);
			}
		}
		let mut inner_impl_fn = impl_fn.clone();
		inner_impl_fn.extend(Some(group!(Delimiter::Brace, innner_token_stream)));
		out.extend(Some(group!(Delimiter::Brace; inner_impl_fn)));
    }
    out
}

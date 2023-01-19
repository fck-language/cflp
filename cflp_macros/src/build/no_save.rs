//! Token streams for values and groups

use proc_macro::{Delimiter, TokenStream, Spacing, Span};
use quote::ToTokens;
use crate::prelude::{Value, Group};

macro_rules! ident { ($t:expr) => {proc_macro::TokenTree::Ident(proc_macro::Ident::new($t, Span::mixed_site()))}; }
macro_rules! group {
	($t:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::new()))};
	($t:expr, $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, TokenStream::from_iter($s)))};
	($t:expr; $s:expr) => {proc_macro::TokenTree::Group(proc_macro::Group::new($t, $s))};
}
macro_rules! punc { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Alone))}; }
macro_rules! puncj { ($t:literal) => {proc_macro::TokenTree::Punct(proc_macro::Punct::new($t, Spacing::Joint))}; }

impl Value {
	pub(crate) fn build_no_save(&self, is_loop_while: bool, match_type: &TokenStream) -> TokenStream {
		match self {
			Value::Single(t) => {
				// let next = src.next();
				let mut out = TokenStream::from_iter(vec![
					ident!("let"), ident!("next"), punc!('='), ident!("src"), punc!('.'),
					ident!("next"), group!(Delimiter::Parenthesis), punc!(';')
				]);
				
				// if next != $t { $ret_break Err( cflp::Error { expected: $t, found: next } ) }
				let mut err_inner = TokenStream::from_iter(vec![ident!("expected"), punc!(':')]);
				err_inner.extend(TokenStream::from(t.to_token_stream()));
				err_inner.extend(vec![punc!(','), ident!("found"), punc!(':'), ident!("next")]);
				out.extend(vec![
					ident!("if"), ident!("next"), puncj!('!'), punc!('=')
				]);
				out.extend(TokenStream::from(t.to_token_stream()));
				out.extend(Some(group!(Delimiter::Brace, vec![
					if is_loop_while { ident!("break") } else { ident!("return") },
					ident!("Err"), group!(Delimiter::Parenthesis, vec![
						ident!("cflp"), puncj!(':'), punc!(':'), ident!("Error"),
						group!(Delimiter::Brace; err_inner)
					])
				])));
				out
			}
			Value::Save(_) => unreachable!("Value::Save variant should be inaccessible under a no_save function"),
			Value::Group(g, s) => {
				#[cfg(debug_assertions)]
				if *s { panic!(
					"Group with nested Value::Save should be unreachable under no_build!\n\
					This is a bug. Please submit an issue https://github.com/fck-language/cflp/issues/new"
				) }
				let mut out = TokenStream::new();
				for i in g {
					out.extend(i.build_no_save(is_loop_while, match_type))
				}
				out
			}
		}
	}
}

impl Group {
	pub(crate) fn build_no_save(&self, is_loop_while: bool, match_type: &TokenStream) -> TokenStream {
		match self {
			Group::Literal(v, s) => {
				#[cfg(debug_assertions)]
				if *s { panic!(
					"Group with nested Value::Save should be unreachable under no_build!\n\
					This is a bug. Please submit an issue https://github.com/fck-language/cflp/issues/new"
				) }
				v.build_no_save(is_loop_while, match_type)
			},
			Group::Kleene(v, s) => {
				#[cfg(debug_assertions)]
				if *s { panic!(
					"Group with nested Value::Save should be unreachable under no_build!\n\
					This is a bug. Please submit an issue https://github.com/fck-language/cflp/issues/new"
				) }
				// let mut src_old;
				let mut out = TokenStream::from_iter(vec![
					ident!("let"), ident!("mut"), ident!("src_old"), punc!(';')
				]);
				// loop {
				//     src_old = src.clone();
				//     if { $v.build_no_save(true) Ok(()) }.is_err() { src = src_old; break }
				// }
				let mut conditional_block = v.build_no_save(false, match_type);
				conditional_block.extend(vec![ident!("Ok"), group!(Delimiter::Parenthesis, Some(group!(Delimiter::Parenthesis)))]);
				out.extend(vec![
					ident!("loop"), group!(Delimiter::Brace, vec![
						ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';'),
						ident!("if"), group!(Delimiter::Brace; conditional_block), punc!('.'), ident!("is_err"), group!(Delimiter::Parenthesis),
						group!(Delimiter::Brace, vec![punc!('*'), ident!("src"), punc!('='), ident!("src_old"), punc!(';'), ident!("break")])
					])
				]);
				out
			}
			Group::Positive(v, s) => {
				#[cfg(debug_assertions)]
				if *s { panic!(
					"Group with nested Value::Save should be unreachable under no_build!\n\
					This is a bug. Please submit an issue https://github.com/fck-language/cflp/issues/new"
				) }
				// a+ == a a*
				let mut out = Group::Literal(v.clone(), false).build_no_save(is_loop_while, match_type);
				out.extend(Group::Kleene(v.clone(), false).build_no_save(is_loop_while, match_type));
				out
			}
			Group::Option(v, s) => {
				#[cfg(debug_assertions)]
				if *s { panic!(
					"Group with nested Value::Save should be unreachable under no_build!\n\
					This is a bug. Please submit an issue https://github.com/fck-language/cflp/issues/new"
				) }
				// let mut src_old = src.clone();
				let mut out = TokenStream::from_iter(vec![
					ident!("let"), ident!("mut"), ident!("src_old"), punc!('='), ident!("src"), punc!('.'), ident!("clone"), group!(Delimiter::Parenthesis), punc!(';')
				]);
				// if loop { $v.build_no_save(true) break Ok(()) }.is_err() { src = src_old }
				let mut conditional_block = v.build_no_save(true, match_type);
				conditional_block.extend(vec![ident!("break"), ident!("Ok"), group!(Delimiter::Parenthesis, Some(group!(Delimiter::Parenthesis)))]);
				out.extend(vec![
					ident!("if"), ident!("loop"), group!(Delimiter::Brace; conditional_block), punc!('.'), ident!("is_err"), group!(Delimiter::Parenthesis),
					group!(Delimiter::Brace, vec![punc!('*'), ident!("src"), punc!('='), ident!("src_old")])
				]);
				out
			}
		}
	}
}

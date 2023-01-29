//! An example with an or rule
//!
//! This demonstrates how to have a rule that matches multiple things, and how you can name the
//! generated variants

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
	OP, CP,
	Literal(u8),
	Other(char),
}

#[derive(Debug)]
pub struct Token {
	pub ps: usize,
	pub pe: usize,
	pub _type: TokenType,
}

impl PartialEq<TokenType> for &Token {
	fn eq(&self, other: &TokenType) -> bool {
		&self._type == other
	}
}

use cflp::rule;
// Generate types and parser impls
rule!(
	(pub(crate), Token, TokenType, |t| t._type.clone(), (Debug, Clone))
	// Root(Vec<Expr>)
	(Root; TokenType::OP, ([@Expr])*, TokenType::CP)
	// Or rule. Will generate an enum
	// Expr( u8 | char )
	(Expr; Literal => [TokenType::Literal; u8]; [TokenType::Other; char])
);

#[allow(unused_parens, unused_imports)]
mod equivalent {
	use super::{Token, TokenType};
	use cflp::rule;
	use cflp::Parser;
	
	#[derive(Debug, Clone)]
	pub(crate) struct Root(Vec<(Expr)>);
	
	impl<'a> Parser<&'a Token, TokenType> for Root {
		fn parse<T: Iterator<Item=&'a Token> + Clone>(
			src: &mut T,
		) -> Result<Self, cflp::Error<&'a Token, TokenType>> {
			let next = src.next();
			if next.clone().map(|t| t._type.clone()) != Some(TokenType::OP) {
				return Err(cflp::Error {
					expected: TokenType::OP,
					found: next,
				});
			}
			let v_1 = {
				let mut v_1_out = Vec::new();
				loop {
					let src_old = src.clone();
					match 'l0: {
						let v_1_0_0 = {
							match Expr::parse(src) {
								Ok(t) => t,
								Err(e) => break 'l0 Err(e),
							}
						};
						Ok(v_1_0_0)
					} {
						Ok(t) => v_1_out.push(t),
						Err(_) => {
							*src = src_old;
							break;
						}
					}
				}
				v_1_out
			};
			let next = src.next();
			if next.clone().map(|t| t._type.clone()) != Some(TokenType::CP) {
				return Err(cflp::Error {
					expected: TokenType::CP,
					found: next,
				});
			}
			return Ok(Self(v_1));
		}
	}
	
	#[derive(Debug, Clone)]
	pub(crate) enum Expr {
		Literal(((u8))),
		Var2(((char))),
	}
	
	impl<'a> Parser<&'a Token, TokenType> for Expr {
		fn parse<T: Iterator<Item=&'a Token> + Clone>(
			src: &mut T,
		) -> Result<Self, cflp::Error<&'a Token, TokenType>> {
			let first_err;
			let src_old = src.clone();
			match 'l0: {
				let v_0 = {
					let next = src.next();
					if let Some(TokenType::Literal(n_0)) = next.map(|t| t._type.clone()) {
						((n_0))
					} else {
						break 'l0 Err(cflp::Error {
							expected: TokenType::Literal(Default::default()),
							found: next,
						});
					}
				};
				break 'l0 Ok(Self::Literal(v_0));
			} {
				Ok(t) => return Ok(t),
				Err(e) => {
					first_err = e;
					*src = src_old;
				}
			}
			let src_old = src.clone();
			match 'l0: {
				let v_0 = {
					let next = src.next();
					if let Some(TokenType::Other(n_0)) = next.map(|t| t._type.clone()) {
						((n_0))
					} else {
						break 'l0 Err(cflp::Error {
							expected: TokenType::Other(Default::default()),
							found: next,
						});
					}
				};
				break 'l0 Ok(Self::Var2(v_0));
			} {
				Ok(t) => return Ok(t),
				Err(_) => *src = src_old,
			}
			return Err(first_err);
		}
	}
}

fn main() {
	let sample_input = vec![
		Token { ps: 0, pe: 1, _type: TokenType::OP },
		Token { ps: 1, pe: 2, _type: TokenType::Literal(1) },
		Token { ps: 2, pe: 3, _type: TokenType::Other('b') },
		Token { ps: 3, pe: 4, _type: TokenType::Literal(3) },
		Token { ps: 4, pe: 5, _type: TokenType::CP },
	];
	let mut input_iterator = sample_input.iter().peekable();
	while input_iterator.peek().is_some() {
		match Root::parse(&mut input_iterator) {
			Ok(r) => println!("{:?}", r),
			Err(e) => {
				println!("{:?}", e);
				break;
			}
		}
	}
}

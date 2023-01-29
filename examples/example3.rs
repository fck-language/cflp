//! An example that won't generate the types for you
//!
//! This example is functionally the same as example1 but without the type generation by making use
//! of the `rule_no_types!` macro

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
	OP,
	CP,
	Literal(u8),
}

#[derive(Debug)]
pub struct Token {
	pub ps: usize,
	pub pe: usize,
	pub _type: TokenType,
}

impl PartialEq<TokenType> for &Token {
	fn eq(&self, other: &TokenType) -> bool { &self._type == other }
}

use cflp::rule_no_types;
// Generate only Parser impls for the already existing types
rule_no_types!(
	(Token, TokenType, |t| t._type.clone())
	// Root(Option<Inner>)
	(Root; TokenType::OP, ([@Inner])?, TokenType::CP)
	// Inner(Vec<u8>)
	(Inner; ([TokenType::Literal; u8])*)
);

#[derive(Debug, Clone)]
struct Root(Option<Inner>);
#[derive(Debug, Clone)]
struct Inner(Vec<u8>);

#[allow(unused_parens, unused_imports)]
mod equivalent {
	#[derive(Debug, Clone)]
	struct Root(Option<Inner>);
	#[derive(Debug, Clone)]
	struct Inner(Vec<u8>);
	// The above section is not generated. It's been included to allow the example to run.
	// Only the impls are generated
	use cflp::{rule, Parser};

	use super::{Token, TokenType};

	impl<'a> Parser<&'a Token, TokenType> for Root {
		fn parse<T: Iterator<Item = &'a Token> + Clone>(
			src: &mut T,
		) -> Result<Self, cflp::Error<&'a Token, TokenType>> {
			let next = src.next();
			if next.clone().map(|t| t._type.clone()) != Some(TokenType::OP) {
				return Err(cflp::Error {
					expected: TokenType::OP,
					found: next,
				})
			}
			let v_1 = {
				let src_old = src.clone();
				match 'l0: {
					let v_1_0_0 = {
						match Inner::parse(src) {
							Ok(t) => t,
							Err(e) => break 'l0 Err(e),
						}
					};
					Ok(v_1_0_0)
				} {
					Ok(t) => Some(t),
					Err(_) => {
						*src = src_old;
						None
					}
				}
			};
			let next = src.next();
			if next.clone().map(|t| t._type.clone()) != Some(TokenType::CP) {
				return Err(cflp::Error {
					expected: TokenType::CP,
					found: next,
				})
			}
			return Ok(Self(v_1))
		}
	}

	impl<'a> Parser<&'a Token, TokenType> for Inner {
		fn parse<T: Iterator<Item = &'a Token> + Clone>(
			src: &mut T,
		) -> Result<Self, cflp::Error<&'a Token, TokenType>> {
			let v_0 = {
				let mut v_0_out = Vec::new();
				loop {
					let src_old = src.clone();
					match 'l0: {
						let v_0_0_0 = {
							let next = src.next();
							if let Some(TokenType::Literal(n_0)) = next.map(|t| t._type.clone()) {
								(n_0)
							} else {
								break 'l0 Err(cflp::Error {
									expected: TokenType::Literal(Default::default()),
									found: next,
								})
							}
						};
						Ok(v_0_0_0)
					} {
						Ok(t) => v_0_out.push(t),
						Err(_) => {
							*src = src_old;
							break
						}
					}
				}
				v_0_out
			};
			return Ok(Self(v_0))
		}
	}
}

fn main() {
	let sample_input = vec![
		Token {
			ps: 0,
			pe: 1,
			_type: TokenType::OP,
		},
		Token {
			ps: 1,
			pe: 2,
			_type: TokenType::Literal(1),
		},
		Token {
			ps: 2,
			pe: 3,
			_type: TokenType::Literal(2),
		},
		Token {
			ps: 3,
			pe: 4,
			_type: TokenType::Literal(3),
		},
		Token {
			ps: 4,
			pe: 5,
			_type: TokenType::CP,
		},
	];
	let mut input_iterator = sample_input.iter().peekable();
	while input_iterator.peek().is_some() {
		match Root::parse(&mut input_iterator) {
			Ok(r) => println!("{:?}", r),
			Err(e) => {
				println!("{:?}", e);
				break
			}
		}
	}
}

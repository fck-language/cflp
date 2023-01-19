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
	fn eq(&self, other: &TokenType) -> bool {
		&self._type == other
	}
}

impl PartialEq<TokenType> for Option<&Token> {
	fn eq(&self, other: &TokenType) -> bool {
		if let Some(t) = self { t == other } else { false }
	}
}

mod example {
	use super::{Token, TokenType};
	use cflp::rule;
	rule!(
		(pub(crate), Token, TokenType, |t| t._type.clone(), (Debug, Clone))
		// Root(Option<Inner>)
		(Root; TokenType::OP, ([@Inner])?, TokenType::CP)
		// Inner(Vec<u8>)
		(Inner; ([TokenType::Literal; u8])*)
	);
}

mod equivalent {
	use crate::{Token, TokenType};
	use cflp::rule;
	use cflp::Parser;
	
	#[derive(Debug, Clone)]
	pub(crate) struct Root(Option<(Inner)>);
	
	impl<'a> Parser<&'a Token, TokenType> for Root {
		fn parse<T: Iterator<Item=&'a Token> + Clone>(
			mut src: &mut T,
		) -> Result<Self, cflp::Error<&'a Token, TokenType>> {
			let next = src.next();
			if next != TokenType::OP {
				return Err(cflp::Error {
					expected: TokenType::OP,
					found: next,
				});
			}
			let v_1 = {
				let mut src_old = src.clone();
				match {
					let v_1_0 = { Inner::parse(src) };
					(v_1_0)
				} {
					Ok(t) => Some(t),
					Err(_) => {
						*src = src_old;
						None
					}
				}
			};
			let next = src.next();
			if next != TokenType::CP {
				return Err(cflp::Error {
					expected: TokenType::CP,
					found: next,
				});
			}
			return Ok(Self(v_1));
		}
	}
	
	#[derive(Debug, Clone)]
	pub(crate) struct Inner(Vec<((u8))>);
	
	impl<'a> Parser<&'a Token, TokenType> for Inner {
		fn parse<T: Iterator<Item=&'a Token> + Clone>(
			mut src: &mut T,
		) -> Result<Self, cflp::Error<&'a Token, TokenType>> {
			let v_0 = {
				let mut v_0_out = Vec::new();
				let mut src_old;
				loop {
					src_old = src.clone();
					match {
						let v_0_0_0 = {
							let next = src.next();
							if let Some(TokenType::Literal(n_0))
								= next.map(|t| t._type.clone())
							{
								Ok((n_0))
							} else {
								Err(cflp::Error {
									expected: TokenType::Literal(Default::default()),
									found: next,
								})
							}
						};
						(v_0_0_0)
					} {
						Ok(t) => v_0_out.push(t),
						Err(_) => {
							*src = src_old;
							break;
						}
					}
				}
				v_0_out
			};
			return Ok(Self(v_0));
		}
	}
}

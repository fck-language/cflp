use cflp::{Error, Parser};
use crate::simple::prelude::*;

#[derive(Debug, Clone)]
pub struct Root(Expr);

#[derive(Debug, Clone)]
pub enum Expr {
	Var1(u8),
	Var2(char)
}

impl<'a> Parser<&'a Token, TokenType> for Root {
	fn parse<T: Iterator<Item=&'a Token> + Clone>(src: &mut T) -> Result<Self, Error<&'a Token, TokenType>> where Self: Sized {
		let next = src.next();
		if next.map(|t| t.t) != Some(TokenType::OP) {
			return Err(Error { found: next, expected: TokenType::OP })
		}
		let inner = Expr::parse(src)?;
		let next = src.next();
		if next.map(|t| t.t) != Some(TokenType::CP) {
			return Err(Error { found: next, expected: TokenType::OP })
		}
		Ok(Self(inner))
	}
}

impl<'a> Parser<&'a Token, TokenType> for Expr {
	fn parse<T: Iterator<Item=&'a Token> + Clone>(src: &mut T) -> Result<Self, Error<&'a Token, TokenType>> where Self: Sized {
		let next = src.next();
		match next.map(|t| t.t) {
			None => Err(Error { found: None, expected: TokenType::Value(0) }),
			Some(TokenType::Value(i)) => Ok(Self::Var1(i)),
			Some(TokenType::Punc(i)) => Ok(Self::Var2(i)),
			_ => Err(Error { found: next, expected: TokenType::Value(0) })
		}
	}
}

use cflp::{parser, Parser};
use crate::simple::prelude::*;

#[derive(Debug, Clone)]
#[parser(Token, TokenType, |t| t.t; TokenType::OP, [@Expr], TokenType::CP)]
pub struct Root(Expr);

#[derive(Debug, Clone)]
#[parser(Token, TokenType, |t| t.t)]
pub enum Expr {
	#[parser([TokenType::Value; u8])]
	Var1(u8),
	#[parser([TokenType::Punc; char])]
	Var2(char)
}

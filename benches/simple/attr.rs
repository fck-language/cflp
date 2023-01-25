use cflp::{parser, Parser};
use crate::simple::prelude::*;

#[derive(Debug, Clone)]
#[parser(Token, TokenType, |t| t.t; TokenType::OP, [@Expr], TokenType::CP)]
pub struct Root(Expr);

#[derive(Debug, Clone)]
#[parser(Token, TokenType, |t| t.t; [TokenType::Value; u8]; [TokenType::Punc; char])]
pub enum Expr {
	Var1(u8),
	Var2(char)
}

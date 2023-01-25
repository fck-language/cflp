use cflp::{parser, Parser};
use crate::complex::prelude::*;

#[derive(Debug, Clone)]
#[parser(Token<'a>, TokenType<'a>, |t| t.t; ([@Statement<'a>])*)]
pub struct Root<'a>(Vec<Statement<'a>>);

#[derive(Debug, Clone)]
#[parser(Token<'a>, TokenType<'a>, |t| t.t;
	TokenType::Kwd(Kwd::If), TokenType::OP, [@Expr<'a>], TokenType::CP, TokenType::OB, ([@Statement<'a>])*, TokenType::CB;
	TokenType::Kwd(Kwd::Let), [TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol);
	[TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol)
)]
pub enum Statement<'a> {
	Var1(Expr<'a>, Vec<Box<Statement<'a>>>),
	Var2(&'a str, Expr<'a>),
	Var3(&'a str, Expr<'a>)
}

#[derive(Debug, Clone)]
#[parser(Token<'a>, TokenType<'a>, |t| t.t;
	[TokenType::Ident; &'a str];
	[TokenType::Literal; u8];
	[@Expr<'a>], [TokenType::Punc; Punc], [@Expr<'a>]
)]
pub enum Expr<'a> {
	Var1(&'a str),
	Var2(u8),
	Var3(Box<Expr<'a>>, Punc, Box<Expr<'a>>)
}

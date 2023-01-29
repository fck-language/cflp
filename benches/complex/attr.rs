use cflp::{parser, Parser};

use crate::complex::prelude::*;

#[derive(Debug, Clone)]
#[parser(Token<'a>, TokenType<'a>, |t| t.t; ([@Statement<'a>])*)]
pub struct Root<'a>(Vec<Statement<'a>>);

#[derive(Debug, Clone)]
#[parser(Token<'a>, TokenType<'a>, |t| t.t)]
pub enum Statement<'a> {
	#[parser(TokenType::Kwd(Kwd::If), TokenType::OP, [@Expr<'a>], TokenType::CP, TokenType::OB, ([@Statement<'a>])*, TokenType::CB)]
	Var1(Expr<'a>, Vec<Box<Statement<'a>>>),
	#[parser(TokenType::Kwd(Kwd::Let), [TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol))]
	Var2(&'a str, Expr<'a>),
	#[parser([TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol))]
	Var3(&'a str, Expr<'a>),
}

#[derive(Debug, Clone)]
#[parser(Token<'a>, TokenType<'a>, |t| t.t)]
pub enum Expr<'a> {
	#[parser([TokenType::Ident; &'a str])]
	Var1(&'a str),
	#[parser([TokenType::Literal; u8])]
	Var2(u8),
	#[parser([@Expr<'a>], [TokenType::Punc; Punc], [@Expr<'a>])]
	Var3(Box<Expr<'a>>, Punc, Box<Expr<'a>>),
}

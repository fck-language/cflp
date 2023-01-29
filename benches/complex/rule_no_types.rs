use cflp::rule_no_types;

use crate::complex::prelude::*;

#[derive(Debug, Clone)]
pub struct Root<'a>(Vec<Statement<'a>>);

#[derive(Debug, Clone)]
pub enum Statement<'a> {
	Var1(Expr<'a>, Vec<Box<Statement<'a>>>),
	Var2(&'a str, Expr<'a>),
	Var3(&'a str, Expr<'a>),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
	Var1(&'a str),
	Var2(u8),
	Var3(Box<Expr<'a>>, Punc, Box<Expr<'a>>),
}

rule_no_types!(
	(Token<'a>, TokenType<'a>, |t| t.t)
	(Root; ([@Statement<'a>])*)
	(Statement;
		TokenType::Kwd(Kwd::If), TokenType::OP, [@Expr<'a>], TokenType::CP, TokenType::OB, ([@Statement<'a>])*, TokenType::CB;
		TokenType::Kwd(Kwd::Let), [TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol);
		[TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol)
	)
	(Expr;
		[TokenType::Ident; &'a str];
		[TokenType::Literal; u8];
		[@Expr<'a>], [TokenType::Punc; Punc], [@Expr<'a>]
	)
);

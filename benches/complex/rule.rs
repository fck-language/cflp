use cflp::rule;

use crate::complex::prelude::*;

rule!(
	(pub, Token<'a>, TokenType<'a>, |t| t.t, (Debug, Clone))
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

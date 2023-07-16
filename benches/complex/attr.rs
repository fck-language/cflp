use crate::complex::prelude::*;
use cflp::Parser;

#[derive(Debug, Clone, Parser)]
#[parser(Token<'a>, TokenType<'a>, |t| t.t; ([@Statement<'a>])*)]
pub struct Root<'a>(Vec<Statement<'a>>);

#[derive(Debug, Clone, Parser)]
#[parser(Token<'a>, TokenType<'a>, |t: &Token<'_>| t.t)]
pub enum Statement<'a> {
    #[parser(TokenType::Kwd(Kwd::If), TokenType::OP, [@Expr<'a>], TokenType::CP, TokenType::OB, ([@Statement<'a>])*, TokenType::CB)]
    Var1(Expr<'a>, Vec<Statement<'a>>),
    #[parser(TokenType::Kwd(Kwd::Let), [TokenType::Ident(t)], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol))]
    Var2(&'a str, Expr<'a>),
    #[parser([TokenType::Ident(t)], TokenType::Punc(Punc::Eq), [@Expr<'a>], TokenType::Punc(Punc::SCol))]
    Var3(&'a str, Expr<'a>),
}

#[derive(Debug, Clone, Parser)]
#[parser(Token<'a>, TokenType<'a>, |t: &Token<'a>| t.t)]
pub enum Expr<'a> {
    #[parser([TokenType::Ident(t)])]
    Var1(&'a str),
    #[parser([TokenType::Literal(t)])]
    Var2(u8),
    #[parser([[@Expr<'a>]], [TokenType::Punc(p)], [[@Expr<'a>]])]
    Var3(Box<Expr<'a>>, Punc, Box<Expr<'a>>),
}


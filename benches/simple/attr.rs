use crate::simple::prelude::*;
use cflp::Parser;

#[derive(Debug, Clone, Parser)]
#[parser(Token, TokenType, |t| t.t; TokenType::OP, [@Expr], TokenType::CP)]
pub struct Root(Expr);

#[derive(Debug, Clone, Parser)]
#[parser(Token, TokenType, |t: &Token| t.t)]
pub enum Expr {
    #[parser([TokenType::Value(t)])]
    Var1(u8),
    #[parser([TokenType::Punc(t)])]
    Var2(char),
}

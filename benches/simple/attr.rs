use crate::simple::prelude::*;
use cflp::Parser;

#[derive(Debug, Clone, Parser)]
#[parser(Token, TokenType; TokenType::OP, [@Expr], TokenType::CP)]
pub struct Root(Expr);

#[derive(Debug, Clone, Parser)]
#[parser(Token, TokenType)]
pub enum Expr {
    #[parser([TokenType::Value(t)])]
    Var1(u8),
    #[parser([TokenType::Punc(t)])]
    Var2(char),
}

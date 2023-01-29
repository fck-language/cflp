use cflp::rule_no_types;

use crate::simple::prelude::*;

#[derive(Debug, Clone)]
pub struct Root(Expr);

#[derive(Debug, Clone)]
pub enum Expr {
	Var1(u8),
	Var2(char),
}

rule_no_types!(
	(Token, TokenType, |t| t.t)
	(Root; TokenType::OP, [@Expr], TokenType::CP)
	(Expr; [TokenType::Value; u8]; [TokenType::Punc; char])
);

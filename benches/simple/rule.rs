use cflp::rule;
use crate::simple::prelude::*;

rule!(
	(pub, Token, TokenType, |t| t.t, (Debug, Clone))
	(Root; TokenType::OP, [@Expr], TokenType::CP)
	(Expr; [TokenType::Value; u8]; [TokenType::Punc; char])
);

//! Complex parser benchmarks (C-like grammar)
//!
//! ```
//! (Root; ([@Statement])*)
//! (Statement;
//! 	TokenType::Kwd(Kwd::If), TokenType::OP, [@Expr], TokenType::CP, TokenType::OB, ([@Statement])*, TokenType::CB;
//! 	TokenType::Kwd(Kwd::Let), [TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr], TokenType::Punc(Punc::SCol);
//! 	[TokenType::Ident; &'a str], TokenType::Punc(Punc::Eq), [@Expr], TokenType::Punc(Punc::SCol)
//! )
//! (Expr;
//! 	[TokenType::Ident; &'a str];
//! 	[TokenType::Literal; u8];
//! 	[@Expr], [TokenType::Punc; Punc], [@Expr]
//! )
//! ```

use cflp::Parser;
use criterion::Criterion;

mod attr;
mod custom;
mod prelude;

use prelude::*;

// if (1 + 2) {
//     let test = 5;
//     test = test - 3;
//     if (9) { let t = 1; }
// }
macro_rules! tok {
    ($t:expr, $p1:literal, $p2:literal $(,)?) => {
        Token {
            p: ($p1, $p2),
            t: $t
        }
    };
}
const ITER: [Token; 30] = [
	tok!(TokenType::Kwd(Kwd::If), 0, 1),
	tok!(TokenType::OP, 1, 2),
		tok!(TokenType::Literal(1), 1, 2),
		tok!(TokenType::Punc(Punc::Plus), 1, 2),
		tok!(TokenType::Literal(2), 1, 2),
	tok!(TokenType::CP, 1, 2),
	tok!(TokenType::OB, 1, 2),
		tok!(TokenType::Kwd(Kwd::Let), 1, 2),
		tok!(TokenType::Ident("test"), 1, 2),
		tok!(TokenType::Punc(Punc::Eq), 1, 2),
		tok!(TokenType::Literal(5), 1, 2),
		tok!(TokenType::Punc(Punc::SCol), 1, 2),
		
		tok!(TokenType::Ident("test"), 1, 2),
		tok!(TokenType::Punc(Punc::Eq), 1, 2),
		tok!(TokenType::Ident("test"), 1, 2),
		tok!(TokenType::Punc(Punc::Minus), 1, 2),
		tok!(TokenType::Literal(3), 1, 2),
		tok!(TokenType::Punc(Punc::SCol), 1, 2),
		
		tok!(TokenType::Kwd(Kwd::If), 0, 1),
		tok!(TokenType::OP, 1, 2),
			tok!(TokenType::Literal(9), 1, 2),
		tok!(TokenType::CP, 1, 2),
		tok!(TokenType::OB, 1, 2),
			tok!(TokenType::Kwd(Kwd::Let), 1, 2),
			tok!(TokenType::Ident("T"), 1, 2),
			tok!(TokenType::Punc(Punc::Eq), 1, 2),
			tok!(TokenType::Literal(1), 1, 2),
			tok!(TokenType::Punc(Punc::SCol), 1, 2),
		tok!(TokenType::CB, 1, 2),
	tok!(TokenType::CB, 1, 2),
];

pub fn main(c: &mut Criterion) {
	let mut g = c.benchmark_group("Complex");
	g.bench_function("derived", |b| {
		b.iter(|| attr::Root::parse(&mut ITER.iter()))
	});
	g.bench_function("handwritten", |b| {
		b.iter(|| custom::Root::parse(&mut ITER.iter()))
	});
	g.finish()
}

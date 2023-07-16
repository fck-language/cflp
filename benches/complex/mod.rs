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
// }
const ITER: [Token; 19] = [
    Token {
        p: (0, 1),
        t: TokenType::Kwd(Kwd::If),
    },
    Token {
        p: (1, 2),
        t: TokenType::OP,
    },
    Token {
        p: (1, 2),
        t: TokenType::Literal(1),
    },
    Token {
        p: (1, 2),
        t: TokenType::Punc(Punc::Plus),
    },
    Token {
        p: (1, 2),
        t: TokenType::Literal(2),
    },
    Token {
        p: (1, 2),
        t: TokenType::CP,
    },
    Token {
        p: (1, 2),
        t: TokenType::OB,
    },
    Token {
        p: (1, 2),
        t: TokenType::Kwd(Kwd::Let),
    },
    Token {
        p: (1, 2),
        t: TokenType::Ident("test"),
    },
    Token {
        p: (1, 2),
        t: TokenType::Punc(Punc::Eq),
    },
    Token {
        p: (1, 2),
        t: TokenType::Literal(5),
    },
    Token {
        p: (1, 2),
        t: TokenType::Punc(Punc::SCol),
    },
    Token {
        p: (1, 2),
        t: TokenType::Ident("test"),
    },
    Token {
        p: (1, 2),
        t: TokenType::Punc(Punc::Eq),
    },
    Token {
        p: (1, 2),
        t: TokenType::Ident("test"),
    },
    Token {
        p: (1, 2),
        t: TokenType::Punc(Punc::Minus),
    },
    Token {
        p: (1, 2),
        t: TokenType::Literal(3),
    },
    Token {
        p: (1, 2),
        t: TokenType::Punc(Punc::SCol),
    },
    Token {
        p: (1, 2),
        t: TokenType::CB,
    },
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

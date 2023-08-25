//! Simple parser benchmarks
//!
//! ```
//! (Root; TokenType::OP, [@Expr], TokenType::CP)
//! (Expr; [TokenType::Value; u8]; [TokenType::Punc; char])
//! ```

use cflp::Parser;
use criterion::Criterion;

mod attr;
mod custom;
mod prelude;

use prelude::*;

const ITER: [Token; 19] = [
    Token {
        p: (0, 1),
        t: TokenType::OP,
    },
    Token {
        p: (1, 2),
        t: TokenType::Value(5),
    },
    Token {
        p: (2, 3),
        t: TokenType::Punc('+'),
    },
    Token {
        p: (3, 4),
        t: TokenType::Value(3),
    },
    Token {
        p: (4, 5),
        t: TokenType::Punc('-'),
    },
    Token {
        p: (5, 6),
        t: TokenType::Value(8),
    },
    Token {
        p: (6, 7),
        t: TokenType::Punc('*'),
    },
    Token {
        p: (7, 9),
        t: TokenType::Value(28),
    },
    Token {
        p: (9, 10),
        t: TokenType::Punc('/'),
    },
    Token {
        p: (10, 12),
        t: TokenType::Value(17),
    },
    Token {
        p: (12, 13),
        t: TokenType::Punc('.'),
    },
    Token {
        p: (13, 14),
        t: TokenType::Value(5),
    },
    Token {
        p: (14, 15),
        t: TokenType::Punc('%'),
    },
    Token {
        p: (15, 16),
        t: TokenType::Value(2),
    },
    Token {
        p: (16, 17),
        t: TokenType::Punc('+'),
    },
    Token {
        p: (17, 18),
        t: TokenType::Value(8),
    },
    Token {
        p: (18, 19),
        t: TokenType::Punc('*'),
    },
    Token {
        p: (19, 20),
        t: TokenType::Value(9),
    },
    Token {
        p: (20, 21),
        t: TokenType::CP,
    },
];

pub fn main(c: &mut Criterion) {
    let mut g = c.benchmark_group("Simple");
    g.bench_function("derived", |b| {
        b.iter(|| attr::Root::parse(&mut ITER.iter()))
    });
    g.bench_function("handwritten", |b| {
        b.iter(|| custom::Root::parse(&mut ITER.iter()))
    });
    g.finish()
}

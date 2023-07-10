# Context free language parser

This crate provides functionality for parsing context-free languages and was written for use with [fck](https://github.com/fck-language/fck). The [documentation](https://fck-language.github.io/cflp) will also link you to some [benchmarks](https://fck-language.github.io/cflp/bench/report/index.html). These were performed on a MacBook Pro with an M1 Pro CPU. The times are mainly intended for comparisons.

## Usage

This crate provides a derive macro for the `Parser` trait. It also provides another trait and two structs used along-side the `Parser` trait.

For how to use the derive macro, you will need to read the [documentation](https://fck-language.github.io/cflp)

## Examples

The [examples](examples) directory contains some example files with generated expansions. These are generated using [`cargo-expand`](https://crates.io/crates/cargo-expand) and have been neatened up to make them more readable.

The structure of the examples is the same for all of them:
1. Two enums for `TokenType` and `Token` along with required trait impls
2. `mod nodes` which contains the nodes with `#[derive(Parser)]`
3. `mod equivalent` which is the expanded code for the `Parser` impl

## Current issues

- Errors are a bit rudimentary at the moment:
  - If you consider the rule `(Token::T1)?, Token::T2`, the first token could be either `Token::T1` or `Token::T2`. If neither of these are found then the returned error will say it expected `Token::T2`. A future version wil have better errors where the `expected` is a `Vec<E>` that will be calculated for each possible error position for more useful errors
  - When parsing an enum, if no variants can be matched, then the error from the error returned is from mathing the first variant. This is a lossy error and may be change in the future
- Non-positional token data is not saved (with positional data only being saved in a `NodeWrapper`). This is intended for v1.1.0 with an additional method in the `NodeData` trait to return additional data wrapped in an `Option`. This will then be wrapped in a new struct to contain additional data

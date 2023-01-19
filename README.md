# Context free language parser

This crate provides functionality for parsing context-free languages and was written for use with [fck](https://github.com/fck-language/fck).

## Usage

This crate provides the `rule!` macro to enable parsing context-free languages. The macro is quite powerful so we recommend you have a look at the [documentation](https://fck-language.com/cflp).

the crate also provides the `Parser` trait and `Error` struct returned when the `Parser::parse` function encounters an error.

### Trait impl requirements

There are three trait requirements for the two types which we'll call `T` for the input type (the input iterator into the parse functions has type `F: Iter<Item=&T>`) and `E` for the comparison type:
1. `E: PartialEq<E>`
2. `&T: PartialEq<E>`
3. `Option<&T>: PartialEq<E>`

The saved values in the resulting structs must also implement the derived traits given to the macro. For example, you can derive `Copy` if you store a `u8` but not a `String`.

## Examples

The [examples](examples) directory contains some example files with generated expansions. These are generated using [`cargo-expand`](https://crates.io/crates/cargo-expand) without the expansion of the derived traits and some manual visual formatting (no removal of parentheses or brackets, just changing some line breaks).

## Current issues

- You can't currently partially match the value of an enum variant. For example, if you wanted to match `EnumName::Variant(T, E)` where the first value was some specific value and then save the second value similarly to
  ```rust
  if let EnumName::Variant(1, a) = var_to_check { f(a) }
  ```
- Or blocks are currently unsupported but will be soon
- Calling other functions (an identifying feature of CFLs) is not currently implemented. I'm working on it

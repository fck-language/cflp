# Changelog

> For each version, the changes are from the previous version to the labeled one. For example, the section for 1.2.1 is for changes from 1.2.0. The first version (not listed) is 0.1.0

- [1.0.0](#100)
  - [1.1.0](#110)
  - [1.2.0](#120)
    - [1.2.1](#121)

## 1.0.0

> Migration: When migrating from 0.x.x to 1.x.x, you should completely rewrite any derive macros into the new format following the advice in the [book](https://fck-language.github.io/cflp/).

- Removed macros to generate types
- Changed derive macro grammar

## 1.1.0

- Added `Parser::parse_with_recursion` to prevent stack overflows

## 1.2.0

- Better stack overflow prevention

## 1.2.1

- Added optional default values for wild matches
- Fixed a lot of bugs (sorry)
- Made the code nicer. Not really important for end users, but nice for me
- You can now derive `Parser` for types with generics

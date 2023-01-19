# Context free language parser

This crate provides functionality for parsing context-free languages and was written for use with [fck](https://github.com/fck-language/fck).

## Usage

This crate provides

This crate provides the `rule!` macro. This macro has the following syntax:
```rust
rule!(
    (enum_visibility, function_visibility, src_iter_item)
    (rule_name1; rule tokens)
    (rule_name2; rule tokens)
    /* ... */
    (rule_name_n; rule tokens)
);
```

For the expansion and some notes on this macro, have a look at the [README](cflp_macros/README.md) for the macro.

## Rule tokens

The rule tokens are split into two parts:
1. Values
2. Modifiers

The values are either a literal type, group, saving value, or call; and the modifiers apply to value to change how they're parsed.

For the examples below we'll use the following token enum

```rust
pub enum Token {
  OCB,         // '('
  CCB,         // ')'
  Value(u8),   // Sample token 1
  Other(char), // Sample token 2
}
```

### Values

A literal value is any expression that will return a single token. For example `Token::OCB` or `Token::Value(5)`.

A group is a set of tokens enclosed in parentheses. For example `(Token::OCB, Token::CCB)` would be a group containing two literal values. Groups can be nested.

A value can also be a saving value which means the match is saved when it's found. Saving values are indicated by square brackets and have two forms:
1. Simple saves
2. Internal saves

A simple save just saves whatever is inside the saving value. For example `[Token::OCB]` would save the found `Token::OCB` token. These can also be nested in modifiers, so `([Token::CCB])*` would save 0+ repetitions of `Token::CCB` into a `Vec<Token::CCB>`.

An internal save is used to access the value contained in an enum variant. These are more complex and require you to specify the types expected in the enum variant after the variant name. These have the syntax `[EnumVariant; (enum_value1, enum_value2, ...)]`. If we were to use this with `Token::Other` we would have `[Token::Other; (char)]` which would match any `Token::Other` and save the `char` value. As with the simple variant, these can also be nested, so `([Token::Other; (char)])+` would match 1+ occurrences of `Token::Other` with any value and save it in a `Vec<char>`.

The final value type is a call. A call is a reference to another rule (or the same rule). This means that you can (and should) separate your language into separate rules and call different rules for other rules. the syntax for a rule is an at symbol followed by the rule name. For example, we might have the following two rules

```rust
(root; Token::OCB, ([@expr])*, Token::CCB)
(expr; [Token::Value; (u8)])
```

The resultant enum for these rules would be

```rust
#[allow(non_camel_case_types)]
enum CFLPToken {
  root(Vec<CFLPToken>),
  expr(u8)
}
```

If a rule is called without any modifier or a conditional modifier, the inner token is wrapped in a `Box` struct.

### Modifiers

Modifiers change how values are matched and have three variations:
- `()*` for a Kleene closure, matching the inner group 0+ times
- `()+` for a positive closure, matching the inner group 1+ times
- `()?` for an optional closure, matching the inner group if it's there and advancing if not

All of these must be applied to groups, not literals. So `Token::OCB+` is not valid syntax but `(Token::OCB)+` is.

A saving group is a modifier or value enclosed in square brackets. Anything enclosed in these will be saved into a resultant enum. For example if you had a rule `(rule; Token::OCB, ([Token::Value(1)])*, Token::CCB)`, this would match a `Token::OCB` followed by 0+ `Token::Value(1)` tokens which would be stored in a `Vec<Token>`, and then a `Token::CCB` token.

Anything inside a saving group must be a literal.

The resultant types for each variant is as follows, where `T` is the type returned from the modifier group:
- `()*` returns `Vec<T>`
- `()+` returns `Vec<T>`
- `()?` returns `Option<T>`

The pseudo regex syntax used is a modified version of regex with the following syntax:
- **Concatenation**

  Concatenating two sections means placing them with a comma in between. For example `Token::Token1,Token::Token2` would match `Token::Token1` then `Token::Token2`
- **Blocks**

  Blocks can be made by surrounding tokens in round brackets such as `(Token::Token1 Token::Token2)`. Blocks can be nested
- **Optional blocks**

  Optional blocks are suffixed by a question mark `?`. For example `Token::Token1?`
- **Function calls**

  Context-free grammars are unique in their calls of other rules, or in this case functions. Calls to other rules are surrounded by a `call` closure such as `call(some_rule)`
- **Positive and Kleene closures**

  Blocks can be suffixed by a `+` or `*` for a positive or Kleene closure respectively. For example `(Token::Token1,Token::Token2)+` would match `Token::Token1` then `Token::Token2` as a whole at least once
- **Block saving**

  Saved blocks are individual tokens with a type after it and has the form `{token, type}` (for example `{Token::Token1, u8}`). The type has to match the data stored in the token enum variant. In the given example, `Token::Token1` would have a `u8` contained within it. Saved blocks determine the data stored inside the `Block` enum variant for the rule.

  For example if one of the rules was `root, {Token::Token1} {Token::Token2}? {Token::Token3}*` where `Token::Token1` had a `u8` in it, `Token::Token2` had a `u16`, and `Token::Token3` has a `u32`, `Block` would contain the variant `root(u8, Option<u16>, Vec<u32>)`

## Errors

The macro can cause a compilation error in the following cases:
- **Rules with the same name**

  If two or more rules have the same name, functions and enum variants will be generated with the same name which will cause a compilation error
- **Invalid rule matching syntax**

  If the pseudo regex rule is not valid, a compilation error will be raised
- **Matched block type traits**

  Each type that is matched requires `Default` and `PartialEq` to be implemented for it. This is used to generate default blocks to compare these to input blocks
- **Things not existing**

  If you try to use things that don't exist, you'll get an error. That's completely your fault

## Current issues

Currently, most macro arguments are checked fully. Some are not:
- Doc comments can only be simple. No multiline or markdown just simple unformatted strings unfortunately
- The data contained with a source token variant is not checked because it cannot be without requiring the source token be declared by the macro

//! An example that won't generate the types for you
//!
//! This example is an equivalent to example2 but using the `#[parser]` attribute macro and showing
//! how to change the used enum variant names. If you want the equivalent code have a look in
//! `example2::equivalent`

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
	OP,
	CP,
	Literal(u8),
	Other(char),
}

#[derive(Debug)]
pub struct Token {
	pub ps: usize,
	pub pe: usize,
	pub _type: TokenType,
}

impl PartialEq<TokenType> for &Token {
	fn eq(&self, other: &TokenType) -> bool { &self._type == other }
}

// You need to import cflp::Parser because the attribute macros won't do it for you
use cflp::{parser, Parser};
// Declare a struct Root and then use the attribute macro #[parser] to generate the Parser impl for
// that type
#[derive(Debug, Clone)]
#[parser(Token, TokenType, |t| t._type.clone(); TokenType::OP, ([@Expr])*, TokenType::CP)]
struct Root(Vec<Expr>);
// Declare an enum (Or rule) with the #[parser] attribute to generate the Parser impl. This will
// take the variant names from the enum in order of declaration
#[derive(Debug, Clone)]
#[parser(Token, TokenType, |t| t._type.clone(); [TokenType::Literal; u8]; [TokenType::Other; char])]
enum Expr {
	Lit(u8),
	Other(char),
}

fn main() {
	let sample_input = vec![
		Token {
			ps: 0,
			pe: 1,
			_type: TokenType::OP,
		},
		Token {
			ps: 1,
			pe: 2,
			_type: TokenType::Literal(1),
		},
		Token {
			ps: 2,
			pe: 3,
			_type: TokenType::Other('b'),
		},
		Token {
			ps: 3,
			pe: 4,
			_type: TokenType::Literal(3),
		},
		Token {
			ps: 4,
			pe: 5,
			_type: TokenType::CP,
		},
	];
	let mut input_iterator = sample_input.iter().peekable();
	while input_iterator.peek().is_some() {
		match Root::parse(&mut input_iterator) {
			Ok(r) => println!("{:?}", r),
			Err(e) => {
				println!("{:?}", e);
				break
			}
		}
	}
}

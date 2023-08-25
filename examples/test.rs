mod prelude {
    //! General things we need as inputs
	use std::fmt::{Debug, Formatter};
	
    pub enum Token {
        Digit(usize),
        Ident(String),
        Check(u8, u8),
        Comma, SemiColon, Dot,
        Default
    }
	
	impl Clone for Token {
		fn clone(&self) -> Self {
			match self {
				Token::Digit(i) => Token::Digit(*i),
				Token::Ident(i) => Token::Ident(i.clone()),
				Token::Check(i, j) => Token::Check(*i, *j),
				Token::Comma => Token::Comma,
				Token::SemiColon => {}
				Token::Dot => Token::Dot,
				Token::Default => Token::Default
			}
		}
	}
	
	impl Debug for Token {
		fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
			match self {
				Token::Digit(i) => write!(f, "{}", i),
				Token::Ident(i) => write!(f, "{}", i),
				Token::Check(i, j) => write!(f, "({},{})", i, j),
				Token::Comma => write!(f, ","),
				Token::SemiColon => write!(f, ";"),
				Token::Dot => write!(f, ".."),
				Token::Default => write!(f, "*")
			}
		}
	}
	
	impl PartialEq for Token {
		fn eq(&self, other: &Self) -> bool {
			match (self, other) {
				(Token::Digit(i), Token::Digit(j)) => i == j,
				(Token::Ident(i), Token::Ident(j)) => i == j,
				(Token::Check(i1, i2), Token::Check(j1, j2)) => i1 == j1 && i2 == j2,
				(Token::Comma, Token::Comma) => true,
				(Token::SemiColon, Token::SemiColon) => true,
				(Token::Dot, Token::Dot) => true,
				(Token::Default, Token::Default) => true,
				_ => false
			}
		}
	}
    
    impl Default for Token {
        fn default() -> Self {
            Self::Default
        }
    }
    
    impl PartialEq<Token> for &Token {
        fn eq(&self, other: &Token) -> bool {
            self == other
        }
    }
}

use cflp::Parser;
use prelude::Token;

#[derive(Parser)]
#[parser(Token, Token; [Token::Check(a, b)], [Token::Check(a, b)])]
pub struct Test((u8, u8), (u8, u8));

#[derive(Parser)]
#[parser(Token, Token)]
pub enum Sep {
	#[parser([Token::Comma])]
	Comma(Token),
	#[parser(Token::SemiColon)]
	SemiColon,
	#[parser(Token::Dot, Token::Dot)]
	Dot
}

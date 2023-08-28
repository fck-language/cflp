//! A simple example
//!
//! This example gives some really simple parsing examples with simple token inputs.

mod prelude {
    //! General things we need as inputs
    #[derive(Debug, Clone, PartialEq)]
    pub enum Token {
        Digit(usize),
        Ident(String),
        Comma, SemiColon, Dot,
        Default
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

mod nodes {
    //! This module contains the structs and enums we'll be deriving the [`Parser`] trait for.
    //!
    //! The expended impls are in the [`expanded`](crate::expanded) module
    use cflp::Parser;
    use crate::Token;
    
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, Token; ([@Value])+, [@Sep], ([@Value])*, (@Sep)?)]
    pub struct Base {
        first: Vec<Value>,
        sep: Sep,
        last: Vec<Value>
    }
    
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, Token)]
    pub enum Value {
        #[parser([Token::Digit(t)])]
        Int(usize),
        #[parser([Token::Ident(t)])]
        Ident(String)
    }
    
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, Token)]
    pub enum Sep {
        #[parser([Token::Comma])]
        Comma(Token),
        #[parser(Token::SemiColon)]
        SemiColon,
        #[parser(Token::Dot, Token::Dot)]
        Dot
    }
}

#[ignore]
mod expanded {
    #![allow(dead_code, unused)]
    use cflp::Parser;
    use crate::Token;
    
    #[derive(Debug, Clone)]
    pub struct Base {
        first: Vec<Value>,
        sep: Sep,
        last: Vec<Value>,
    }
    
    #[automatically_derived]
    impl<'a> cflp::Parser<&'a Token, Token, Self> for Base {
        fn parse_with_recursion<T: Iterator<Item = &'a Token> + Clone>(src: &mut T, recurse: bool) -> Result<Self, cflp::Error<&'a Token, Token>> {
            let first = {
                let first_0 = {
                    let first_0_0 = {
                        match <Value as cflp::Parser<_, _, _>>::parse(src) {
                            Ok(t) => t,
                            Err(e) => return Err(e),
                        }
                    };
                    first_0_0
                };
                let mut first_out = <[_]>::into_vec(
                    #[rustc_box]
                    ::alloc::boxed::Box::new([first_0]),
                );
                loop {
                    let src_old = src.clone();
                    match 'l0: {
                        let first_0_0 = {
                            match <Value as cflp::Parser<_, _, _>>::parse(src) {
                                Ok(t) => t,
                                Err(e) => break 'l0 Err(e),
                            }
                        };
                        Ok(first_0_0)
                    } {
                        Ok(t) => first_out.push(t),
                        Err(_) => {
                            *src = src_old;
                            break;
                        }
                    }
                }
                first_out
            };
            let sep = {
                match <Sep as cflp::Parser<_, _, _>>::parse(src) {
                    Ok(t) => t,
                    Err(e) => return Err(e),
                }
            };
            let last = {
                let mut last_out = Vec::new();
                loop {
                    let src_old = src.clone();
                    match 'l0: {
                        let last_0_0 = {
                            match <Value as cflp::Parser<_, _, _>>::parse(src) {
                                Ok(t) => t,
                                Err(e) => break 'l0 Err(e),
                            }
                        };
                        Ok(last_0_0)
                    } {
                        Ok(t) => last_out.push(t),
                        Err(_) => {
                            *src = src_old;
                            break;
                        }
                    }
                }
                last_out
            };
            let src_old = src.clone();
            if 'l0: {
                if let Err(e) = <Sep as cflp::Parser<_, _, _>>::parse(src) {
                    break 'l0 Err(e);
                }
                Ok(())
            }
            .is_err()
            {
                *src = src_old;
            }
            return Ok(Self { first, sep, last });
        }
    }
    
    #[derive(Debug, Clone)]
    pub enum Value {
        Int(usize),
        Ident(String),
    }
    
    #[automatically_derived]
    impl<'a> cflp::Parser<&'a Token, Token, Self> for Value {
        fn parse_with_recursion<T: Iterator<Item = &'a Token> + Clone>(src: &mut T, recurse: bool) -> Result<Self, cflp::Error<&'a Token, Token>> {
            match src.next() {
                Some(t_unwrapped) => match t_unwrapped {
                    Token::Digit(t) => Ok(Value::Int(t.clone())),
                    Token::Ident(t) => Ok(Value::Ident(t.clone())),
                    _ => Err(cflp::Error { expected: Token::Digit(Default::default()), found: Some(t_unwrapped) }),
                },
                _ => Err(cflp::Error { expected: Token::Digit(Default::default()), found: None }),
            }
        }
    }
    
    #[derive(Debug, Clone)]
    pub enum Sep {
        Comma(Token),
        SemiColon,
        Dot,
    }
    
    #[automatically_derived]
    impl<'a> cflp::Parser<&'a Token, Token, Self> for Sep {
        fn parse_with_recursion<T: Iterator<Item = &'a Token> + Clone>(src: &mut T, recurse: bool) -> Result<Self, cflp::Error<&'a Token, Token>> {
            let first_err;
            let src_old = src.clone();
            match match src.next() {
                Some(t_unwrapped) => match t_unwrapped {
                    __next_unwrapped @ Token::Comma => Ok(Sep::Comma(__next_unwrapped.clone())),
                    Token::SemiColon => Ok(Sep::SemiColon),
                    _ => Err(cflp::Error { expected: Token::Comma, found: Some(t_unwrapped) }),
                },
                _ => Err(cflp::Error { expected: Token::Comma, found: None }),
            } {
                Ok(t) => return Ok(t),
                Err(e) => {
                    first_err = e;
                    *src = src_old;
                }
            }
            let src_old = src.clone();
            match 'l0: {
                let next = src.next();
                if next != Some(&Token::Dot) {
                    break 'l0 Err(cflp::Error { expected: Token::Dot, found: next });
                }
                let next = src.next();
                if next != Some(&Token::Dot) {
                    break 'l0 Err(cflp::Error { expected: Token::Dot, found: next });
                }
                break 'l0 Ok(Sep::Dot);
            } {
                Ok(t) => return Ok(t),
                Err(_) => *src = src_old,
            }
            return Err(first_err);
        }
    }
}

use nodes::*;
use prelude::*;
use cflp::Parser;

fn main() {
    use crate::Token::*;
    let sample_input = vec![
        Digit(0), Ident("hello".to_string()),
        SemiColon,
        Ident("world".to_string()), Ident("!".to_string()), Digit(48879),
    ];
    let mut input_iterator = sample_input.iter().peekable();
    match Base::parse(&mut input_iterator) {
        Ok(r) => println!("{:?}", r),
        Err(e) => println!("Error: {:?}", e)
    }
    if input_iterator.peek().is_some() {
        println!("Unused tokens:");
        for i in input_iterator {
            println!("- {:?}", i)
        }
    } else {
        println!("Tokens were fully consumed")
    }
}

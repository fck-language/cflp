//! A simple example
//!
//! This example gives some really simple parsing examples with simple token inputs.
//!
//! The derived [`Parser`](cflp::Parser) impls are available in [`expanded`]

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Digit(usize),
    Ident(String),
    Comma, SemiColon, Dot
}

impl PartialEq<Token> for &Token {
    fn eq(&self, other: &Token) -> bool {
        self == other
    }
}

mod nodes {
    #![allow(dead_code)]
    use cflp::Parser;
    use crate::Token;
    
    #[derive(Debug, Clone)]
    #[derive(Parser)]
    #[parser(Token, Token; ([@Value])+, [@Sep], ([@Value])+)]
    pub struct Base {
        first: Vec<Value>,
        sep: Sep,
        last: Vec<Value>
    }
    
    #[derive(Debug, Clone)]
    #[derive(Parser)]
    #[parser(Token, Token)]
    pub enum Value {
        #[parser([Token::Digit(t)])]
        Int(usize),
        #[parser([Token::Ident(t)])]
        Ident(String)
    }
    
    #[derive(Debug, Clone)]
    #[derive(Parser)]
    #[parser(Token, Token)]
    pub enum Sep {
        #[parser(Token::Comma)]
        Comma,
        #[parser(Token::SemiColon)]
        SemiColon,
        #[parser(Token::Dot)]
        Dot
    }
}

mod expanded {
    #![allow(dead_code)]
    use cflp::Parser;
    use crate::Token;
    
    #[derive(Debug, Clone)]
    pub struct Base {
        first: Vec<Value>,
        sep: Sep,
        last: Vec<Value>
    }
    
    impl<'a> cflp::Parser<&'a Token, Token, Self> for Base {
        fn parse<T: Iterator<Item = &'a Token> + Clone>(src: &mut T) -> Result<Self, cflp::Error<&'a Token, Token>> {
            let first = {
                let first_0 = {
                    let first_0_0 = {
                        match Value::parse(src) {
                            Ok(t) => t,
                            Err(e) => return Err(e),
                        }
                    };
                    first_0_0
                };
                let mut first_out = vec![first_0];
                loop {
                    let src_old = src.clone();
                    match 'l0: {
                        let first_0_0 = {
                            match Value::parse(src) {
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
                match Sep::parse(src) {
                    Ok(t) => t,
                    Err(e) => return Err(e),
                }
            };
            let last = {
                let last_0 = {
                    let last_0_0 = {
                        match Value::parse(src) {
                            Ok(t) => t,
                            Err(e) => return Err(e),
                        }
                    };
                    last_0_0
                };
                let mut last_out = vec![last_0];
                loop {
                    let src_old = src.clone();
                    match 'l0: {
                        let last_0_0 = {
                            match Value::parse(src) {
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
            return Ok(Self { first, sep, last });
        }
    }
    
    #[derive(Debug, Clone)]
    pub enum Value {
        Int(usize),
        Ident(String)
    }
    
    impl<'a> cflp::Parser<&'a Token, Token, Self> for Value {
        fn parse<T: Iterator<Item = &'a Token> + Clone>(src: &mut T) -> Result<Self, cflp::Error<&'a Token, Token>> {
            let first_err;
            let src_old = src.clone();
            match 'l0: {
                let v_0 = {
                    let next = src.next();
                    match next {
                        Some(Token::Digit(t)) => t.clone(),
                        _ => {
                            break 'l0 Err(cflp::Error { found: next, expected: Token::Digit(Default::default()) });
                        }
                    }
                };
                break 'l0 Ok(Value::Int(v_0));
            } {
                Ok(t) => return Ok(t),
                Err(e) => {
                    first_err = e;
                    *src = src_old;
                }
            }
            let src_old = src.clone();
            match 'l0: {
                let v_0 = {
                    let next = src.next();
                    match next {
                        Some(Token::Ident(t)) => t.clone(),
                        _ => {
                            break 'l0 Err(cflp::Error { found: next, expected: Token::Ident(Default::default()) });
                        }
                    }
                };
                break 'l0 Ok(Value::Ident(v_0));
            } {
                Ok(t) => return Ok(t),
                Err(_) => *src = src_old,
            }
            return Err(first_err);
        }
    }
    
    #[derive(Debug, Clone)]
    pub enum Sep {
        Comma,
        SemiColon,
        Dot
    }
    
    impl<'a> cflp::Parser<&'a Token, Token, Self> for Sep {
        fn parse<T: Iterator<Item = &'a Token> + Clone>(src: &mut T) -> Result<Self, cflp::Error<&'a Token, Token>> {
            let first_err;
            let src_old = src.clone();
            match 'l0: {
                let next = src.next();
                if next != Some(&Token::Comma) {
                    break 'l0 Err(cflp::Error { expected: Token::Comma, found: next });
                }
                break 'l0 Ok(Sep::Comma);
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
                if next != Some(&Token::SemiColon) {
                    break 'l0 Err(cflp::Error { expected: Token::SemiColon, found: next });
                }
                break 'l0 Ok(Sep::SemiColon);
            } {
                Ok(t) => return Ok(t),
                Err(_) => *src = src_old,
            }
            let src_old = src.clone();
            match 'l0: {
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
use cflp::Parser;

fn main() {
    use crate::Token::*;
    let sample_input = vec![
        Digit(0), Ident("hello".to_string()),
        SemiColon,
        Ident("world".to_string()), Ident("!".to_string()), Digit(48879)
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

//! A more self-referential example
//!
//! This example demonstrates that self-referential types don't result in a stack overflow when
//! they parse nothing before branching

mod prelude {
    //! General things we need as inputs
    use cflp::NodeData;
    
    #[derive(Debug, Copy, Clone, PartialEq)]
    pub enum TokType {
        OP,
        CP,
        Plus,
        Literal(u8),
        Other(char),
    }
    
    impl Default for TokType {
        // this value is only used to prevent stack overflows in parsing and
        // is ultimately a placeholder
        fn default() -> Self {
            Self::OP
        }
    }
    
    #[derive(Debug)]
    pub struct Token {
        pub ps: usize,
        pub pe: usize,
        pub t: TokType,
    }
    
    impl NodeData<usize> for Token {
        fn start(&self) -> usize { self.ps }
        fn end(&self) -> usize { self.pe }
    }
    
    impl Into<TokType> for &Token {
        fn into(self) -> TokType {
            self.t
        }
    }
}

mod nodes {
    //! This module contains the structs and enums we'll be deriving the [`Parser`] trait for.
    //!
    //! The expended impls are in the [`expanded`](crate::expanded) module
    use cflp::{NodeWrapper, Parser};
    use crate::{Token, TokType};
    
    /// # Expression
    ///
    /// This contains self-referential zero-consumption branches ([`Add`](Expr::Add))
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, TokType, usize)]
    pub enum Expr {
        #[parser(TokType::OP, [[@Expr]], TokType::CP)]
        Bracketed(Box<NodeWrapper<Self, usize>>),
        #[parser([[@Expr]], (TokType::Plus, [@Expr])+)]
        Add {
            first: Box<NodeWrapper<Self, usize>>,
            rem: Vec<NodeWrapper<Self, usize>>
        },
        #[parser([@ExprLit])]
        Literal(NodeWrapper<ExprLit, usize>)
    }
    
    /// # Literal expression
    ///
    /// Simple enum that contains literals only
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, TokType, usize)]
    pub enum ExprLit {
        #[parser([TokType::Literal(t)])]
        Lit(u8),
        #[parser([TokType::Other(t)])]
        Char(char)
    }
}

mod expanded {
    use cflp::{NodeWrapper, Parser};
    use crate::{Token, TokType};
    
    #[derive(Debug, Clone)]
    pub enum Expr {
        Bracketed(Box<NodeWrapper<Self, usize>>),
        Add { first: Box<NodeWrapper<Self, usize>>, rem: Vec<NodeWrapper<Self, usize>> },
        Literal(NodeWrapper<ExprLit, usize>),
    }
    
    #[automatically_derived]
    impl<'a> cflp::Parser<&'a Token, TokType, cflp::NodeWrapper<Expr, usize>> for Expr {
        fn parse_with_recursion<T: Iterator<Item = &'a Token> + Clone>(
            src: &mut T,
            recurse: bool,
        ) -> Result<cflp::NodeWrapper<Expr, usize>, cflp::Error<&'a Token, TokType>> {
            use cflp::NodeData;
            let mut start = <usize as Default>::default();
            let mut end = <usize as Default>::default();
            let first_err;
            let src_old = src.clone();
            match 'l0: {
                let next = src.next();
                if let Some(__next) = next {
                    if Into::<TokType>::into(__next) != TokType::OP {
                        break 'l0 Err(cflp::Error {
                            expected: TokType::OP,
                            found: next,
                        });
                    }
                    start = __next.start();
                } else {
                    break 'l0 Err(cflp::Error {
                        expected: TokType::OP,
                        found: next,
                    })
                };
                let v_0 = {
                    match <Expr as cflp::Parser<_, _, _>>::parse(src) {
                        Ok(t) => Box::new(t),
                        Err(e) => break 'l0 Err(e),
                    }
                };
                let next = src.next();
                if let Some(__next) = next {
                    if Into::<TokType>::into(__next) != TokType::CP {
                        break 'l0 Err(cflp::Error {
                            expected: TokType::CP,
                            found: next,
                        });
                    }
                    end = __next.end();
                } else {
                    break 'l0 Err(cflp::Error {
                        expected: TokType::CP,
                        found: next,
                    })
                };
                break 'l0 Ok(Expr::Bracketed(v_0));
            } {
                Ok(t) => {
                    return Ok(cflp::NodeWrapper {
                        node: t,
                        start,
                        end,
                    });
                }
                Err(e) => {
                    first_err = e;
                    *src = src_old;
                }
            }
            let src_old = src.clone();
            match 'l0: {
                let first = {
                    if !recurse {
                        break 'l0 Err(cflp::Error {
                            expected: Default::default(),
                            found: None,
                        });
                    }
                    match Expr::parse_with_recursion(src, false) {
                        Ok(t) => {
                            start = t.start();
                            Box::new(t)
                        }
                        Err(e) => break 'l0 Err(e),
                    }
                };
                let rem = {
                    let mut rem_out = Vec::new();
                    rem_out
                        .push(
                            'l1: {
                                let next = src.next();
                                if next.clone().map(Into::<TokType>::into)
                                    != Some(TokType::Plus)
                                {
                                    break 'l1 Err(cflp::Error {
                                        expected: TokType::Plus,
                                        found: next,
                                    });
                                }
                                let rem_out_0 = {
                                    match Expr::parse_with_recursion(src, false) {
                                        Ok(t) => {
                                            end = t.end();
                                            t
                                        }
                                        Err(e) => break 'l1 Err(e),
                                    }
                                };
                                Ok(rem_out_0)
                            }?,
                        );
                    loop {
                        let src_old = src.clone();
                        let end_old = end;
                        match 'l1: {
                            let next = src.next();
                            if next.clone().map(Into::<TokType>::into)
                                != Some(TokType::Plus)
                            {
                                break 'l1 Err(cflp::Error {
                                    expected: TokType::Plus,
                                    found: next,
                                });
                            }
                            let rem_out_0 = {
                                match Expr::parse_with_recursion(src, false) {
                                    Ok(t) => {
                                        end = t.end();
                                        t
                                    }
                                    Err(e) => break 'l1 Err(e),
                                }
                            };
                            Ok(rem_out_0)
                        } {
                            Ok(__t) => rem_out.push(__t),
                            _ => {
                                *src = src_old;
                                end = end_old;
                                break;
                            }
                        }
                    }
                    rem_out
                };
                break 'l0 Ok(Expr::Add { first, rem });
            } {
                Ok(t) => {
                    return Ok(cflp::NodeWrapper {
                        node: t,
                        start,
                        end,
                    });
                }
                Err(_) => *src = src_old,
            }
            let src_old = src.clone();
            match 'l0: {
                let v_0 = {
                    match ExprLit::parse_with_recursion(src, false) {
                        Ok(t) => {
                            start = t.start();
                            end = t.end();
                            t
                        }
                        Err(e) => break 'l0 Err(e),
                    }
                };
                break 'l0 Ok(Expr::Literal(v_0));
            } {
                Ok(t) => {
                    return Ok(cflp::NodeWrapper {
                        node: t,
                        start,
                        end,
                    });
                }
                Err(_) => *src = src_old,
            }
            return Err(first_err);
        }
    }
    
    #[derive(Debug, Clone)]
    pub enum ExprLit {
        Lit(u8),
        Char(char),
    }
    
    #[automatically_derived]
    impl<'a> cflp::Parser<&'a Token, TokType, cflp::NodeWrapper<ExprLit, usize>> for ExprLit {
        fn parse_with_recursion<T: Iterator<Item = &'a Token> + Clone>(
            src: &mut T,
            recurse: bool,
        ) -> Result<cflp::NodeWrapper<ExprLit, usize>, cflp::Error<&'a Token, TokType>> {
            match src.next() {
                Some(t_unwrapped) => {
                    let start = <Token as cflp::NodeData<usize>>::start(t_unwrapped);
                    let end = <Token as cflp::NodeData<usize>>::end(t_unwrapped);
                    match Into::<TokType>::into(t_unwrapped) {
                        TokType::Literal(t) => {
                            Ok(cflp::NodeWrapper {
                                start,
                                end,
                                node: ExprLit::Lit(t.clone()),
                            })
                        }
                        TokType::Other(t) => {
                            Ok(cflp::NodeWrapper {
                                start,
                                end,
                                node: ExprLit::Char(t.clone()),
                            })
                        }
                        _ => {
                            Err(cflp::Error {
                                expected: TokType::Literal(Default::default()),
                                found: Some(t_unwrapped),
                            })
                        }
                    }
                }
                _ => {
                    Err(cflp::Error {
                        expected: TokType::Literal(Default::default()),
                        found: None,
                    })
                }
            }
        }
    }
}

use nodes::*;
use prelude::*;
use cflp::Parser;

fn main() {
    let sample_input = vec![
        Token { ps: 1, pe: 2, t: TokType::Literal(1) },
        Token { ps: 0, pe: 1, t: TokType::Plus },
        Token { ps: 0, pe: 1, t: TokType::OP },
        Token { ps: 1, pe: 2, t: TokType::Literal(1) },
        Token { ps: 0, pe: 1, t: TokType::Plus },
        Token { ps: 2, pe: 3, t: TokType::Other('b') },
        Token { ps: 4, pe: 5, t: TokType::CP }
    ];
    let mut input_iterator = sample_input.iter().peekable();
    match Expr::parse(&mut input_iterator) {
        Ok(r) => println!("{:?}", r),
        Err(e) => println!("Error: {:?}", e)
    }
    if input_iterator.peek().is_some() {
        println!("Unused tokens:");
        for i in input_iterator {
            println!("- {:?}", i)
        }
    } else {
        println!("All tokens used")
    }
}

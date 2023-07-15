//! A more momplex example
//!
//! This example demonstrates the use of [`NodeData`] and [`NodeWrapper`] to preserve positional
//! data when parsing. It also requires the use of boxed matches.

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
    
    #[derive(Debug)]
    pub struct Token {
        pub ps: usize,
        pub pe: usize,
        pub t: TokType,
    }
    
    impl PartialEq<TokType> for &Token {
        fn eq(&self, other: &TokType) -> bool { &self.t == other }
    }
    
    impl NodeData<usize> for Token {
        fn start(&self) -> usize { self.ps }
        fn end(&self) -> usize { self.pe }
    }
}

mod nodes {
    //! This module contains the structs and enums we'll be deriving the [`Parser`] trait for.
    //!
    //! The expended impls are in the [`expanded`](crate::expanded) module
    use cflp::{NodeWrapper, Parser};
    use crate::{Token, TokType};
    
    /// # Root match struct
    ///
    /// Matches as many [`Add`] repetitions as possible
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, TokType, |t| t.t, usize; ([@Add])*)]
    pub struct Root(pub Vec<NodeWrapper<Add, usize>>);
    
    /// # Add struct
    ///
    /// Matches one [expression](Expr) plus another [expression](Expr)
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, TokType, |t| t.t, usize; [[@Expr]], TokType::Plus, [[@Expr]])]
    pub struct Add {
        pub left: NodeWrapper<Box<Expr>, usize>,
        pub right: NodeWrapper<Box<Expr>, usize>
    }
    
    /// # Expression
    ///
    /// This matches either an [add expression](Add) in parentheses, a digits (`u8`), or a
    /// character (`char`)
    #[derive(Debug, Clone, Parser)]
    #[parser(Token, TokType, |t: &Token| t.t, usize)]
    pub enum Expr {
        #[parser(TokType::OP, [@Add], TokType::CP)]
        Paren { inner: NodeWrapper<Add, usize> },
        #[parser([TokType::Literal(t)])]
        Lit(u8),
        #[parser([TokType::Other(t)])]
        Other(char),
    }
}

mod expanded {
    //! Expanded [`Parser`](cflp::Parser) impls from the derive macro
    //!
    //! *Please note*: This section may be highlighted as including errors. The impls do not contain
    //! errors. I don't know why this happens
    // You need to import cflp::Parser because the attribute macros won't do it for you
    use cflp::{Parser, NodeWrapper};
    use crate::{Token, TokType};
    
    #[derive(Debug, Clone)]
    pub struct Root(pub Vec<NodeWrapper<Add, usize>>);
    
    impl<'a> cflp::Parser<&'a Token, TokType, cflp::NodeWrapper<Root, usize>> for Root {
        fn parse<T: Iterator<Item = &'a Token> + Clone>(
            src: &mut T,
        ) -> Result<cflp::NodeWrapper<Root, usize>, cflp::Error<&'a Token, TokType>> {
            use cflp::NodeData;
            let mut start = Default::default();
            let mut end = Default::default();
            let v_0 = {
                let mut v_0_out = Vec::new();
                let src_old = src.clone();
                match 'l0: {
                    let v_0_out_0 = {
                        match Add::parse(src) {
                            Ok(t) => {
                                start = t.start();
                                end = t.end();
                                t
                            }
                            Err(e) => break 'l0 Err(e),
                        }
                    };
                    Ok(v_0_out_0)
                } {
                    Ok(t) => v_0_out.push(t),
                    _ => {
                        *src = src_old;
                        start = Default::default();
                        end = Default::default();
                    }
                }
                if !v_0_out.is_empty() {
                    loop {
                        let src_old = src.clone();
                        let end_old = end;
                        match 'l0: {
                            let v_0_out_0 = {
                                match Add::parse(src) {
                                    Ok(t) => {
                                        end = t.end();
                                        t
                                    }
                                    Err(e) => break 'l0 Err(e),
                                }
                            };
                            Ok(v_0_out_0)
                        } {
                            Ok(t) => v_0_out.push(t),
                            _ => {
                                *src = src_old;
                                end = end_old;
                                break;
                            }
                        }
                    }
                }
                v_0_out
            };
            return Ok(cflp::NodeWrapper {
                node: Self(v_0),
                start,
                end,
            });
        }
    }
    
    #[derive(Debug, Clone)]
    pub struct Add {
        pub left: NodeWrapper<Box<Expr>, usize>,
        pub right: NodeWrapper<Box<Expr>, usize>
    }
    
    impl<'a> cflp::Parser<&'a Token, TokType, cflp::NodeWrapper<Add, usize>> for Add {
        fn parse<T: Iterator<Item = &'a Token> + Clone>(
            src: &mut T,
        ) -> Result<cflp::NodeWrapper<Add, usize>, cflp::Error<&'a Token, TokType>> {
            use cflp::NodeData;
            let mut start = Default::default();
            let mut end = Default::default();
            let left = {
                match Expr::parse(src) {
                    Ok(t) => {
                        start = t.start();
                        NodeWrapper {
                            node: Box::new(t.node),
                            start: t.start,
                            end: t.end,
                        }
                    }
                    Err(e) => return Err(e),
                }
            };
            let next = src.next();
            if next != Some(TokType::Plus) {
                return Err(cflp::Error {
                    expected: TokType::Plus,
                    found: next,
                });
            }
            let right = {
                match Expr::parse(src) {
                    Ok(t) => {
                        end = t.end();
                        NodeWrapper {
                            node: Box::new(t.node),
                            start: t.start,
                            end: t.end,
                        }
                    }
                    Err(e) => return Err(e),
                }
            };
            return Ok(cflp::NodeWrapper {
                node: Self { left, right },
                start,
                end,
            });
        }
    }
    
    #[derive(Debug, Clone)]
    pub enum Expr {
        Paren { inner: NodeWrapper<Add, usize> },
        Lit(u8),
        Other(char),
    }
    
    impl<'a> cflp::Parser<&'a Token, TokType, cflp::NodeWrapper<Expr, usize>> for Expr {
        fn parse<T: Iterator<Item = &'a Token> + Clone>(
            src: &mut T,
        ) -> Result<cflp::NodeWrapper<Expr, usize>, cflp::Error<&'a Token, TokType>> {
            use cflp::NodeData;
            let mut start = <usize as Default>::default();
            let mut end = <usize as Default>::default();
            let first_err;
            let src_old = src.clone();
            match match src.next() {
                Some(t_unwrapped) => {
                    let start = <Token as NodeData<usize>>::start(t_unwrapped);
                    let end = <Token as NodeData<usize>>::end(t_unwrapped);
                    match (|t: &Token| t.t)(t_unwrapped) {
                        TokType::Literal(t) => {
                            Ok(cflp::NodeWrapper {
                                start,
                                end,
                                node: Expr::Lit(t.clone()),
                            })
                        }
                        TokType::Other(t) => {
                            Ok(cflp::NodeWrapper {
                                start,
                                end,
                                node: Expr::Other(t.clone()),
                            })
                        }
                        t => {
                            Err(cflp::Error {
                                expected: TokType::Literal(Default::default()),
                                found: Some(t),
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
                if let Some(__next) = next {
                    if (|t: &Token| t.t)(__next) != TokType::OP {
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
                let inner = {
                    match Add::parse(src) {
                        Ok(t) => t,
                        Err(e) => break 'l0 Err(e),
                    }
                };
                let next = src.next();
                if let Some(__next) = next {
                    if (|t: &Token| t.t)(__next) != TokType::CP {
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
                break 'l0 Ok(Expr::Paren { inner });
            } {
                Ok(t) => return Ok(NodeWrapper { node: t, start, end }),
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
    match Root::parse(&mut input_iterator) {
        Ok(r) => println!("{:?}", r),
        Err(e) => println!("Error: {:?}", e)
    }
    if input_iterator.peek().is_some() {
        println!("Unused tokens:");
        for i in input_iterator {
            println!("- {:?}", i)
        }
    }
}

use crate::complex::prelude::*;
use cflp::{Error, Parser};

#[derive(Debug, Clone)]
pub struct Root<'a>(Vec<Statement<'a>>);

impl<'a> Parser<&'a Token<'a>, TokenType<'a>> for Root<'a> {
    fn parse_with_recursion<T: Iterator<Item = &'a Token<'a>> + Clone>(
        src: &mut T, recurse: bool
    ) -> Result<Self, Error<&'a Token<'a>, TokenType<'a>>>
    where
        Self: Sized,
    {
        let mut out = Vec::new();
        loop {
            let src_old = src.clone();
            match Statement::parse(src) {
                Ok(s) => out.push(s),
                Err(_) => {
                    *src = src_old;
                    break;
                }
            }
        }
        Ok(Self(out))
    }
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Var1(Expr<'a>, Vec<Box<Statement<'a>>>),
    Var2(&'a str, Expr<'a>),
    Var3(&'a str, Expr<'a>),
}

impl<'a> Parser<&'a Token<'a>, TokenType<'a>> for Statement<'a> {
    fn parse_with_recursion<T: Iterator<Item = &'a Token<'a>> + Clone>(
        src: &mut T, recurse: bool
    ) -> Result<Self, Error<&'a Token<'a>, TokenType<'a>>>
    where
        Self: Sized,
    {
        let next = match src.next() {
            None => {
                return Err(Error {
                    expected: TokenType::Kwd(Kwd::If),
                    found: None,
                })
            }
            Some(next) => next,
        };
        match next.t {
            TokenType::Kwd(Kwd::If) => {
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::OP) {
                    return Err(Error {
                        expected: TokenType::OP,
                        found: next,
                    });
                }
                let condition = match Expr::parse(src) {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                };
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::CP) {
                    return Err(Error {
                        expected: TokenType::OP,
                        found: next,
                    });
                }
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::OB) {
                    return Err(Error {
                        expected: TokenType::OP,
                        found: next,
                    });
                }
                let mut statements = Vec::new();
                loop {
                    let src_old = src.clone();
                    match Statement::parse(src) {
                        Ok(s) => statements.push(Box::new(s)),
                        Err(_) => {
                            *src = src_old;
                            break;
                        }
                    }
                }
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::CB) {
                    return Err(Error {
                        expected: TokenType::OP,
                        found: next,
                    });
                }
                Ok(Self::Var1(condition, statements))
            }
            TokenType::Kwd(Kwd::Let) => {
                let next = src.next();
                let ident = if let Some(TokenType::Ident(ident)) = next.map(|t| t.t) {
                    ident
                } else {
                    return Err(Error {
                        expected: TokenType::Ident(Default::default()),
                        found: next,
                    });
                };
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::Punc(Punc::Eq)) {
                    return Err(Error {
                        expected: TokenType::Punc(Punc::Eq),
                        found: next,
                    });
                }
                let expr = match Expr::parse(src) {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                };
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::Punc(Punc::SCol)) {
                    return Err(Error {
                        expected: TokenType::Punc(Punc::SCol),
                        found: next,
                    });
                }
                Ok(Self::Var2(ident, expr))
            }
            TokenType::Ident(ident) => {
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::Punc(Punc::Eq)) {
                    return Err(Error {
                        expected: TokenType::Punc(Punc::Eq),
                        found: next,
                    });
                }
                let expr = match Expr::parse(src) {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                };
                let next = src.next();
                if next.clone().map(|t| t.t) != Some(TokenType::Punc(Punc::SCol)) {
                    return Err(Error {
                        expected: TokenType::Punc(Punc::SCol),
                        found: next,
                    });
                }
                Ok(Self::Var3(ident, expr))
            }
            _ => Err(Error {
                expected: TokenType::Kwd(Kwd::If),
                found: Some(next),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Var1(&'a str),
    Var2(u8),
    Var3(Box<Expr<'a>>, Punc, Box<Expr<'a>>),
}

impl<'a> Parser<&'a Token<'a>, TokenType<'a>> for Expr<'a> {
    fn parse_with_recursion<T: Iterator<Item = &'a Token<'a>> + Clone>(
        src: &mut T, recurse: bool
    ) -> Result<Self, Error<&'a Token<'a>, TokenType<'a>>>
    where
        Self: Sized,
    {
        let src_old = src.clone();
        match src.next().map(|t| t.t) {
            Some(TokenType::Ident(ident)) => Ok(Self::Var1(ident)),
            Some(TokenType::Literal(value)) => Ok(Self::Var2(value)),
            _ => {
                *src = src_old;
                let expr1 = match Expr::parse(src) {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                };
                let next = src.next();
                let op = if let Some(TokenType::Punc(p)) = next.map(|t| t.t) {
                    p
                } else {
                    return Err(Error {
                        expected: TokenType::Punc(Default::default()),
                        found: next,
                    });
                };
                let expr2 = match Expr::parse(src) {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                };
                Ok(Self::Var3(Box::new(expr1), op, Box::new(expr2)))
            }
        }
    }
}

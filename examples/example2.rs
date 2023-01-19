#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
	OP, CP,
	Literal(u8)
}

#[derive(Debug)]
pub struct Token {
	pub ps: usize, pub pe: usize,
	pub _type: TokenType
}

impl PartialEq<TokenType> for &Token {
	fn eq(&self, other: &TokenType) -> bool {
		&self._type == other
	}
}

impl PartialEq<TokenType> for Option<&Token> {
	fn eq(&self, other: &TokenType) -> bool {
		if let Some(t) = self { t == other } else { false }
	}
}

mod example {
	use super::{Token, TokenType};
	use cflp::rule;
	rule!(
		(pub(crate), Token, TokenType, |t| t._type.clone(), (Debug, Clone))
		(Root; TokenType::OP, [TokenType::Literal(1)], ([TokenType::Literal; u8])*, (TokenType::CP)?)
	);
}

mod equivalent {
    use crate::{Token, TokenType};
    use cflp::rule;
    use cflp::Parser;
	#[derive(Debug, Clone)]
    pub(crate) struct Root(TokenType, Vec<((u8))>);
    impl<'a> Parser<&'a Token, TokenType> for Root {
        fn parse<T: Iterator<Item = &'a Token> + Clone>(mut src: &mut T,) -> Result<Self, cflp::Error<&'a Token, TokenType>> {
			// TokenType::OP
            let next = src.next();
            if next != TokenType::OP {
                return Err(cflp::Error {
                    expected: TokenType::OP,
                    found: next,
                });
            }
			// [TokenType::Literal(1)]
            let v_1 = {
                let next = src.next();
                if Some(TokenType::Literal(1)) == next.map(|t| t._type.clone()) {
                    (TokenType::Literal(1))
                } else {
                    return Err(cflp::Error {
                        expected: TokenType::Literal(1),
                        found: next,
                    })
                }
            };
			// ([TokenType::Literal; u8])*
            let v_2 = {
                let mut v_2_out = Vec::new();
                let mut src_old;
                loop {
					// [TokenType::Literal; u8]
                    src_old = src.clone();
                    match {
                        let v_2_0_0 = {
                            let next = src.next();
                            if let Some(TokenType::Literal(n_0))
                                = next.map(|t| t._type.clone())
                            {
                                Ok((n_0))
                            } else {
                                Err(cflp::Error {
                                    expected: TokenType::Literal(Default::default()),
                                    found: next,
                                })
                            }
                        };
                        (v_2_0_0)
                    } {
                        Ok(t) => v_2_out.push(t),
                        Err(_) => {
                            *src = src_old;
                            break;
                        }
                    }
                }
                v_2_out
            };
			// (TokenType::CP)?
            let mut src_old = src.clone();
            if loop {
                let next = src.next();
                if next != TokenType::CP {
                    break Err(cflp::Error {
                        expected: TokenType::CP,
                        found: next,
                    });
                }
                break Ok(());
            }.is_err() {
                *src = src_old;
            }
            return Ok(Self(v_1, v_2));
        }
    }
}

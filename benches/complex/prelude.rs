#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub p: (u8, u8),
    pub t: TokenType<'a>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType<'a> {
    OP,
    CP, // ( ... )
    OB,
    CB, // { ... }
    Kwd(Kwd),
    Ident(&'a str),
    Literal(u8),
    Punc(Punc),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Kwd {
    If,
    Let,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Punc {
    LT,
    GT, // < >
    Eq,
    EEq,  // = ==
    SCol, // ;
    Plus,
    Minus, // + -
    Any,   // Default
}

impl PartialEq<TokenType<'_>> for &Token<'_> {
    fn eq(&self, other: &TokenType) -> bool {
        &self.t == other
    }
}

impl Default for Punc {
    fn default() -> Self {
        Self::Any
    }
}

impl<'a> Into<TokenType<'a>> for &Token<'a> {
    fn into(self) -> TokenType<'a> {
        self.t
    }
}

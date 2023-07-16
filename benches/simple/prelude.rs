#[derive(Clone, Debug)]
pub struct Token {
    pub p: (u8, u8),
    pub t: TokenType,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenType {
    OP,
    CP,
    Punc(char),
    Value(u8),
}

impl PartialEq<TokenType> for &Token {
    fn eq(&self, other: &TokenType) -> bool {
        &self.t == other
    }
}

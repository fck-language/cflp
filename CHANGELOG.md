# Changelog

- [V0.2.0](#v020)

## V0.2.0

- Changed attribute macros on enum types. The previous macro required all rules be declared in a single macro as with struct types. The new method requires the general data (iterator type etc.) only to be declared for the enum type, and the rules for each variant be declared for that variant. For example:
    ```rust
    // Old macro
    #[parser(Token, Token, |t| t; Token::Var1; Token::Var2)]
    enum ExampleOld {
        Var1,
        Var2
    }
    
    // Old macro
    #[parser(Token, Token, |t| t)]
    enum ExampleNew {
        #[parser(Token::Var1)]
        Var1,
        #[parser(Token::Var2)]
        Var2
    }
    ```
  This has been done to improve readability and make checking rules against variant values easier

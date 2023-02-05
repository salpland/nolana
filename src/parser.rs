use logos::{Lexer, Logos};

use crate::token::Token;

pub struct Parser<'a> {
    lexer: Lexer<'a, Token<'a>>,
    token: Token<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new Molang parser.
    pub fn new(input: &'a [u8]) -> Self {
        Self {
            lexer: Token::lexer(
                // SAFETY: the caller of the function must ensure the input is valid UTF-8.
                unsafe { std::str::from_utf8_unchecked(input) },
            ),
            token: Token::Eof,
        }
    }

    /// Advances the lexer to the next token.
    fn advance_token(&mut self) {
        self.token = self.lexer.next().unwrap_or(Token::Eof);
    }
}

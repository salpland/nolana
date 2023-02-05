use logos::{Lexer, Logos};
use thiserror::Error;

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

pub struct Parser<'a> {
    lexer: Lexer<'a, Token<'a>>,
    token: Token<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new Molang parser.
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            lexer: Token::lexer(
                // SAFETY: the caller of the function must ensure the source is valid UTF-8.
                unsafe { std::str::from_utf8_unchecked(source) },
            ),
            token: Token::Eof,
        }
    }

    /// Parses a program which consists of one or more statements.
    fn parse_program(&mut self) -> Result<Program, ParseError> {
        self.advance_token();

        let mut statements = Vec::new();
        while self.token != Token::Eof {
            statements.push(self.parse_statement(statements.is_empty())?);
        }

        Ok(statements)
    }

    /// Parses a single statement.
    ///
    /// It also parses any optional statement terminators. Those determine
    /// whether the program is a simple or complex Molang expression. An error
    /// will be returned if a statement is not terminated in a complex
    /// expression.
    fn parse_statement(&mut self, is_first: bool) -> Result<Statement, ParseError> {
        let statement = match &self.token {
            Token::Number(_) => Statement::Expression(self.parse_expression()?),
            _ => return Err(ParseError::UnexpectedToken),
        };

        self.advance_token();
        if self.token != Token::Semi && !is_first {
            return Err(ParseError::UnterminatedStatement);
        }

        Ok(statement)
    }

    /// Parses a single expression.
    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        todo!()
    }

    /// Advances the lexer to the next token.
    fn advance_token(&mut self) {
        self.token = self.lexer.next().unwrap_or(Token::Eof);
    }
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("found an unexpected token")]
    UnexpectedToken,

    #[error("statement is unterminated")]
    UnterminatedStatement,
}

#[cfg(test)]
mod tests {}

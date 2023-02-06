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
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        self.advance_token();

        let mut statements = Vec::new();
        while self.token != Token::Eof {
            statements.push(self.parse_statement(statements.is_empty())?);
        }

        Ok(Program(statements))
    }

    /// Parses a single statement.
    ///
    /// It also parses any optional statement terminators. Those determine
    /// whether the program is a simple or complex Molang expression. An error
    /// will be returned if a statement is not terminated in a complex
    /// expression.
    fn parse_statement(&mut self, is_first: bool) -> Result<Statement, ParseError> {
        let statement = match &self.token {
            Token::Number(_) | Token::OpenParen => Statement::Expression(self.parse_expression(0)?),
            Token::Eof => return Err(ParseError::UnexpectedEof),
            _ => return Err(ParseError::UnexpectedToken),
        };

        self.advance_token();
        if self.token != Token::Semi && !is_first {
            return Err(ParseError::UnterminatedStatement);
        }

        Ok(statement)
    }

    /// Parses a single expression.
    ///
    /// This uses Pratt parsing as it is more efficient for expressions.
    fn parse_expression(&mut self, min_bp: u8) -> Result<Expression, ParseError> {
        let mut lhs = match &self.token {
            Token::Number(v) => Expression::Number(*v),
            Token::OpenParen => {
                self.advance_token();
                let lhs = self.parse_expression(0)?;
                self.expect_token(Token::CloseParen)?;
                lhs
            }
            _ => return Err(ParseError::UnexpectedToken),
        };

        loop {
            // We peek instead of consuming the next token to ensure cases where there is no
            // operator do not lead to a shift in the token stream.
            let op = match self.lexer.clone().next().unwrap_or(Token::Eof) {
                Token::Eof | Token::CloseParen => break,
                t if matches!(t, Token::Plus | Token::Minus | Token::Star | Token::Slash) => t,
                _ => return Err(ParseError::UnexpectedToken),
            };

            if let Some((lbp, rbp)) = op.binary_binding_power() {
                if lbp < min_bp {
                    break;
                }

                // Skip to previously peeked operator.
                self.advance_token();
                // Skip to next expression.
                self.advance_token();

                let rhs = self.parse_expression(rbp)?;
                lhs = Expression::Binary(lhs.into(), op.into(), rhs.into());
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    /// Advances the lexer to the next token.
    fn advance_token(&mut self) {
        self.token = self.lexer.next().unwrap_or(Token::Eof);
    }

    /// Advances the lexer consumnig the next token and checking whether it
    /// matches the given one or not.
    fn expect_token(&mut self, expected_token: Token) -> Result<(), ParseError> {
        self.advance_token();

        if self.token == expected_token {
            Ok(())
        } else {
            Err(ParseError::UnterminatedParen)
        }
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("found an unexpected token")]
    UnexpectedToken,

    #[error("statement is unterminated")]
    UnterminatedStatement,

    #[error("unterminated parenthesis")]
    UnterminatedParen,
}

impl<'a> Token<'a> {
    /// Returns the binding power which represents the precedence of this token.
    pub fn binary_binding_power(&self) -> Option<(u8, u8)> {
        Some(match self {
            Self::Plus | Self::Minus => (1, 2),
            Self::Star | Self::Slash => (3, 4),
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Operator;

    use super::*;

    macro_rules! text_parser {
        ($name:ident, $( ( $source:literal, $expected_output:expr ) ), +) => {
            #[test]
            fn $name() {
                $(
                    let mut parser = Parser::new($source.as_bytes());
                    let actual_output = parser.parse_program();
                    assert_eq!(actual_output, $expected_output);
                ) *
            }
        };
    }

    text_parser!(
        binary_expressions,
        (
            "1 + 2",
            Ok(Program(vec![Statement::Expression(Expression::Binary(
                Expression::Number(1.0).into(),
                Operator::Add,
                Expression::Number(2.0).into()
            ))]))
        ),
        (
            "1 + 2 * 3",
            Ok(Program(vec![Statement::Expression(Expression::Binary(
                Expression::Number(1.0).into(),
                Operator::Add,
                Expression::Binary(
                    Expression::Number(2.0).into(),
                    Operator::Mul,
                    Expression::Number(3.0).into()
                )
                .into()
            ))]))
        ),
        (
            "(1 + 2) * 3",
            Ok(Program(vec![Statement::Expression(Expression::Binary(
                Expression::Binary(
                    Expression::Number(1.0).into(),
                    Operator::Add,
                    Expression::Number(2.0).into()
                )
                .into(),
                Operator::Mul,
                Expression::Number(3.0).into()
            ))]))
        )
    );
}

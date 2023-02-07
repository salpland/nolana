use logos::{Lexer, Logos};
use thiserror::Error;

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

/// A Molang parser.
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
            Token::Number(_) | Token::OpenParen | Token::Minus | Token::Bang => {
                Statement::Expression(self.parse_expression(0)?)
            }
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
            Token::Minus | Token::Bang => {
                let op = self.token.clone();
                self.advance_token();
                // This will always return a binding power so unwrapping is fine.
                let (_, rbp) = op.binding_power(true).unwrap();
                let rhs = self.parse_expression(rbp)?;

                Expression::new_unary(op.into(), rhs)
            }
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
                Token::Eof | Token::CloseParen | Token::Colon => break,
                t if matches!(
                    t,
                    Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Question
                ) =>
                {
                    t
                }
                _ => return Err(ParseError::UnexpectedToken),
            };

            if let Some((lbp, rbp)) = op.binding_power(false) {
                if lbp < min_bp {
                    break;
                }

                // To previously peeked operator token.
                self.advance_token();
                // To next expression token.
                self.advance_token();

                lhs = if op == Token::Question {
                    let mhs = self.parse_expression(0)?;
                    self.expect_token(Token::Colon)?;
                    self.advance_token();
                    let rhs = self.parse_expression(rbp)?;

                    Expression::new_ternary(lhs, mhs, rhs)
                } else {
                    let rhs = self.parse_expression(rbp)?;

                    Expression::new_binary(lhs, op.into(), rhs)
                };
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

#[cfg(test)]
mod tests {
    use crate::ast::Operator;

    use super::*;

    macro_rules! text_parser {
        ($name:ident, $source:literal, $expected_output:expr) => {
            #[test]
            fn $name() {
                let mut parser = Parser::new($source.as_bytes());
                let actual_output = parser.parse_program();
                assert_eq!(actual_output, $expected_output);
            }
        };
    }

    text_parser!(
        binary_addition_expression,
        "1 + 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Add,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_subtraction_expression,
        "1 - 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Subtract,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_multiplication_expression,
        "1 * 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Multiply,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_division_expression,
        "1 / 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Divide,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        order_of_operations,
        "1 + 2 * 3",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Add,
            Expression::new_binary(
                Expression::Number(2.0),
                Operator::Multiply,
                Expression::Number(3.0)
            )
        ))])
    );

    text_parser!(
        forced_order_of_operations,
        "(1 + 2) * 3",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::new_binary(
                Expression::Number(1.0),
                Operator::Add,
                Expression::Number(2.0)
            ),
            Operator::Multiply,
            Expression::Number(3.0)
        ))])
    );

    text_parser!(
        unary_expression,
        "!-1",
        Ok(vec![Statement::Expression(Expression::new_unary(
            Operator::Negate,
            Expression::new_unary(Operator::Subtract, Expression::Number(1.0))
        ))])
    );

    text_parser!(
        ternary_expression,
        "0 ? 1 : 2",
        Ok(vec![Statement::Expression(Expression::new_ternary(
            Expression::Number(0.0),
            Expression::Number(1.0),
            Expression::Number(2.0)
        ))])
    );
}

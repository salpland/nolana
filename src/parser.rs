use logos::{Lexer, Logos};
use thiserror::Error;

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

/// A Molang parser.
pub struct Parser<'a> {
    lexer: Lexer<'a, Token<'a>>,
    current_token: Token<'a>,
    next_token: Token<'a>,
}

impl<'a> Parser<'a> {
    /// Creates a new Molang parser.
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            lexer: Token::lexer(
                // SAFETY: the caller of the function must ensure the source is valid UTF-8.
                unsafe { std::str::from_utf8_unchecked(source) },
            ),
            current_token: Token::Eof,
            next_token: Token::Eof,
        }
    }

    /// Parses a program which consists of one or more statements.
    pub fn parse_program(&mut self) -> Result<Program<'a>, ParseError> {
        self.first_advance();
        self.advance();

        let mut statements = Vec::new();
        while self.current_token != Token::Eof {
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
    fn parse_statement(&mut self, is_first: bool) -> Result<Statement<'a>, ParseError> {
        let statement = match (&self.current_token, &self.next_token) {
            (Token::Number(_) | Token::OpenParen | Token::Minus | Token::Bang, _)
            | (Token::Identifier(_), Token::OpenParen) => {
                Statement::Expression(self.parse_expression(0)?)
            }
            (Token::Eof, _) => return Err(ParseError::UnexpectedEof),
            _ => return Err(ParseError::UnexpectedToken),
        };

        self.advance();
        if self.current_token != Token::Semi && !is_first {
            return Err(ParseError::UnterminatedStatement);
        }

        Ok(statement)
    }

    /// Parses a single expression.
    ///
    /// This uses Pratt parsing as it is more efficient for expressions.
    fn parse_expression(&mut self, min_bp: u8) -> Result<Expression<'a>, ParseError> {
        let mut lhs = match &self.current_token {
            Token::Number(v) => Expression::Number(*v),
            Token::Minus | Token::Bang => self.parse_unary_expression()?,
            Token::OpenParen => self.parse_expression_in_paren()?,
            Token::Identifier(id) => self.parse_call_expression(id)?,
            _ => return Err(ParseError::UnexpectedToken),
        };

        loop {
            let op = match self.next_token.clone() {
                Token::Eof | Token::CloseParen | Token::Colon | Token::Comma => break,
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
                self.advance();
                // To next expression token.
                self.advance();

                lhs = if op == Token::Question {
                    self.parse_ternary_expression(rbp, lhs)?
                } else {
                    self.parse_binary_expression(rbp, lhs, op)?
                };
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    /// Parses a binary expression.
    fn parse_binary_expression(
        &mut self,
        rbp: u8,
        lhs: Expression<'a>,
        op: Token,
    ) -> Result<Expression<'a>, ParseError> {
        let rhs = self.parse_expression(rbp)?;

        Ok(Expression::new_binary(lhs, op.into(), rhs))
    }

    /// Parses a unary expression.
    fn parse_unary_expression(&mut self) -> Result<Expression<'a>, ParseError> {
        let op = self.current_token.clone();
        self.advance();
        let (_, rbp) = op.binding_power(true).unwrap();
        let rhs = self.parse_expression(rbp)?;
        Ok(Expression::new_unary(op.into(), rhs))
    }

    /// Parses a ternary expression.
    fn parse_ternary_expression(
        &mut self,
        rbp: u8,
        lhs: Expression<'a>,
    ) -> Result<Expression<'a>, ParseError> {
        let mhs = self.parse_expression(0)?;
        self.expect(Token::Colon)?;
        self.advance();
        let rhs = self.parse_expression(rbp)?;

        Ok(Expression::new_ternary(lhs, mhs, rhs))
    }

    /// Parses an an expression inside parenthesis.
    fn parse_expression_in_paren(&mut self) -> Result<Expression<'a>, ParseError> {
        self.advance();
        let lhs = self.parse_expression(0)?;
        self.expect(Token::CloseParen)?;
        Ok(lhs)
    }

    /// Parses a function call expression.
    fn parse_call_expression(&mut self, id: &'a str) -> Result<Expression<'a>, ParseError> {
        let mut arguments = Vec::new();

        self.expect(Token::OpenParen)?;

        loop {
            self.advance();
            arguments.push(self.parse_expression(0)?);
            self.advance();

            match &self.current_token {
                Token::Comma => continue,
                Token::CloseParen => break,
                _ => return Err(ParseError::UnexpectedToken),
            }
        }

        Ok(Expression::new_call(id, arguments))
    }

    /// Advances the current and next tokens.
    fn advance(&mut self) {
        self.current_token = std::mem::replace(
            &mut self.next_token,
            self.lexer.next().unwrap_or(Token::Eof),
        );
    }

    /// Similar to `self.advance` but used only once at the start to correctly
    /// offset the token stream.
    fn first_advance(&mut self) {
        self.next_token = self.lexer.next().unwrap_or(Token::Eof);
    }

    /// Advances the lexer consumnig the next token and checking whether it
    /// matches the given one or not.
    fn expect(&mut self, expected_token: Token) -> Result<(), ParseError> {
        self.advance();

        if self.current_token == expected_token {
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
        binary_addition,
        "1 + 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Add,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_subtraction,
        "1 - 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Subtract,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_multiplication,
        "1 * 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            Operator::Multiply,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_division,
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
        unary,
        "!-1",
        Ok(vec![Statement::Expression(Expression::new_unary(
            Operator::Negate,
            Expression::new_unary(Operator::Subtract, Expression::Number(1.0))
        ))])
    );

    text_parser!(
        ternary,
        "0 ? 1 : 2",
        Ok(vec![Statement::Expression(Expression::new_ternary(
            Expression::Number(0.0),
            Expression::Number(1.0),
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        function_call,
        "foo.bar(1, 2)",
        Ok(vec![Statement::Expression(Expression::new_call(
            "foo.bar",
            vec![Expression::Number(1.0), Expression::Number(2.0)]
        ))])
    );
}

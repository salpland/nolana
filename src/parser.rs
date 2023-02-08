//! Molang parser implementation.

use logos::{Lexer, Logos};
use miette::Diagnostic;
use std::ops::Range;
use thiserror::Error;

use crate::{ast::*, token::Token};

/// A Molang parser.
///
/// This parser implementation tries to be conformant to the most recent
/// Molang implementation. [Legacy behavior](https://bedrock.dev/docs/stable/Molang#Versioned%20Changes)
/// such as incorrect ternary operator associativity are not supported.
///
/// # Example
///
/// ```
/// use nolana::{
///     ast::{Expression, Statement},
///     parser::Parser,
/// };
///
/// let mut parser = Parser::new("math.pow(2, 3)");
/// let ast = parser.parse_program().expect("should parse");
///
/// assert_eq!(
///     ast,
///     vec![Statement::Expression(Expression::new_call(
///         "math.pow",
///         vec![
///             Expression::new_number(2.0),
///             Expression::new_number(3.0)
///         ]
///     ))]
/// );
/// ```
pub struct Parser<'src> {
    lexer: Lexer<'src, Token<'src>>,
    token: Token<'src>,
}

impl<'src> Parser<'src> {
    /// Creates a new Molang parser with provided source to parse.
    pub fn new(source: &'src str) -> Self {
        Self {
            lexer: Token::lexer(source),
            token: Token::Eof,
        }
    }

    /// Parses a program which consists of one or more statements.
    pub fn parse_program(&mut self) -> Result<Program<'src>, ParseError<'src>> {
        self.advance();

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
    fn parse_statement(&mut self, is_first: bool) -> Result<Statement<'src>, ParseError<'src>> {
        let statement = match &self.token {
            Token::Number(_)
            | Token::OpenParen
            | Token::Minus
            | Token::Bang
            | Token::Identifier(_) => Statement::Expression(self.parse_expression(0)?),
            Token::Eof => return Err(ParseError::UnexpectedEof(self.span())),
            _ => return Err(ParseError::UnexpectedToken(self.token.clone(), self.span())),
        };

        self.advance();
        if self.token != Token::Semi && !is_first {
            return Err(ParseError::UnterminatedStatement(self.span()));
        }

        Ok(statement)
    }

    /// Parses a single expression.
    ///
    /// This uses Pratt parsing as it is more efficient for expressions.
    fn parse_expression(&mut self, min_bp: u8) -> Result<Expression<'src>, ParseError<'src>> {
        let mut lhs = match &self.token {
            Token::Number(v) => Expression::Number(*v),
            Token::Minus | Token::Bang => self.parse_unary_expression()?,
            Token::OpenParen => self.parse_expression_in_paren()?,
            Token::Identifier(id) => self.parse_call_expression(id)?,
            _ => return Err(ParseError::UnexpectedToken(self.token.clone(), self.span())),
        };

        loop {
            let op = match self.lexer.clone().next().unwrap_or(Token::Eof) {
                // Expression terminators.
                Token::Eof
                | Token::CloseParen
                | Token::CloseBrace
                | Token::Colon
                | Token::Comma
                | Token::Semi => break,
                // Expression operators.
                token
                    if matches!(
                        token,
                        Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Question
                    ) =>
                {
                    token
                }
                token => return Err(ParseError::UnexpectedToken(token, self.span())),
            };

            if let Some((lbp, rbp)) = op.binding_power(false) {
                if lbp < min_bp {
                    break;
                }

                // To previously next operator token.
                self.advance();
                // To next expression.
                self.advance();

                lhs = match (&op, &self.token) {
                    (Token::Question, Token::OpenBrace) => {
                        self.parse_conditional_expression(lhs)?
                    }
                    (Token::Question, _) => self.parse_ternary_expression(rbp, lhs)?,
                    _ => self.parse_binary_expression(rbp, lhs, op)?,
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
        lhs: Expression<'src>,
        op: Token,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        let rhs = self.parse_expression(rbp)?;

        Ok(Expression::new_binary(lhs, op.into(), rhs))
    }

    /// Parses a unary expression.
    fn parse_unary_expression(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        let op = self.token.clone();
        self.advance();
        let (_, rbp) = op.binding_power(true).unwrap();
        let rhs = self.parse_expression(rbp)?;

        Ok(Expression::new_unary(op.into(), rhs))
    }

    /// Parses a ternary expression.
    fn parse_ternary_expression(
        &mut self,
        rbp: u8,
        lhs: Expression<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        let mhs = self.parse_expression(0)?;
        self.expect(Token::Colon)?;
        self.advance();
        let rhs = self.parse_expression(rbp)?;

        Ok(Expression::new_ternary(lhs, mhs, rhs))
    }

    /// Parses a list of statements inside a block.
    fn parse_conditional_expression(
        &mut self,
        lhs: Expression<'src>,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        let mut statements = Vec::new();

        self.advance();
        while self.token != Token::CloseBrace {
            statements.push(self.parse_statement(false)?);
            self.advance();
        }

        Ok(Expression::new_conditional(lhs, statements))
    }

    /// Parses an an expression inside parenthesis.
    fn parse_expression_in_paren(&mut self) -> Result<Expression<'src>, ParseError<'src>> {
        self.advance();
        let lhs = self.parse_expression(0)?;
        self.expect(Token::CloseParen)?;
        Ok(lhs)
    }

    /// Parses a function call expression.
    fn parse_call_expression(
        &mut self,
        id: &'src str,
    ) -> Result<Expression<'src>, ParseError<'src>> {
        let mut arguments = Vec::new();

        self.expect(Token::OpenParen)?;

        loop {
            self.advance();
            arguments.push(self.parse_expression(0)?);
            self.advance();

            match &self.token {
                Token::Comma => continue,
                Token::CloseParen => break,
                _ => return Err(ParseError::UnexpectedToken(self.token.clone(), self.span())),
            }
        }

        Ok(Expression::new_call(id, arguments))
    }

    /// Advances the current and next tokens.
    fn advance(&mut self) {
        self.token = self.lexer.next().unwrap_or(Token::Eof);
    }

    /// Returns the current token span.
    fn span(&self) -> Range<usize> {
        self.lexer.span()
    }

    /// Advances the lexer consuming the next token and checking whether it
    /// matches the given one or not.
    fn expect(&mut self, expected_token: Token<'src>) -> Result<(), ParseError<'src>> {
        self.advance();

        if self.token == expected_token {
            Ok(())
        } else {
            Err(ParseError::Expected(
                expected_token,
                self.token.clone(),
                self.span(),
            ))
        }
    }
}

/// Represents an error that occured during parsing.
#[derive(Debug, PartialEq, Error, Diagnostic)]
pub enum ParseError<'src> {
    /// A specific token was expected to be present but a different one was
    /// found.
    #[error("expected {0} but found {0}")]
    #[diagnostic(code(nolana::parser::ParseError::Expected), url(docsrs))]
    Expected(Token<'src>, Token<'src>, #[label("here")] Range<usize>),

    /// The token stream ended abruptly during parsing. This could because no
    /// more characters were found or unrecognized/invalid characters were
    /// found.
    #[error("unexpected end of input")]
    #[diagnostic(code(nolana::parser::ParseError::UnexpectedEof), url(docsrs))]
    UnexpectedEof(#[label("no more input here?")] Range<usize>),

    /// An unexpected token was found. This usually means that some important
    /// delimiter or character was misplaced or not present.
    #[error("unexpected {0}")]
    #[diagnostic(code(nolana::parser::ParseError::UnexpectedToken), url(docsrs))]
    UnexpectedToken(Token<'src>, #[label("here")] Range<usize>),

    /// A statement was unterminated, meaning that a `;` was missing.
    #[error("statement is unterminated")]
    #[diagnostic(help("try to append a semicolon"))]
    #[diagnostic(code(nolana::parser::ParseError::UnterminatedStatement), url(docsrs))]
    UnterminatedStatement(#[label("this statement")] Range<usize>),
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! text_parser {
        ($name:ident, $source:literal, $expected_output:expr) => {
            #[test]
            fn $name() {
                let mut parser = Parser::new($source);
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
            BinaryOperator::Add,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_subtraction,
        "1 - 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            BinaryOperator::Subtract,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_multiplication,
        "1 * 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            BinaryOperator::Multiply,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        binary_division,
        "1 / 2",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            BinaryOperator::Divide,
            Expression::Number(2.0)
        ))])
    );

    text_parser!(
        order_of_operations,
        "1 + 2 * 3",
        Ok(vec![Statement::Expression(Expression::new_binary(
            Expression::Number(1.0),
            BinaryOperator::Add,
            Expression::new_binary(
                Expression::Number(2.0),
                BinaryOperator::Multiply,
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
                BinaryOperator::Add,
                Expression::Number(2.0)
            ),
            BinaryOperator::Multiply,
            Expression::Number(3.0)
        ))])
    );

    text_parser!(
        unary,
        "!-1",
        Ok(vec![Statement::Expression(Expression::new_unary(
            UnaryOperator::Not,
            Expression::new_unary(UnaryOperator::Negate, Expression::Number(1.0))
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
        conditional,
        "0 ? {
            q.foo(1);
            v.bar(2);
        }",
        Ok(vec![Statement::Expression(Expression::new_conditional(
            Expression::Number(0.0),
            vec![
                Statement::Expression(Expression::new_call("q.foo", vec![Expression::Number(1.0)])),
                Statement::Expression(Expression::new_call("v.bar", vec![Expression::Number(2.0)]))
            ]
        ))])
    );

    text_parser!(
        function_call,
        "q.bar(1, 2)",
        Ok(vec![Statement::Expression(Expression::new_call(
            "q.bar",
            vec![Expression::Number(1.0), Expression::Number(2.0)]
        ))])
    );
}

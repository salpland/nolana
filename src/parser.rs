use logos::{Lexer, Logos};
use thiserror::Error;

use crate::{
    ast::{Expression, Program, Statement},
    token::Token,
};

/// A Molang parser.
///
/// This parser implementation tries to be conformant to the most recent
/// Molang implementation. [Legacy behavior](https://bedrock.dev/docs/stable/Molang#Versioned%20Changes)
/// such as incorrect ternary operator associativity are not supported.
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
    pub fn parse_program(&mut self) -> Result<Program<'src>, ParseError> {
        self.first_advance();
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
    fn parse_statement(&mut self, is_first: bool) -> Result<Statement<'src>, ParseError> {
        let statement = match &self.token {
            Token::Number(_)
            | Token::OpenParen
            | Token::Minus
            | Token::Bang
            | Token::Identifier(_) => Statement::Expression(self.parse_expression(0)?),
            Token::Eof => return Err(ParseError::UnexpectedEof),
            _ => return Err(ParseError::UnexpectedToken),
        };

        self.advance();
        if self.token != Token::Semi && !is_first {
            return Err(ParseError::UnterminatedStatement);
        }

        Ok(statement)
    }

    /// Parses a single expression.
    ///
    /// This uses Pratt parsing as it is more efficient for expressions.
    fn parse_expression(&mut self, min_bp: u8) -> Result<Expression<'src>, ParseError> {
        let mut lhs = match &self.token {
            Token::Number(v) => Expression::Number(*v),
            Token::Minus | Token::Bang => self.parse_unary_expression()?,
            Token::OpenParen => self.parse_expression_in_paren()?,
            Token::Identifier(id) => self.parse_call_expression(id)?,
            _ => return Err(ParseError::UnexpectedToken),
        };

        loop {
            let op = match &self.lexer.clone().next().unwrap_or(Token::Eof) {
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
                    token.clone()
                }
                _ => return Err(ParseError::UnexpectedToken),
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
    ) -> Result<Expression<'src>, ParseError> {
        let rhs = self.parse_expression(rbp)?;

        Ok(Expression::new_binary(lhs, op.into(), rhs))
    }

    /// Parses a unary expression.
    fn parse_unary_expression(&mut self) -> Result<Expression<'src>, ParseError> {
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
    ) -> Result<Expression<'src>, ParseError> {
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
    ) -> Result<Expression<'src>, ParseError> {
        let mut statements = Vec::new();

        self.advance();
        while self.token != Token::CloseBrace {
            statements.push(self.parse_statement(false)?);
            self.advance();
        }

        Ok(Expression::new_conditional(lhs, statements))
    }

    /// Parses an an expression inside parenthesis.
    fn parse_expression_in_paren(&mut self) -> Result<Expression<'src>, ParseError> {
        self.advance();
        let lhs = self.parse_expression(0)?;
        self.expect(Token::CloseParen)?;
        Ok(lhs)
    }

    /// Parses a function call expression.
    fn parse_call_expression(&mut self, id: &'src str) -> Result<Expression<'src>, ParseError> {
        let mut arguments = Vec::new();

        self.expect(Token::OpenParen)?;

        loop {
            self.advance();
            arguments.push(self.parse_expression(0)?);
            self.advance();

            match &self.token {
                Token::Comma => continue,
                Token::CloseParen => break,
                _ => return Err(ParseError::UnexpectedToken),
            }
        }

        Ok(Expression::new_call(id, arguments))
    }

    /// Advances the current and next tokens.
    fn advance(&mut self) {
        // self.current_token = std::mem::replace(
        //     &mut self.next_token,
        //     self.lexer.next().unwrap_or(Token::Eof),
        // );
        self.token = self.lexer.next().unwrap_or(Token::Eof);
    }

    /// Similar to `self.advance` but used only once at the start to correctly
    /// offset the token stream.
    fn first_advance(&mut self) {
        // self.next_token = self.lexer.next().unwrap_or(Token::Eof);
    }

    /// Advances the lexer consuming the next token and checking whether it
    /// matches the given one or not.
    fn expect(&mut self, expected_token: Token) -> Result<(), ParseError> {
        self.advance();

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

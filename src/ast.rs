//! Molang abstract syntax tree.
//!
//! Since there is no official Molang grammar specification, this implementation
//! is an attempt to make a Molang AST based on publicly available information
//! such as the [Molang documentation](https://bedrock.dev/docs/stable/Molang).

use crate::token::Token;

/// A program containing a list of statements.
pub type Program<'src> = Vec<Statement<'src>>;

/// A statement parse node.
///
/// Molang statements are mainly composed of control flow operations, such as a
/// loop or a return.
///
/// If a program contains more than one statement expressions, each
/// terminated with a semicolon, then it's considered a "complex expression".
/// This type of a program must end in a return statement, otherwise, the return
/// value is inferred as `0.0`.
///
/// Otherwise, the program is considered a "simple expression", meaning the
/// value of the statement expression is directly returned.
#[derive(Debug, PartialEq)]
pub enum Statement<'src> {
    /// See [`Expression`].
    Expression(Expression<'src>),
}

/// A statement block parse node.
///
/// It contains a list of statements.
///
/// Unlike a regular program statement list, every statement must be terminated
/// with a semicolon.
#[derive(Debug, PartialEq)]
pub struct StatementBlock<'src> {
    statements: Vec<Statement<'src>>,
}

/// An expression parse node.
#[derive(Debug, PartialEq)]
pub enum Expression<'src> {
    /// A floating-point number.
    Number(f64),

    /// A binary expression node.
    ///
    /// This expression requires two operands, one before the operator and one
    /// after it.
    ///
    /// Syntax: `x + y`.
    Binary {
        lhs: Box<Expression<'src>>,
        op: Operator,
        rhs: Box<Expression<'src>>,
    },

    /// A unary expression node.
    ///
    /// This expression is an operation with only one operand.
    ///
    /// Syntax: `-v.bar`.
    Unary {
        op: Operator,
        rhs: Box<Expression<'src>>,
    },

    /// A ternary expression node.
    ///
    /// This expression requires a condition and two operands, one for the
    /// truthy case and one for the falsy case.
    ///
    /// Syntax: `false ? 1 : 2`.
    Ternary {
        condition: Box<Expression<'src>>,
        if_true: Box<Expression<'src>>,
        if_false: Box<Expression<'src>>,
    },

    /// A conditional expression node.
    ///
    /// This expression will execute the block on the right if the condition on
    /// the left evaluates to `true`.
    ///
    /// Syntax: `q.foo ? { v.x = 2.0; }`.
    Conditional {
        condition: Box<Expression<'src>>,
        block: StatementBlock<'src>,
    },

    /// A function call expression node.
    ///
    /// This expression will call a built-in function with the given arguments.
    ///
    /// Syntax: `q.bar(1, 2)`.
    Call {
        id: &'src str,
        arguments: Vec<Expression<'src>>,
    },
}

impl<'src> Expression<'src> {
    /// Creates a number expression.
    pub fn new_number(value: f64) -> Self {
        Self::Number(value)
    }

    /// Creates a binary expression.
    pub fn new_binary(lhs: Expression<'src>, op: Operator, rhs: Expression<'src>) -> Self {
        Self::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    /// Creates a unary expression.
    pub fn new_unary(op: Operator, rhs: Expression<'src>) -> Self {
        Self::Unary {
            op,
            rhs: Box::new(rhs),
        }
    }

    /// Creates a ternary expression.
    pub fn new_ternary(
        condition: Expression<'src>,
        if_true: Expression<'src>,
        if_false: Expression<'src>,
    ) -> Self {
        Self::Ternary {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        }
    }

    /// Creates a conditional expression.
    pub fn new_conditional(condition: Expression<'src>, statements: Vec<Statement<'src>>) -> Self {
        Self::Conditional {
            condition: Box::new(condition),
            block: StatementBlock { statements },
        }
    }

    /// Creates a function call expression.
    pub fn new_call(id: &'src str, arguments: Vec<Expression<'src>>) -> Self {
        Self::Call { id, arguments }
    }
}

/// Any operator type.
#[derive(Debug, PartialEq)]
pub enum Operator {
    /// The addition operator produces the sum of two operands.
    ///
    /// Syntax: `x + y`.
    Add,

    /// # Binary
    ///
    /// The subtraction operator subtracts the two operands, producing their
    /// difference.
    ///
    /// Syntax: `x - y`.
    ///
    /// # Unary
    ///
    /// The unary negation operator precedes its operand and negates it.
    ///
    /// Syntax: `-x`.
    Subtract,

    /// The multiplication operator produces the product of the operands.
    ///
    /// Syntax: `x * y`.
    Multiply,

    /// The division operator produces the quotient of its operands where the
    /// left operand is the dividend and the right operand is the divisor.
    ///
    /// Syntax: `x / y`.
    Divide,

    /// Returns `false` if its single operand can be converted to `true`;
    /// otherwise, returns `true`.
    ///
    /// Syntax: `!x`.
    Negate,
}

impl<'src> From<Token<'src>> for Operator {
    fn from(token: Token<'src>) -> Self {
        match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Subtract,
            Token::Star => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::Bang => Self::Negate,
            _ => unreachable!(),
        }
    }
}

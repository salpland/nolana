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
    /// Syntax: `v.x + v.y`.
    Binary {
        lhs: Box<Expression<'src>>,
        op: BinaryOperator,
        rhs: Box<Expression<'src>>,
    },

    /// A unary expression node.
    ///
    /// This expression is an operation with only one operand.
    ///
    /// Syntax: `-v.foo`.
    Unary {
        op: UnaryOperator,
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
    pub fn new_binary(lhs: Expression<'src>, op: BinaryOperator, rhs: Expression<'src>) -> Self {
        Self::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    /// Creates a unary expression.
    pub fn new_unary(op: UnaryOperator, rhs: Expression<'src>) -> Self {
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

/// Represents a binary operation between two values.
#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    /// The addition operator produces the sum of two operands.
    ///
    /// Syntax: `v.x + v.y`.
    Add,

    /// The subtraction operator subtracts the two operands, producing their
    /// difference.
    ///
    /// Syntax: `v.x - v.y`.
    Subtract,

    /// The multiplication operator produces the product of the operands.
    ///
    /// Syntax: `v.x * v.y`.
    Multiply,

    /// The division operator produces the quotient of its operands where the
    /// left operand is the dividend and the right operand is the divisor.
    ///
    /// Syntax: `v.x / v.y`.
    Divide,

    /// The equality operator applies comparison between two values returning
    /// `1.0` or `0.0` based on that.
    ///
    /// Syntax: `v.x == v.y`.
    Equal,

    /// The inequality operator returns `1.0` if the operands are not equal.
    /// Otherwise, it returns `0.0`.
    ///
    /// Syntax: `v.x != v.y`.
    NotEqual,

    /// The greater than operator returns `1.0` if the left operand is greater
    /// than the right operand. Otherwise, it returns `0.0`.
    ///
    /// Syntax: `v.x > v.y`.
    GreaterThan,

    /// The greater than or equal operator returns `1.0` if the left operand is
    /// greater than or equal to the right operand. Otherwise, it returns
    /// `0.0`.
    ///
    /// Syntax: `v.x >= v.y`.
    GreaterThanOrEqual,

    /// The less than operator returns `1.0` if the left operand is less than
    /// the right operand. Otherwise, it returns `0.0`.
    ///
    /// Syntax: `v.x < v.y`.
    LessThan,

    /// The less than or equal operator returns `1.0` if the left operand is
    /// less than or equal to the right operand. Otherwise, it returns
    /// `0.0`.
    ///
    /// Syntax: `v.x <= v.y`.
    LessThanOrEqual,

    /// The logical AND operator returns the value of the first operand if it
    /// can be coerced into `1.0`; otherwise, it returns the second operand.
    ///
    /// Syntax: `v.x && v.y`.
    And,

    /// The logical OR operator returns the value the first operand if it can be
    /// coerced into `1.0`; otherwise, it returns the second operand.
    ///
    /// Syntax: `v.x || v.y`.
    Or,

    /// The nullish coalescing operator is a logical operator that returns the
    /// second operand when its first operand is a value that does not
    /// exist, and otherwise returns its first operand.
    ///
    /// Syntax: `v.x ?? v.y`.
    Coalesce,
}

impl<'src> From<Token<'src>> for BinaryOperator {
    fn from(token: Token<'src>) -> Self {
        match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Subtract,
            Token::Star => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::EqualEqual => Self::Equal,
            Token::BangEqual => Self::NotEqual,
            Token::GreaterThan => Self::GreaterThan,
            Token::GreaterThanEqual => Self::GreaterThanOrEqual,
            Token::LessThan => Self::LessThan,
            Token::LessThanEqual => Self::LessThanOrEqual,
            Token::AndAnd => Self::And,
            Token::BarBar => Self::Or,
            Token::QuestionQuestion => Self::Coalesce,
            _ => unreachable!(),
        }
    }
}

/// Represents a unary operation on a single value.
#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    /// The negation operator precedes its operand and negates it.
    ///
    /// Syntax: `-x`.
    Negate,

    /// The NOT operation returns `0.0` if its single operand can be
    /// converted to `1.0`; otherwise, returns `1.0`.
    ///
    /// Syntax: `!x`.
    Not,
}

impl<'src> From<Token<'src>> for UnaryOperator {
    fn from(token: Token<'src>) -> Self {
        match token {
            Token::Minus => Self::Negate,
            Token::Bang => Self::Not,
            _ => unreachable!(),
        }
    }
}

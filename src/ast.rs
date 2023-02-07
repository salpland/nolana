use crate::token::Token;

/// A program containing a list of statements.
pub type Program = Vec<Statement>;

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
pub enum Statement {
    /// See [`Expression`].
    Expression(Expression),
}

/// An expression parse node.
#[derive(Debug, PartialEq)]
pub enum Expression {
    /// A floating-point number.
    Number(f64),

    /// A binary expression node.
    ///
    /// This expression requires two operands, one before the operator and one
    /// after it.
    ///
    /// Syntax: `x [Operator] y`.
    Binary {
        lhs: Box<Expression>,
        op: Operator,
        rhs: Box<Expression>,
    },

    /// A unary expression node.
    ///
    /// This expression is an operation with only one operand.
    ///
    /// Syntax: `[Operator]x`.
    Unary { op: Operator, rhs: Box<Expression> },

    /// A ternary expression node.
    ///
    /// This expression requires a condition and two operands, one for the
    /// truthy case and one for the falsy case.
    ///
    /// Syntax: `x ? y : z`.
    Ternary {
        condition: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
}

impl Expression {
    /// Creates a number expression.
    pub fn new_number(value: f64) -> Self {
        Self::Number(value)
    }

    /// Creates a binary expression.
    pub fn new_binary(lhs: Expression, op: Operator, rhs: Expression) -> Self {
        Self::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    /// Creates a unary expression.
    pub fn new_unary(op: Operator, rhs: Expression) -> Self {
        Self::Unary {
            op,
            rhs: Box::new(rhs),
        }
    }

    /// Creates a ternary expression.
    pub fn new_ternary(condition: Expression, if_true: Expression, if_false: Expression) -> Self {
        Self::Ternary {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        }
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

impl<'a> From<Token<'a>> for Operator {
    fn from(token: Token<'a>) -> Self {
        match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Divide,
            Token::Star => Self::Multiply,
            Token::Slash => Self::Subtract,
            Token::Bang => Self::Negate,
            _ => unreachable!(),
        }
    }
}

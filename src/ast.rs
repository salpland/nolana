use crate::token::Token;

/// A program containing a list of statements.
pub type Program<'a> = Vec<Statement<'a>>;

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
pub enum Statement<'a> {
    /// See [`Expression`].
    Expression(Expression<'a>),
}

/// An expression parse node.
#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    /// A floating-point number.
    Number(f64),

    /// A binary expression node.
    ///
    /// This expression requires two operands, one before the operator and one
    /// after it.
    ///
    /// Syntax: `x [Operator] y`.
    Binary {
        lhs: Box<Expression<'a>>,
        op: Operator,
        rhs: Box<Expression<'a>>,
    },

    /// A unary expression node.
    ///
    /// This expression is an operation with only one operand.
    ///
    /// Syntax: `[Operator]x`.
    Unary {
        op: Operator,
        rhs: Box<Expression<'a>>,
    },

    /// A ternary expression node.
    ///
    /// This expression requires a condition and two operands, one for the
    /// truthy case and one for the falsy case.
    ///
    /// Syntax: `x ? y : z`.
    Ternary {
        condition: Box<Expression<'a>>,
        if_true: Box<Expression<'a>>,
        if_false: Box<Expression<'a>>,
    },

    /// A function call expression node.
    ///
    /// Syntax: `foo.bar([Expression], [Expression])`.
    Call {
        id: &'a str,
        arguments: Vec<Expression<'a>>,
    },
}

impl<'a> Expression<'a> {
    /// Creates a number expression.
    pub fn new_number(value: f64) -> Self {
        Self::Number(value)
    }

    /// Creates a binary expression.
    pub fn new_binary(lhs: Expression<'a>, op: Operator, rhs: Expression<'a>) -> Self {
        Self::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    /// Creates a unary expression.
    pub fn new_unary(op: Operator, rhs: Expression<'a>) -> Self {
        Self::Unary {
            op,
            rhs: Box::new(rhs),
        }
    }

    /// Creates a ternary expression.
    pub fn new_ternary(
        condition: Expression<'a>,
        if_true: Expression<'a>,
        if_false: Expression<'a>,
    ) -> Self {
        Self::Ternary {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        }
    }

    /// Creates a function call expression.
    pub fn new_call(id: &'a str, arguments: Vec<Expression<'a>>) -> Self {
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

impl<'a> From<Token<'a>> for Operator {
    fn from(token: Token<'a>) -> Self {
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

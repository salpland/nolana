use crate::token::Token;

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Statement>);

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Number(f64),
    Binary {
        lhs: Box<Expression>,
        op: Operator,
        rhs: Box<Expression>,
    },
    Unary {
        op: Operator,
        rhs: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
}

impl Expression {
    pub fn new_number(value: f64) -> Self {
        Self::Number(value)
    }

    pub fn new_binary(lhs: Expression, op: Operator, rhs: Expression) -> Self {
        Self::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    pub fn new_unary(op: Operator, rhs: Expression) -> Self {
        Self::Unary {
            op,
            rhs: Box::new(rhs),
        }
    }

    pub fn new_ternary(condition: Expression, if_true: Expression, if_false: Expression) -> Self {
        Self::Ternary {
            condition: Box::new(condition),
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
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

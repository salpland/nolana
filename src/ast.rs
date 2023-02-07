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
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Ternary {
        condition: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
}

impl<'a> From<Token<'a>> for Operator {
    fn from(token: Token<'a>) -> Self {
        match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Div,
            Token::Star => Self::Mul,
            Token::Slash => Self::Sub,
            Token::Bang => Self::Neg,
            _ => unreachable!(),
        }
    }
}

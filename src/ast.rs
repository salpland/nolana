pub enum Statement {
    Expression(Expression),
}

pub enum Expression {
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Number(f64),
}

impl Expression {
    /// Helper to wrap the expression into a box smart pointer.
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
}

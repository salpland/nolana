use nolana::{ast::*, parser::*};

macro_rules! test_parser {
    ($name:ident, $source:literal, $expected_output:expr) => {
        #[test]
        fn $name() {
            let mut parser = Parser::new($source);
            let actual_output = parser.parse_program();
            assert_eq!(actual_output, $expected_output);
        }
    };
}

test_parser!(
    binary_addition,
    "1 + 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::Add,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_subtraction,
    "1 - 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::Subtract,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_multiplication,
    "1 * 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::Multiply,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_division,
    "1 / 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::Divide,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_equality,
    "1 == 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::Equal,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_inequality,
    "1 != 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::NotEqual,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_greater_than,
    "1 > 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::GreaterThan,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_greater_than_or_equal,
    "1 >= 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::GreaterThanOrEqual,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_less_than,
    "1 < 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::LessThan,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_less_than_or_equal,
    "1 <= 2",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::LessThanOrEqual,
        Expression::Number(2.0)
    ))])
);

test_parser!(
    binary_and,
    "1 && 1",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::And,
        Expression::Number(1.0)
    ))])
);

test_parser!(
    binary_or,
    "1 || 1",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(1.0),
        BinaryOperator::Or,
        Expression::Number(1.0)
    ))])
);

test_parser!(
    binary_nullish_coalescing,
    "0 ?? 1",
    Ok(vec![Statement::Expression(Expression::new_binary(
        Expression::Number(0.0),
        BinaryOperator::Coalesce,
        Expression::Number(1.0)
    ))])
);

test_parser!(
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

test_parser!(
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

test_parser!(
    unary,
    "!-1",
    Ok(vec![Statement::Expression(Expression::new_unary(
        UnaryOperator::Not,
        Expression::new_unary(UnaryOperator::Negate, Expression::Number(1.0))
    ))])
);

test_parser!(
    ternary,
    "0 ? 1 : 2",
    Ok(vec![Statement::Expression(Expression::new_ternary(
        Expression::Number(0.0),
        Expression::Number(1.0),
        Expression::Number(2.0)
    ))])
);

test_parser!(
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

test_parser!(
    function_call,
    "q.bar(1, 2)",
    Ok(vec![Statement::Expression(Expression::new_call(
        "q.bar",
        vec![Expression::Number(1.0), Expression::Number(2.0)]
    ))])
);

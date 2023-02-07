use logos::Logos;

#[derive(Debug, PartialEq, Clone, Logos)]
pub enum Token<'a> {
    #[regex("[a-zA-Z]+[.][a-zA-Z_]+")]
    Identifier(&'a str),

    #[regex("'[^']*'")]
    Literal(&'a str),

    #[regex("([0-9]*[.])?[0-9]+", |lex| lex.slice().parse())]
    Number(f64),

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[token("[")]
    OpenBracket,

    #[token("]")]
    CloseBracket,

    #[token("!")]
    Bang,

    #[token("!=")]
    BangEqual,

    #[token("=")]
    Equal,

    #[token("==")]
    EqualEqual,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterThanEqual,

    #[token("<")]
    LessThan,

    #[token("<=")]
    LessThanEqual,

    #[token("||")]
    BarBar,

    #[token("&&")]
    AndAnd,

    #[token("->")]
    MinusGreaterThan,

    #[token("?")]
    Question,

    #[token("??")]
    QuestionQuestion,

    #[token(":")]
    Colon,

    #[token(";")]
    Semi,

    #[token(",")]
    Comma,

    #[token("-")]
    Minus,

    #[token("+")]
    Plus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("for_each")]
    ForEach,

    #[token("loop")]
    Loop,

    #[token("return")]
    Return,

    Eof,

    #[regex("#[^\n]*", logos::skip)]
    #[regex("[ \t\n\r]+", logos::skip)]
    #[error]
    Error,
}

impl<'a> Token<'a> {
    /// Returns the binding power of this token (precedence).
    pub fn binding_power(&self, is_unary: bool) -> Option<(u8, u8)> {
        Some(match self {
            Self::Minus if is_unary => (0, 9),
            Self::Bang if is_unary => (0, 8),
            Self::Star | Self::Slash => (6, 7),
            Self::Plus | Self::Minus => (4, 5),
            Self::Question => (3, 2),
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::*;

    macro_rules! text_lexer {
        ($name:ident, $input:literal, $expected_output:expr) => {
            #[test]
            fn $name() {
                let actual_output: Vec<_> = Token::lexer($input).spanned().collect();
                assert_eq!(actual_output, $expected_output);
            }
        };
    }

    text_lexer!(comments_and_whitespace, "\t\r# foo bar\n", vec![]);

    text_lexer!(
        numbers,
        "1 16 1.5 .25",
        vec![
            (Token::Number(1.0), 0..1),
            (Token::Number(16.0), 2..4),
            (Token::Number(1.5), 5..8),
            (Token::Number(0.25), 9..12)
        ]
    );

    text_lexer!(
        literals,
        "'foo:bar_baz' ''",
        vec![
            (Token::Literal("'foo:bar_baz'"), 0..13),
            (Token::Literal("''"), 14..16),
        ]
    );

    text_lexer!(
        identifiers,
        "foo.bar_baz Foo.Bar",
        vec![
            (Token::Identifier("foo.bar_baz"), 0..11),
            (Token::Identifier("Foo.Bar"), 12..19)
        ]
    );

    text_lexer!(
        keywords,
        "return continue break for_each loop true false",
        vec![
            (Token::Return, 0..6),
            (Token::Continue, 7..15),
            (Token::Break, 16..21),
            (Token::ForEach, 22..30),
            (Token::Loop, 31..35),
            (Token::True, 36..40),
            (Token::False, 41..46)
        ]
    );

    text_lexer!(
        symbols,
        "; : , -> ! = == != < > <= >= || && ?? + - * / () {} []",
        vec![
            (Token::Semi, 0..1),
            (Token::Colon, 2..3),
            (Token::Comma, 4..5),
            (Token::MinusGreaterThan, 6..8),
            (Token::Bang, 9..10),
            (Token::Equal, 11..12),
            (Token::EqualEqual, 13..15),
            (Token::BangEqual, 16..18),
            (Token::LessThan, 19..20),
            (Token::GreaterThan, 21..22),
            (Token::LessThanEqual, 23..25),
            (Token::GreaterThanEqual, 26..28),
            (Token::BarBar, 29..31),
            (Token::AndAnd, 32..34),
            (Token::QuestionQuestion, 35..37),
            (Token::Plus, 38..39),
            (Token::Minus, 40..41),
            (Token::Star, 42..43),
            (Token::Slash, 44..45),
            (Token::OpenParen, 46..47),
            (Token::CloseParen, 47..48),
            (Token::OpenBrace, 49..50),
            (Token::CloseBrace, 50..51),
            (Token::OpenBracket, 52..53),
            (Token::CloseBracket, 53..54),
        ]
    );

    text_lexer!(
        invalid_input,
        "1.23.45 foo 'bar",
        vec![
            (Token::Number(1.23), 0..4),
            (Token::Number(0.45), 4..7),
            (Token::Error, 8..11),
            (Token::Error, 12..16),
        ]
    );
}

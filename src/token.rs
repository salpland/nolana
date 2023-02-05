use logos::Logos;

#[derive(Logos)]
pub enum Token<'a> {
    #[regex("[a-zA-Z]+.[a-zA-Z_]+")]
    Identifier(&'a str),

    #[regex("'[^']*'")]
    String(&'a str),

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
    GreaterThanEqualual,

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

    #[regex("#[^\n]*", logos::skip)]
    #[regex("[ \t\n\r]+", logos::skip)]
    #[error]
    Error,
}

use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Error {
    UnknownSymbol,
    UnclosedQuote,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Number {
    BinInteger,
    HexInteger,
    DecInteger,
    DecFloating,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Mod,    // %

    Gt,     // >
    Ge,     // >=
    Lt,     // <
    Le,     // <=
    Ne,     // !=
    Eq,     // ==

    Or,     // |
    And,    // &
    Not,    // !
    Dot,    // .
    Rng,    // ..
    Err,    // ?
    Lam,    // ->

    Asg,    // =
    AddAsg, // +=
    SubAsg, // -=
    MulAsg, // *=
    DivAsg, // /=
    ModAsg, // %=
    AndAsg, // &=
    OrAsg,  // |=

    As,     // as
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    If,
    El,
    Ef,
    Match,
    For,
    In,
    While,
    Loop,
    Let,
    Var,
    Fun,
    Type,
    Struct,
    Enum,
    Trait,
    Use,
    Module,
    Pub,
    Void,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Delimiter {
    Comma,      // ,
    Colon,      // :
    Semicolon,  // ;
    RoundL,     // (
    RoundR,     // )
    SquareL,    // [
    SquareR,    // ]
    CurvedL,    // {
    CurvedR,    // }
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    Attribute,
    Identifier,
    Delimiter(Delimiter),
    Operator(Operator),
    Keyword(Keyword),
    Number(Number),
    String,
    Error(Error),
    DocComment,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub tktype: TokenType,
    pub source: Range<usize>,
}

pub struct Tokenizer<'a> {
    source   : &'a [u8],
    position : usize,
}
impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a [u8])-> Self {
        Self {
            source,
            position: 0,
        }
    }
    fn end(&self)-> bool {
        self.position >= self.source.len()
    }
    fn skip(&mut self, pred: impl Fn(u8)-> bool) {
        while self.source.get(self.position).is_some_and(|x| pred(*x)) {
            self.position += 1;
        }
    }
    fn skip_and_capture(&mut self, pred: impl Fn(u8)-> bool)-> &[u8] {
        let start = self.position;
        self.skip(pred);
        &self.source[start..self.position]
    }
    fn make_token(&mut self, f: impl FnOnce(&mut Self)-> TokenType)-> Token {
        let start = self.position;
        let token = f(self);
        Token { 
            tktype: token, 
            source: start..self.position 
        }
    }
    fn here(&self)-> u8 {
        self.source[self.position]
    }
    fn next(&self)-> Option<u8> {
        self.source.get(self.position + 1).copied()
    }

    fn word(&mut self)-> Option<Token> {
        if self.here().is_ascii() && !self.here().is_ascii_alphabetic() {
            return None;
        }
        Some(self.make_token(|this| {
            match this.skip_and_capture(|x| x.is_ascii_alphanumeric() || !x.is_ascii()) {
                b"if"     => TokenType::Keyword(Keyword::If),
                b"el"     => TokenType::Keyword(Keyword::El),
                b"ef"     => TokenType::Keyword(Keyword::Ef),
                b"match"  => TokenType::Keyword(Keyword::Match),
                b"for"    => TokenType::Keyword(Keyword::For),
                b"in"     => TokenType::Keyword(Keyword::In),
                b"while"  => TokenType::Keyword(Keyword::While),
                b"loop"   => TokenType::Keyword(Keyword::Loop),
                b"let"    => TokenType::Keyword(Keyword::Let),
                b"var"    => TokenType::Keyword(Keyword::Var),
                b"fun"    => TokenType::Keyword(Keyword::Fun),
                b"type"   => TokenType::Keyword(Keyword::Type),
                b"struct" => TokenType::Keyword(Keyword::Struct),
                b"enum"   => TokenType::Keyword(Keyword::Enum),
                b"trait"  => TokenType::Keyword(Keyword::Trait),
                b"use"    => TokenType::Keyword(Keyword::Use),
                b"module" => TokenType::Keyword(Keyword::Module),
                b"pub"    => TokenType::Keyword(Keyword::Pub),
                b"void"   => TokenType::Keyword(Keyword::Void),
                b"as"     => TokenType::Operator(Operator::As),
                _         => TokenType::Identifier,
            }
        }))
    }
    fn number(&mut self)-> Option<Token> {
        if !self.here().is_ascii_digit() {
            return None;
        }

        Some(self.make_token(|this| {
            if this.here() == b'0' && this.next().is_some_and(|x| x == b'x') {
                this.position += 2;
                this.skip(|x| x.is_ascii_hexdigit());
                TokenType::Number(Number::HexInteger)
            }
            else
            if this.here() == b'0' && this.next().is_some_and(|x| x == b'b') {
                this.position += 2;
                this.skip(|x| x == b'0' || x == b'1');
                TokenType::Number(Number::BinInteger)
            }
            else {
                this.skip(|x| x.is_ascii_digit());
                if this.here() == b'.' && this.next().is_some_and(|x| x.is_ascii_digit()) {
                    this.position += 2;
                    this.skip(|x| x.is_ascii_digit());
                    TokenType::Number(Number::DecFloating)
                }
                else {
                    TokenType::Number(Number::DecInteger)
                }
            }
        }))
    }
    fn string(&mut self)-> Option<Token> {
        if self.here() != b'"' {
            return None;
        }

        Some(self.make_token(|this| {
            this.position += 1; // skip opening quote
            loop {
                if this.end() {
                    return TokenType::Error(Error::UnclosedQuote);
                }
                match this.here() {
                    b'"'  => {
                        this.position += 1;
                        break;
                    },
                    b'\\' => this.position += 2,
                    _     => this.position += 1,
                }
            }
            TokenType::String
        }))
    }
    fn delimiter(&mut self)-> Option<Token> {
        match self.here() {
            b',' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::Comma) })),
            b':' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::Colon) })),
            b';' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::Semicolon) })),
            b'(' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::RoundL) })),
            b')' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::RoundR) })),
            b'[' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::SquareL) })),
            b']' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::SquareR) })),
            b'{' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::CurvedL) })),
            b'}' => Some(self.make_token(|t| { t.position += 1; TokenType::Delimiter(Delimiter::CurvedR) })),
            _    => None
        }
    }
    fn attribute(&mut self)-> Option<Token> {
        if self.here() != b'@' {
            return None;
        }
        Some(self.make_token(|this| {
            this.position += 1;
            this.skip(|x| x.is_ascii_alphanumeric() || !x.is_ascii());
            TokenType::Attribute
        }))
    }
    fn doccomment(&mut self)-> Option<Token> {
        if self.here() != b'/' && !self.next().is_some_and(|x| x == b'!') {
            return None;
        }
        Some(self.make_token(|this| {
            this.position += 2;
            this.skip(|x| x != b'\n');
            TokenType::DocComment
        }))
    }
    fn operator(&mut self)-> Option<Token> {
        match self.here() {
            b'+' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::AddAsg) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Add) }))
            }
            b'-' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::SubAsg) })),
                Some(b'>') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::Lam) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Sub) }))
            }
            b'*' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::MulAsg) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Mul) }))
            }
            b'/' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::DivAsg) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Div) }))
            }
            b'%' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::ModAsg) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Mod) }))
            }
            b'|' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::OrAsg) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Or) }))
            }
            b'&' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::AndAsg) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::And) }))
            }
            b'!' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::Ne) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Not) }))
            }
            b'>' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::Ge) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Gt) }))
            }
            b'<' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::Le) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Lt) }))
            }
            b'=' => match self.next() {
                Some(b'=') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::Eq) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Asg) }))
            }
            b'.' => match self.next() {
                Some(b'.') => Some(self.make_token(|t| { t.position += 2; TokenType::Operator(Operator::Rng) })),
                _          => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Dot) }))
            }
            b'?' => Some(self.make_token(|t| { t.position += 1; TokenType::Operator(Operator::Err) })),
            _    => None,
        }
    }
    fn error(&mut self)-> Token {
        self.make_token(|this| {
            this.position += 1;
            this.skip(|x| !x.is_ascii());
            TokenType::Error(Error::UnknownSymbol)
        })
    }
    fn skip_ignored(&mut self) {
        loop {
            self.skip(|x| x.is_ascii_whitespace());
            if !self.end() && self.here() == b'/' && self.next().is_some_and(|x| x == b'/') {
                self.skip(|x| x != b'\n');
            }
            else {
                break;
            }
        }
    }
    pub fn tokenize(&mut self)-> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            self.skip_ignored();
            if self.end() {
                break;
            }
            tokens.push(
                            self.word()
                .or_else(|| self.number())
                .or_else(|| self.string())
                .or_else(|| self.operator())
                .or_else(|| self.attribute())
                .or_else(|| self.delimiter())
                .or_else(|| self.doccomment())
                .unwrap_or_else(|| self.error())
            );
        }
        tokens
    }
}
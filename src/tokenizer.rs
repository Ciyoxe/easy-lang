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
    Add,
    Sub,
    Mul,
    Div,
    AddAsg,
    SubAsg,
    MulAsg,
    DivAsg,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    For,
    If,
    El,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    Identifier,
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
                b"for" => TokenType::Keyword(Keyword::For),
                b"if"  => TokenType::Keyword(Keyword::If),
                b"el"  => TokenType::Keyword(Keyword::El),
                _ => TokenType::Identifier,
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
            _ => None,
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
                .or_else(|| self.doccomment())
                .unwrap_or_else(|| self.error())
            );
        }
        tokens
    }
}
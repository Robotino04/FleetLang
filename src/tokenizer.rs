#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub type_: TokenType,
    pub start: SourceLocation,
    pub end: SourceLocation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Keyword(Keyword),
    Identifier(String),
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Semicolon,
    Dot,
    EqualSign,
    SingleRightArrow,
    Number(i64),

    UnknownCharacters(String),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    On,
    Self_,
    Let,
    I32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceLocation {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    fn start() -> SourceLocation {
        SourceLocation {
            index: 0,
            line: 1,
            column: 0,
        }
    }
}

pub struct Tokenizer {
    chars: Vec<char>,
    current_location: SourceLocation,

    tokens: Vec<Token>,

    unk_char_accumulator: String,
    unk_char_token: Token,
}

impl Tokenizer {
    pub fn new(src: String) -> Tokenizer {
        Tokenizer {
            chars: src.chars().collect::<Vec<_>>(),
            current_location: SourceLocation::start(),

            tokens: vec![],

            unk_char_accumulator: "".to_string(),
            unk_char_token: Token {
                type_: TokenType::UnknownCharacters("".to_string()),
                start: SourceLocation::start(),
                end: SourceLocation::start(),
            },
        }
    }

    fn advance(&mut self) {
        match self.chars[self.current_location.index] {
            '\n' => {
                self.current_location.index += 1;
                self.current_location.column = 0;
                self.current_location.line += 1;
            }
            _ => {
                self.current_location.index += 1;
                self.current_location.column += 1;
            }
        }
    }

    fn unknown_character(&mut self, c: char) {
        if self.unk_char_accumulator.is_empty() {
            self.unk_char_token.start = self.current_location;
            self.advance();
            self.unk_char_token.end = self.current_location;
            self.unk_char_accumulator = c.to_string();
        } else if self.unk_char_token.end.index == self.current_location.index {
            self.unk_char_accumulator
                .push(self.chars[self.current_location.index]);

            self.advance();
            self.unk_char_token.end = self.current_location;
        } else {
            self.unk_char_token.type_ =
                TokenType::UnknownCharacters(self.unk_char_accumulator.clone());
            self.tokens.push(self.unk_char_token.clone());

            self.unk_char_token.start = self.current_location;
            self.advance();
            self.unk_char_token.end = self.current_location;
            self.unk_char_accumulator = c.to_string();
        }
        eprintln!("Unexpected character {:?}", c);
    }
    fn single_char_token(&mut self, t: TokenType) -> Token {
        let token = Token {
            start: self.current_location,
            end: self.current_location,
            type_: t,
        };
        self.advance();
        return token;
    }

    pub fn tokenize(&mut self) -> Result<&Vec<Token>, ()> {
        while self.current_location.index < self.chars.len() {
            match self.chars[self.current_location.index] {
                '(' => {
                    let tok = self.single_char_token(TokenType::OpenParen);
                    self.tokens.push(tok);
                }
                ')' => {
                    let tok = self.single_char_token(TokenType::CloseParen);
                    self.tokens.push(tok);
                }
                '{' => {
                    let tok = self.single_char_token(TokenType::OpenBrace);
                    self.tokens.push(tok);
                }
                '}' => {
                    let tok = self.single_char_token(TokenType::CloseBrace);
                    self.tokens.push(tok);
                }
                '[' => {
                    let tok = self.single_char_token(TokenType::OpenBracket);
                    self.tokens.push(tok);
                }
                ']' => {
                    let tok = self.single_char_token(TokenType::CloseBracket);
                    self.tokens.push(tok);
                }

                '.' => {
                    let tok = self.single_char_token(TokenType::Dot);
                    self.tokens.push(tok);
                }
                ';' => {
                    let tok = self.single_char_token(TokenType::Semicolon);
                    self.tokens.push(tok);
                }
                '=' => {
                    let tok = self.single_char_token(TokenType::EqualSign);
                    self.tokens.push(tok);
                }

                '-' => {
                    let start = self.current_location;
                    match self.chars.get(self.current_location.index + 1) {
                        Some('>') => {
                            self.advance();
                            self.advance();

                            self.tokens.push(Token {
                                type_: TokenType::SingleRightArrow,
                                start,
                                end: self.current_location,
                            });
                        }
                        None => {
                            eprintln!("Hit EOF while tokenizing '-'");
                            break;
                        }
                        Some(c) => {
                            self.unknown_character(*c);
                        }
                    }
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    let start_location = self.current_location;

                    while let 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' =
                        self.chars[self.current_location.index]
                    {
                        self.advance();

                        if self.current_location.index >= self.chars.len() {
                            break;
                        }
                    }

                    let lexeme = &self.chars[start_location.index..self.current_location.index]
                        .iter()
                        .collect::<String>();

                    self.tokens.push(Token {
                        start: start_location,
                        end: self.current_location,
                        type_: match lexeme.as_str() {
                            "on" => TokenType::Keyword(Keyword::On),
                            "self" => TokenType::Keyword(Keyword::Self_),
                            "let" => TokenType::Keyword(Keyword::Let),
                            "i32" => TokenType::Keyword(Keyword::I32),
                            _ => TokenType::Identifier(lexeme.to_string()),
                        },
                    })
                }

                '0'..='9' => {
                    let start_location = self.current_location;

                    while let '0'..='9' = self.chars[self.current_location.index] {
                        self.advance();

                        if self.current_location.index >= self.chars.len() {
                            break;
                        }
                    }

                    let lexeme = &self.chars[start_location.index..self.current_location.index]
                        .iter()
                        .collect::<String>();

                    self.tokens.push(Token {
                        start: start_location,
                        end: self.current_location,
                        type_: TokenType::Number(lexeme.parse().unwrap_or_else(|_| {
                            eprintln!("Unable to parse {:?} as a number", lexeme);
                            return 0;
                        })),
                    })
                }

                ' ' | '\t' | '\n' => {
                    self.advance();
                }

                c => {
                    self.unknown_character(c);
                }
            }
        }

        // flush the last unknown character
        if !self.unk_char_accumulator.is_empty() {
            self.unk_char_token.type_ =
                TokenType::UnknownCharacters(self.unk_char_accumulator.clone());
            self.tokens.push(self.unk_char_token.clone());
        }

        return Ok(&self.tokens);
    }
}

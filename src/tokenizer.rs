use crate::infra::FleetError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub type_: TokenType,
    pub start: SourceLocation,
    pub end: SourceLocation,

    // https://langdev.stackexchange.com/questions/2289/preserving-comments-in-ast
    pub leading_trivia: Vec<Trivia>,
    pub trailing_trivia: Vec<Trivia>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriviaKind {
    LineComment(String),
    BlockComment(String),
    EmptyLine,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Trivia {
    pub kind: TriviaKind,
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

    ExclamationMark,
    Tilde,
    Minus,

    Plus,
    Star,
    Slash,
    Percent,

    UnknownCharacters(String),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    On,
    Self_,
    Let,
    I32,
    Return,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceLocation {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Ord for SourceLocation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        return self.index.cmp(&other.index);
    }
}
impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl SourceLocation {
    pub fn start() -> SourceLocation {
        SourceLocation {
            index: 0,
            line: 1,
            column: 0,
        }
    }

    pub fn end(src: impl AsRef<str>) -> SourceLocation {
        SourceLocation {
            index: src.as_ref().len() - 1,
            line: src.as_ref().chars().filter(|c| *c == '\n').count() + 1,
            column: src.as_ref().split("\n").last().unwrap_or("").len(),
        }
    }
}

pub struct Tokenizer {
    chars: Vec<char>,
    current_location: SourceLocation,

    tokens: Vec<Token>,
    errors: Vec<FleetError>,

    unk_char_accumulator: String,
    unk_char_token: Token,

    trivia_accumulator: Vec<Trivia>,
}

impl Tokenizer {
    pub fn new(src: String) -> Tokenizer {
        Tokenizer {
            chars: src.chars().collect::<Vec<_>>(),
            current_location: SourceLocation::start(),

            tokens: vec![],
            errors: vec![],

            unk_char_accumulator: "".to_string(),
            unk_char_token: Token {
                type_: TokenType::UnknownCharacters("".to_string()),
                start: SourceLocation::start(),
                end: SourceLocation::start(),

                leading_trivia: vec![],
                trailing_trivia: vec![],
            },

            trivia_accumulator: vec![],
        }
    }

    pub fn errors(&self) -> &Vec<FleetError> {
        &self.errors
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
            self.errors.push(FleetError {
                start: self.unk_char_token.start,
                end: self.unk_char_token.end,
                message: "Unrecognized characters".to_string(),
            });

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

            leading_trivia: self.trivia_accumulator.clone(),
            trailing_trivia: vec![],
        };
        self.trivia_accumulator.clear();
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
                '!' => {
                    let tok = self.single_char_token(TokenType::ExclamationMark);
                    self.tokens.push(tok);
                }
                '~' => {
                    let tok = self.single_char_token(TokenType::Tilde);
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

                                leading_trivia: self.trivia_accumulator.clone(),
                                trailing_trivia: vec![],
                            });
                            self.trivia_accumulator.clear();
                        }
                        _ => {
                            let tok = self.single_char_token(TokenType::Minus);
                            self.tokens.push(tok);
                        }
                    }
                }

                '+' => {
                    let tok = self.single_char_token(TokenType::Plus);
                    self.tokens.push(tok);
                }
                '*' => {
                    let tok = self.single_char_token(TokenType::Star);
                    self.tokens.push(tok);
                }
                '/' => {
                    match self.chars.get(self.current_location.index + 1) {
                        Some('/') => {
                            let start_location = self.current_location;
                            self.advance();
                            self.advance();
                            let content_start_location = self.current_location;

                            while self.chars[self.current_location.index] != '\n' {
                                self.advance();

                                if self.current_location.index >= self.chars.len() {
                                    break;
                                }
                            }

                            let content_end_location = self.current_location;
                            self.advance(); // \n
                            let end_location = self.current_location;

                            let content = &self.chars
                                [content_start_location.index..content_end_location.index]
                                .iter()
                                .collect::<String>();

                            self.trivia_accumulator.push(Trivia {
                                kind: TriviaKind::LineComment(content.clone()),
                                start: start_location,
                                end: end_location,
                            });
                            self.flush_trailing_trivia();
                        }
                        Some('*') => {
                            let start_location = self.current_location;
                            self.advance();
                            self.advance();
                            let content_start_location = self.current_location;

                            while self.current_location.index + 1 < self.chars.len()
                                && self.chars
                                    [self.current_location.index..self.current_location.index + 2]
                                    != ['*', '/']
                            {
                                self.advance();
                            }
                            let content_end_location = self.current_location;

                            if self.current_location.index + 1 >= self.chars.len() {
                                self.errors.push(FleetError {
                                    start: start_location,
                                    end: content_end_location,
                                    message: "Unclosed block comment".to_string(),
                                });
                            } else {
                                self.advance(); // *
                                self.advance(); // /
                            }
                            let end_location = self.current_location;

                            let content = &self.chars
                                [content_start_location.index..content_end_location.index]
                                .iter()
                                .collect::<String>();

                            self.trivia_accumulator.push(Trivia {
                                kind: TriviaKind::BlockComment(content.clone()),
                                start: start_location,
                                end: end_location,
                            });
                            self.flush_trailing_trivia();
                        }
                        _ => {
                            let tok = self.single_char_token(TokenType::Slash);
                            self.tokens.push(tok);
                        }
                    }
                }
                '%' => {
                    let tok = self.single_char_token(TokenType::Percent);
                    self.tokens.push(tok);
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
                            "return" => TokenType::Keyword(Keyword::Return),

                            "i32" => TokenType::Keyword(Keyword::I32),
                            _ => TokenType::Identifier(lexeme.to_string()),
                        },

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
                    });
                    self.trivia_accumulator.clear();
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

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
                    });
                    self.trivia_accumulator.clear();
                }

                ' ' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    let start = self.current_location;
                    self.flush_trailing_trivia();
                    self.advance();
                    self.trivia_accumulator.push(Trivia {
                        kind: TriviaKind::EmptyLine,
                        start,
                        end: self.current_location,
                    });
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
            self.errors.push(FleetError {
                start: self.unk_char_token.start,
                end: self.unk_char_token.end,
                message: "Unrecognized characters".to_string(),
            });
        }

        if let Some(token) = self.tokens.last_mut() {
            token
                .trailing_trivia
                .extend(self.trivia_accumulator.clone());
            self.trivia_accumulator.clear();
        }

        return Ok(&self.tokens);
    }
    fn flush_trailing_trivia(&mut self) {
        if let (Some(last_token), Some(trivia)) =
            (self.tokens.last_mut(), self.trivia_accumulator.last())
        {
            if last_token.end.line == trivia.end.line
                || (last_token.end.line + 1 == trivia.end.line && trivia.end.column == 0)
            {
                eprintln!("Flushing {:?} to {:#?}", trivia, last_token);
                last_token
                    .trailing_trivia
                    .extend(self.trivia_accumulator.clone());
                self.trivia_accumulator.clear();
            }
        }
    }
}

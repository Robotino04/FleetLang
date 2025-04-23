pub struct Tokenizer {
    src: String,
    current_location: SourceLocation,
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
    Number(i64),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    On,
    Self_,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceLocation {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub type_: TokenType,
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl Tokenizer {
    pub fn new(src: String) -> Tokenizer {
        Tokenizer {
            src,
            current_location: SourceLocation {
                index: 0,
                line: 1,
                column: 0,
            },
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, ()> {
        let mut tokens = vec![];
        let chars = self.src.chars().collect::<Vec<_>>();

        let single_char_token = |current_location: &mut SourceLocation, t: TokenType| {
            let token = Token {
                start: *current_location,
                end: *current_location,
                type_: t,
            };
            current_location.column += 1;
            current_location.index += 1;
            return token;
        };

        while self.current_location.index < chars.len() {
            match chars[self.current_location.index] {
                '(' => tokens.push(single_char_token(
                    &mut self.current_location,
                    TokenType::OpenParen,
                )),
                ')' => tokens.push(single_char_token(
                    &mut self.current_location,
                    TokenType::CloseParen,
                )),
                '{' => tokens.push(single_char_token(
                    &mut self.current_location,
                    TokenType::OpenBrace,
                )),
                '}' => tokens.push(single_char_token(
                    &mut self.current_location,
                    TokenType::CloseBrace,
                )),
                '[' => tokens.push(single_char_token(
                    &mut self.current_location,
                    TokenType::OpenBracket,
                )),
                ']' => tokens.push(single_char_token(
                    &mut self.current_location,
                    TokenType::CloseBracket,
                )),

                '.' => tokens.push(single_char_token(&mut self.current_location, TokenType::Dot)),
                ';' => tokens.push(single_char_token(
                    &mut self.current_location,
                    TokenType::Semicolon,
                )),

                'a'..='z' | 'A'..='Z' | '_' => {
                    let start_location = self.current_location;

                    while let 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' =
                        chars[self.current_location.index]
                    {
                        self.current_location.index += 1;
                        self.current_location.column += 1;

                        if self.current_location.index >= chars.len() {
                            break;
                        }
                    }

                    let lexeme = &chars[start_location.index..self.current_location.index]
                        .iter()
                        .collect::<String>();

                    tokens.push(Token {
                        start: start_location,
                        end: self.current_location,
                        type_: match lexeme.as_str() {
                            "on" => TokenType::Keyword(Keyword::On),
                            "self" => TokenType::Keyword(Keyword::Self_),
                            _ => TokenType::Identifier(lexeme.to_string()),
                        },
                    })
                }

                '0'..='9' => {
                    let start_location = self.current_location;

                    while let '0'..='9' = chars[self.current_location.index] {
                        self.current_location.index += 1;
                        self.current_location.column += 1;

                        if self.current_location.index >= chars.len() {
                            break;
                        }
                    }

                    let lexeme = &chars[start_location.index..self.current_location.index]
                        .iter()
                        .collect::<String>();

                    tokens.push(Token {
                        start: start_location,
                        end: self.current_location,
                        type_: TokenType::Number(lexeme.parse().unwrap_or_else(|_| {
                            eprintln!("Unable to parse {:?} as a number", lexeme);
                            return 0;
                        })),
                    })
                }

                ' ' | '\t' => {
                    self.current_location.index += 1;
                    self.current_location.column += 1;
                }
                '\n' => {
                    self.current_location.index += 1;
                    self.current_location.column = 0;
                    self.current_location.line += 1;
                }

                c => {
                    eprintln!("Unexpected character {:?}", c);
                    self.current_location.index += 1;
                    self.current_location.column += 1;
                }
            }
        }

        return Ok(tokens);
    }
}

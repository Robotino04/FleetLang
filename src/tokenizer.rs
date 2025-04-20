pub struct Tokenizer {
    src: String,
    index: usize,
}

#[derive(Clone, Debug)]
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

#[derive(Copy, Clone, Debug)]
pub enum Keyword {
    On,
    Self_,
}

#[derive(Copy, Clone, Debug)]
pub struct SourceLocation {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub start: SourceLocation,
    pub end: SourceLocation,
    pub type_: TokenType,
}

impl Tokenizer {
    pub fn new(src: String) -> Tokenizer {
        Tokenizer { src, index: 0 }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, ()> {
        let mut tokens = vec![];
        let chars = self.src.chars().collect::<Vec<_>>();
        let mut current_location = SourceLocation {
            index: 0,
            line: 1,
            column: 0,
        };

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

        while current_location.index < chars.len() {
            match chars[current_location.index] {
                '(' => tokens.push(single_char_token(
                    &mut current_location,
                    TokenType::OpenParen,
                )),
                ')' => tokens.push(single_char_token(
                    &mut current_location,
                    TokenType::CloseParen,
                )),
                '{' => tokens.push(single_char_token(
                    &mut current_location,
                    TokenType::OpenBrace,
                )),
                '}' => tokens.push(single_char_token(
                    &mut current_location,
                    TokenType::CloseBrace,
                )),
                '[' => tokens.push(single_char_token(
                    &mut current_location,
                    TokenType::OpenBracket,
                )),
                ']' => tokens.push(single_char_token(
                    &mut current_location,
                    TokenType::CloseBracket,
                )),

                '.' => tokens.push(single_char_token(&mut current_location, TokenType::Dot)),
                ';' => tokens.push(single_char_token(
                    &mut current_location,
                    TokenType::Semicolon,
                )),

                'a'..='z' | 'A'..='Z' | '_' => {
                    let start_location = current_location;

                    while let 'a'..='z' | 'A'..='Z' | '_' | '0'..='9' =
                        chars[current_location.index]
                    {
                        current_location.index += 1;
                        current_location.column += 1;

                        if current_location.index >= chars.len() {
                            break;
                        }
                    }

                    let lexeme = &chars[start_location.index..current_location.index]
                        .iter()
                        .collect::<String>();

                    tokens.push(Token {
                        start: start_location,
                        end: current_location,
                        type_: match lexeme.as_str() {
                            "on" => TokenType::Keyword(Keyword::On),
                            "self" => TokenType::Keyword(Keyword::Self_),
                            _ => TokenType::Identifier(lexeme.to_string()),
                        },
                    })
                }

                '0'..='9' => {
                    let start_location = current_location;

                    while let '0'..='9' = chars[current_location.index] {
                        current_location.index += 1;
                        current_location.column += 1;

                        if current_location.index >= chars.len() {
                            break;
                        }
                    }

                    let lexeme = &chars[start_location.index..current_location.index]
                        .iter()
                        .collect::<String>();

                    tokens.push(Token {
                        start: start_location,
                        end: current_location,
                        type_: TokenType::Number(lexeme.parse().unwrap_or_else(|_| {
                            eprintln!("Unable to parse {:?} as a number", lexeme);
                            return 0;
                        })),
                    })
                }

                ' ' | '\t' => {
                    current_location.index += 1;
                    current_location.column += 1;
                }
                '\n' => {
                    current_location.index += 1;
                    current_location.column = 0;
                    current_location.line += 1;
                }

                c => {
                    eprintln!("Unexpected character {:?}", c);
                    current_location.index += 1;
                    current_location.column += 1;
                }
            }
        }

        return Ok(tokens);
    }
}

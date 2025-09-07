use std::{cell::RefMut, cmp::Ordering, rc::Rc};

use log::{error, info};

use crate::{
    NewtypeDeref,
    infra::{ErrorSeverity, FleetError},
    passes::pass_manager::{Errors, GlobalState, InputSource, Pass, PassFactory, PassResult},
};

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub type_: TokenType,
    pub range: SourceRange,

    // https://langdev.stackexchange.com/questions/2289/preserving-comments-in-ast
    pub leading_trivia: Vec<Trivia>,
    pub trailing_trivia: Vec<Trivia>,

    pub file_name: FileName,
}

NewtypeDeref!(pub FileName, Rc<String>, Clone, PartialEq, Eq);

impl From<String> for FileName {
    fn from(value: String) -> Self {
        Self(Rc::new(value))
    }
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
    pub range: SourceRange,
}

#[derive(Clone, Debug, PartialEq)]
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
    Comma,
    Colon,
    Dot,
    At,
    EqualSign,
    SingleRightArrow,

    Integer(u64, String),
    Float(f64, String),
    StringLiteral(String),

    ExclamationMark,
    Tilde,
    Minus,

    Plus,
    Star,
    Slash,
    Percent,

    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    DoubleEqual,
    NotEqual,
    DoubleAmpersand,
    DoublePipe,

    UnknownCharacters(String),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
    On,
    Self_,

    Let,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Idk,
    As,

    True,
    False,

    Return,
    If,
    Elif,
    Else,

    While,
    For,
    Break,
    Skip,

    Extern,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceLocation {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl Ord for SourceLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Less => Ordering::Less,
            Ordering::Equal => self.column.cmp(&other.column),
            Ordering::Greater => Ordering::Greater,
        }
    }
}
impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
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
            column: src.as_ref().split("\n").last().unwrap_or("").len() + 1,
        }
    }

    pub fn until(self, other: SourceLocation) -> SourceRange {
        SourceRange {
            start: self,
            end: other,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceRange {
    /// inclusive
    pub start: SourceLocation,
    /// non-inclusive
    pub end: SourceLocation,
}

impl SourceRange {
    pub fn contains(&self, other: SourceLocation) -> bool {
        self.start <= other && other < self.end
    }

    /// Constructs the smallest [SourceRange] that includes both `self` and `other`
    pub fn extend_with(self, other: SourceRange) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.start.min(other.end),
        }
    }

    pub fn num_chars(&self) -> usize {
        self.end.index - self.start.index
    }
}

pub struct Tokenizer<'state> {
    errors: RefMut<'state, Errors>,
    chars: Vec<char>,
    file_name: FileName,

    tokens: RefMut<'state, Vec<Token>>,

    current_location: SourceLocation,

    unk_char_accumulator: String,
    unk_char_range: SourceRange,

    trivia_accumulator: Vec<Trivia>,
}

impl PassFactory for Tokenizer<'_> {
    type Output<'state> = Tokenizer<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        let source = state.check_named::<InputSource>()?;
        let file_name = state.get_named::<InputSource>()?.file_name.clone();

        let errors = state.check_named()?;

        let tokens = state.insert(vec![]);

        Ok(Self::Output {
            errors: errors.get_mut(state),
            tokens: tokens.get_mut(state),

            chars: source.get(state).source.chars().collect(),
            file_name,

            current_location: SourceLocation::start(),

            unk_char_accumulator: "".to_string(),
            unk_char_range: SourceLocation::start().until(SourceLocation::start()),

            trivia_accumulator: vec![],
        })
    }
}
impl Pass for Tokenizer<'_> {
    fn run<'state>(self: Box<Self>) -> PassResult {
        self.tokenize();

        Ok(())
    }
}

impl<'errors> Tokenizer<'errors> {
    fn done(&self) -> bool {
        self.current_location.index >= self.chars.len()
    }

    fn advance(&mut self) {
        if self.done() {
            error!("Tried advancing past EOF");
            return;
        }

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
            self.unk_char_range.start = self.current_location;
            self.advance();
            self.unk_char_range.end = self.current_location;
            self.unk_char_accumulator = c.to_string();
        } else if self.unk_char_range.end.index == self.current_location.index {
            self.unk_char_accumulator
                .push(self.chars[self.current_location.index]);

            self.advance();
            self.unk_char_range.end = self.current_location;
        } else {
            self.tokens.push(Token {
                type_: TokenType::UnknownCharacters(self.unk_char_accumulator.clone()),
                range: self.unk_char_range,
                leading_trivia: vec![],
                trailing_trivia: vec![],
                file_name: self.file_name.clone(),
            });

            self.unk_char_range.start = self.current_location;
            self.advance();
            self.unk_char_range.end = self.current_location;
            self.unk_char_accumulator = c.to_string();
        }
        error!("Unexpected character {c:?}");
    }
    fn single_char_token(&mut self, t: TokenType) -> Token {
        self.multi_char_token(1, t)
    }
    fn multi_char_token(&mut self, n: usize, t: TokenType) -> Token {
        let start = self.current_location;
        for _ in 0..n {
            self.advance();
        }
        let token = Token {
            range: start.until(self.current_location),
            type_: t,

            leading_trivia: self.trivia_accumulator.clone(),
            trailing_trivia: vec![],
            file_name: self.file_name.clone(),
        };
        self.trivia_accumulator.clear();
        token
    }

    pub fn tokenize(mut self) {
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
                '@' => {
                    let tok = self.single_char_token(TokenType::At);
                    self.tokens.push(tok);
                }
                ';' => {
                    let tok = self.single_char_token(TokenType::Semicolon);
                    self.tokens.push(tok);
                }
                ',' => {
                    let tok = self.single_char_token(TokenType::Comma);
                    self.tokens.push(tok);
                }
                ':' => {
                    let tok = self.single_char_token(TokenType::Colon);
                    self.tokens.push(tok);
                }
                '=' => match self.chars.get(self.current_location.index + 1) {
                    Some('=') => {
                        let tok = self.multi_char_token(2, TokenType::DoubleEqual);
                        self.tokens.push(tok);
                    }
                    _ => {
                        let tok = self.single_char_token(TokenType::EqualSign);
                        self.tokens.push(tok);
                    }
                },
                '!' => match self.chars.get(self.current_location.index + 1) {
                    Some('=') => {
                        let tok = self.multi_char_token(2, TokenType::NotEqual);
                        self.tokens.push(tok);
                    }
                    _ => {
                        let tok = self.single_char_token(TokenType::ExclamationMark);
                        self.tokens.push(tok);
                    }
                },
                '~' => {
                    let tok = self.single_char_token(TokenType::Tilde);
                    self.tokens.push(tok);
                }

                '-' => match self.chars.get(self.current_location.index + 1) {
                    Some('>') => {
                        let tok = self.multi_char_token(2, TokenType::SingleRightArrow);
                        self.tokens.push(tok);
                    }
                    _ => {
                        let tok = self.single_char_token(TokenType::Minus);
                        self.tokens.push(tok);
                    }
                },

                '+' => {
                    let tok = self.single_char_token(TokenType::Plus);
                    self.tokens.push(tok);
                }
                '*' => {
                    let tok = self.single_char_token(TokenType::Star);
                    self.tokens.push(tok);
                }
                '<' => match self.chars.get(self.current_location.index + 1) {
                    Some('=') => {
                        let tok = self.multi_char_token(2, TokenType::LessThanEqual);
                        self.tokens.push(tok);
                    }
                    _ => {
                        let tok = self.single_char_token(TokenType::LessThan);
                        self.tokens.push(tok);
                    }
                },
                '>' => match self.chars.get(self.current_location.index + 1) {
                    Some('=') => {
                        let tok = self.multi_char_token(2, TokenType::GreaterThanEqual);
                        self.tokens.push(tok);
                    }
                    _ => {
                        let tok = self.single_char_token(TokenType::GreaterThan);
                        self.tokens.push(tok);
                    }
                },
                '|' => match self.chars.get(self.current_location.index + 1) {
                    Some('|') => {
                        let tok = self.multi_char_token(2, TokenType::DoublePipe);
                        self.tokens.push(tok);
                    }
                    Some(c) => {
                        self.unknown_character(*c);
                    }
                    None => {}
                },
                '&' => match self.chars.get(self.current_location.index + 1) {
                    Some('&') => {
                        let tok = self.multi_char_token(2, TokenType::DoubleAmpersand);
                        self.tokens.push(tok);
                    }
                    Some(c) => {
                        self.unknown_character(*c);
                    }
                    None => {}
                },
                '/' => {
                    match self.chars.get(self.current_location.index + 1) {
                        Some('/') => {
                            let start_location = self.current_location;
                            self.advance();
                            self.advance();
                            let content_start_location = self.current_location;

                            while self.chars[self.current_location.index] != '\n' && !self.done() {
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
                                range: start_location.until(end_location),
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
                                self.errors.push(FleetError::from_range(
                                    start_location.until(content_end_location),
                                    "Unclosed block comment".to_string(),
                                    ErrorSeverity::Error,
                                    self.file_name.clone(),
                                ));
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
                                range: start_location.until(end_location),
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
                        range: start_location.until(self.current_location),
                        type_: match lexeme.as_str() {
                            "on" => TokenType::Keyword(Keyword::On),
                            "self" => TokenType::Keyword(Keyword::Self_),

                            "let" => TokenType::Keyword(Keyword::Let),
                            "return" => TokenType::Keyword(Keyword::Return),
                            "if" => TokenType::Keyword(Keyword::If),
                            "elif" => TokenType::Keyword(Keyword::Elif),
                            "else" => TokenType::Keyword(Keyword::Else),

                            "while" => TokenType::Keyword(Keyword::While),
                            "for" => TokenType::Keyword(Keyword::For),
                            "break" => TokenType::Keyword(Keyword::Break),
                            "skip" => TokenType::Keyword(Keyword::Skip),

                            "extern" => TokenType::Keyword(Keyword::Extern),

                            "i8" => TokenType::Keyword(Keyword::I8),
                            "i16" => TokenType::Keyword(Keyword::I16),
                            "i32" => TokenType::Keyword(Keyword::I32),
                            "i64" => TokenType::Keyword(Keyword::I64),
                            "f32" => TokenType::Keyword(Keyword::F32),
                            "f64" => TokenType::Keyword(Keyword::F64),
                            "bool" => TokenType::Keyword(Keyword::Bool),
                            "idk" => TokenType::Keyword(Keyword::Idk),
                            "as" => TokenType::Keyword(Keyword::As),

                            "true" => TokenType::Keyword(Keyword::True),
                            "false" => TokenType::Keyword(Keyword::False),
                            _ => TokenType::Identifier(lexeme.to_string()),
                        },

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
                        file_name: self.file_name.clone(),
                    });
                    self.trivia_accumulator.clear();
                }

                '0'..='9' => {
                    let mut is_float = false;

                    let start_location = self.current_location;

                    while let '0'..='9' = self.chars[self.current_location.index] {
                        self.advance();

                        if self.current_location.index >= self.chars.len() {
                            break;
                        }
                    }
                    if '.' == self.chars[self.current_location.index] {
                        is_float = true;
                        self.advance();
                        while let '0'..='9' = self.chars[self.current_location.index] {
                            self.advance();

                            if self.current_location.index >= self.chars.len() {
                                break;
                            }
                        }
                    }

                    let lexeme = &self.chars[start_location.index..self.current_location.index]
                        .iter()
                        .collect::<String>();

                    self.tokens.push(Token {
                        range: start_location.until(self.current_location),
                        type_: if is_float {
                            TokenType::Float(
                                lexeme.parse().unwrap_or_else(|_| {
                                    self.errors.push(FleetError::from_range(
                                        start_location.until(self.current_location),
                                        format!("Unable to parse {lexeme:?} as a float"),
                                        ErrorSeverity::Error,
                                        self.file_name.clone(),
                                    ));

                                    0.0
                                }),
                                lexeme.clone(),
                            )
                        } else {
                            TokenType::Integer(
                                lexeme.parse().unwrap_or_else(|_| {
                                    self.errors.push(FleetError::from_range(
                                        start_location.until(self.current_location),
                                        format!("Unable to parse {lexeme:?} as a float"),
                                        ErrorSeverity::Error,
                                        self.file_name.clone(),
                                    ));
                                    0
                                }),
                                lexeme.clone(),
                            )
                        },

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
                        file_name: self.file_name.clone(),
                    });
                    self.trivia_accumulator.clear();
                }
                '"' => {
                    let start_location = self.current_location;

                    self.advance(); // "
                    while !matches!(self.chars[self.current_location.index], '"') && !self.done() {
                        self.advance();

                        if self.current_location.index >= self.chars.len() {
                            break;
                        }
                    }
                    self.advance(); // "

                    let lexeme = self.chars
                        [start_location.index + 1..self.current_location.index - 1]
                        .iter()
                        .collect::<String>();

                    self.tokens.push(Token {
                        range: start_location.until(self.current_location),
                        type_: TokenType::StringLiteral(lexeme),

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
                        file_name: self.file_name.clone(),
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
                        range: start.until(self.current_location),
                    });
                }

                c => {
                    self.unknown_character(c);
                }
            }
        }

        // flush the last unknown character

        if !self.unk_char_accumulator.is_empty() {
            self.tokens.push(Token {
                type_: TokenType::UnknownCharacters(self.unk_char_accumulator.clone()),
                range: self.unk_char_range,
                leading_trivia: vec![],
                trailing_trivia: vec![],
                file_name: self.file_name.clone(),
            });
        }

        if let Some(token) = self.tokens.last_mut() {
            token
                .trailing_trivia
                .extend(self.trivia_accumulator.clone());
            self.trivia_accumulator.clear();
        }

        for token in self.tokens.iter() {
            if let Token {
                type_: TokenType::UnknownCharacters(_),
                ..
            } = token
            {
                self.errors.push(FleetError::from_token(
                    token,
                    "Unrecognized characters",
                    ErrorSeverity::Error,
                ));
            }
        }
    }
    fn flush_trailing_trivia(&mut self) {
        let (Some(last_token), Some(trivia)) =
            (self.tokens.last_mut(), self.trivia_accumulator.last())
        else {
            return;
        };

        let same_line = last_token.range.end.line == trivia.range.end.line;
        let trailing_with_newline =
            last_token.range.end.line + 1 == trivia.range.end.line && trivia.range.end.column == 0;

        if same_line || trailing_with_newline {
            info!("Flushing {trivia:?} to {last_token:#?}");
            last_token
                .trailing_trivia
                .append(&mut self.trivia_accumulator);
        }
    }
}

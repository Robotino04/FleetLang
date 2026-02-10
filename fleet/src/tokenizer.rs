use std::{cell::RefMut, cmp::Ordering, fmt::Debug, ops::Index, rc::Rc};

use itertools::Itertools;
use log::{error, info};

use crate::{
    NewtypeDeref,
    error_reporting::{ErrorKind, Errors},
    escape::{QuoteType, unescape},
    passes::pass_manager::{GlobalState, InputSource, Pass, PassFactory, PassResult},
};

#[derive(Clone, PartialEq, Debug)]
pub struct Token {
    pub type_: TokenType,
    pub range: NamedSourceRange,

    // https://langdev.stackexchange.com/questions/2289/preserving-comments-in-ast
    pub leading_trivia: Vec<Trivia>,
    pub trailing_trivia: Vec<Trivia>,
}

/*
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Token { .. }")
    }
}
*/

NewtypeDeref!(pub FileName, Rc<String>, Clone, PartialEq, Eq, Hash);

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
    CharLiteral(String),

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
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Idk,
    As,
    Struct,

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
    pub fn named(self, name: FileName) -> NamedSourceLocation {
        NamedSourceLocation { name, loc: self }
    }

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

    pub fn until(self, other: impl Into<SourceLocation>) -> SourceRange {
        SourceRange {
            start: self,
            end: other.into(),
        }
    }
    pub fn until_named(self, other: impl Into<NamedSourceLocation>) -> NamedSourceRange {
        let other = other.into();
        self.until(other.loc).named(other.name)
    }
    pub fn prev_inline(self) -> SourceLocation {
        SourceLocation {
            index: self.index - 1,
            line: self.line,
            column: self.column.saturating_sub(1),
        }
    }
    pub fn offset(
        mut self,
        mut offset: usize,
        src: &impl Index<usize, Output = char>,
    ) -> SourceLocation {
        while offset > 0 {
            self.index += 1;
            self.column += 1;
            offset -= 1;
            if *src.index(self.index) == '\n' {
                self.column = 0;
                self.line += 1;
            }
        }
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NamedSourceLocation {
    pub loc: SourceLocation,
    pub name: FileName,
}
impl Ord for NamedSourceLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.loc.cmp(&other.loc) {
            x @ (Ordering::Less | Ordering::Greater) => x,
            Ordering::Equal => self.name.cmp(&other.name),
        }
    }
}
impl PartialOrd for NamedSourceLocation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl From<NamedSourceLocation> for SourceLocation {
    fn from(value: NamedSourceLocation) -> Self {
        value.loc
    }
}

impl NamedSourceLocation {
    pub fn index(&self) -> usize {
        self.loc.index
    }
    pub fn line(&self) -> usize {
        self.loc.line
    }
    pub fn column(&self) -> usize {
        self.loc.column
    }

    pub fn until(self, other: impl Into<SourceLocation>) -> NamedSourceRange {
        self.loc.until(other).named(self.name)
    }
    pub fn prev_inline(self) -> NamedSourceLocation {
        self.loc.prev_inline().named(self.name)
    }
    pub fn offset(
        self,
        offset: usize,
        src: &impl Index<usize, Output = char>,
    ) -> NamedSourceLocation {
        self.loc.offset(offset, src).named(self.name)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceRange {
    /// inclusive
    pub start: SourceLocation,
    /// non-inclusive
    pub end: SourceLocation,
}

impl SourceRange {
    pub fn empty_start() -> Self {
        SourceLocation::start().until(SourceLocation::start())
    }

    pub fn named(self, name: FileName) -> NamedSourceRange {
        NamedSourceRange { name, range: self }
    }

    pub fn contains(&self, other: SourceLocation) -> bool {
        self.start <= other && other < self.end
    }

    /// Constructs the smallest [SourceRange] that includes both `self` and `other`
    pub fn extend_with(self, other: impl Into<SourceRange>) -> Self {
        let other = other.into();
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn maybe_extend(self, other: Option<impl Into<SourceRange>>) -> Self {
        match other {
            Some(other) => self.extend_with(other),
            None => self,
        }
    }

    pub fn intersects(&self, other: impl Into<SourceRange>) -> bool {
        let other = other.into();
        self.start < other.end && other.start < self.end
    }

    pub fn num_chars(&self) -> usize {
        self.end.index - self.start.index
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NamedSourceRange {
    pub name: FileName,
    pub range: SourceRange,
}

impl From<NamedSourceRange> for SourceRange {
    fn from(value: NamedSourceRange) -> Self {
        value.range
    }
}

impl NamedSourceRange {
    pub fn split(&self) -> (NamedSourceLocation, NamedSourceLocation) {
        (self.start(), self.end())
    }

    pub fn start(&self) -> NamedSourceLocation {
        self.range.start.named(self.name.clone())
    }
    pub fn end(&self) -> NamedSourceLocation {
        self.range.end.named(self.name.clone())
    }

    pub fn contains(&self, other: SourceLocation) -> bool {
        self.range.contains(other)
    }

    /// Constructs the smallest [NamedSourceRange] that includes both `self` and `other`
    pub fn extend_with(self, other: impl Into<SourceRange>) -> Self {
        self.range.extend_with(other).named(self.name)
    }

    pub fn maybe_extend(self, other: Option<impl Into<SourceRange>>) -> Self {
        match other {
            Some(other) => self.extend_with(other),
            None => self,
        }
    }

    pub fn intersects(&self, other: impl Into<SourceRange>) -> bool {
        self.range.intersects(other)
    }

    pub fn num_chars(&self) -> usize {
        self.range.num_chars()
    }
}

pub struct Tokenizer<'state> {
    errors: RefMut<'state, Errors>,
    chars: Vec<char>,
    file_name: FileName,

    tokens: RefMut<'state, Vec<Token>>,

    current_location: SourceLocation,

    unk_char_accumulator: String,
    unk_char_range: NamedSourceRange,

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
            file_name: file_name.clone(),

            current_location: SourceLocation::start(),

            unk_char_accumulator: "".to_string(),
            unk_char_range: SourceRange::empty_start().named(file_name),

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
            self.unk_char_range.range.start = self.current_location;
            self.advance();
            self.unk_char_range.range.end = self.current_location;
            self.unk_char_accumulator = c.to_string();
        } else if self.unk_char_range.end().index() == self.current_location.index {
            self.unk_char_accumulator
                .push(self.chars[self.current_location.index]);

            self.advance();
            self.unk_char_range.range.end = self.current_location;
        } else {
            self.tokens.push(Token {
                type_: TokenType::UnknownCharacters(self.unk_char_accumulator.clone()),
                range: self.unk_char_range.clone(),
                leading_trivia: vec![],
                trailing_trivia: vec![],
            });

            self.unk_char_range.range.start = self.current_location;
            self.advance();
            self.unk_char_range.range.end = self.current_location;
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
            range: start
                .until(self.current_location)
                .named(self.file_name.clone()),
            type_: t,

            leading_trivia: self.trivia_accumulator.clone(),
            trailing_trivia: vec![],
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
                            self.advance(); // /
                            let start_marker_range = start_location.until(self.current_location);
                            self.advance(); // *
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
                                self.errors.push(ErrorKind::UnclosedBlockComment {
                                    start_token: start_marker_range.named(self.file_name.clone()),
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
                        range: start_location
                            .until(self.current_location)
                            .named(self.file_name.clone()),
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
                            "u8" => TokenType::Keyword(Keyword::U8),
                            "u16" => TokenType::Keyword(Keyword::U16),
                            "u32" => TokenType::Keyword(Keyword::U32),
                            "u64" => TokenType::Keyword(Keyword::U64),
                            "f32" => TokenType::Keyword(Keyword::F32),
                            "f64" => TokenType::Keyword(Keyword::F64),
                            "bool" => TokenType::Keyword(Keyword::Bool),
                            "idk" => TokenType::Keyword(Keyword::Idk),
                            "as" => TokenType::Keyword(Keyword::As),
                            "struct" => TokenType::Keyword(Keyword::Struct),

                            "true" => TokenType::Keyword(Keyword::True),
                            "false" => TokenType::Keyword(Keyword::False),
                            _ => TokenType::Identifier(lexeme.to_string()),
                        },

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
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
                        range: start_location
                            .until(self.current_location)
                            .named(self.file_name.clone()),
                        type_: if is_float {
                            TokenType::Float(
                                lexeme.parse().unwrap_or_else(|_| {
                                    self.errors.push(ErrorKind::InvalidFloatLexeme {
                                        range: start_location
                                            .until(self.current_location)
                                            .named(self.file_name.clone()),
                                        lexeme: lexeme.to_string(),
                                    });

                                    0.0
                                }),
                                lexeme.clone(),
                            )
                        } else {
                            TokenType::Integer(
                                lexeme.parse().unwrap_or_else(|_| {
                                    self.errors.push(ErrorKind::InvalidIntLexeme {
                                        range: start_location
                                            .until(self.current_location)
                                            .named(self.file_name.clone()),
                                        lexeme: lexeme.to_string(),
                                    });
                                    0
                                }),
                                lexeme.clone(),
                            )
                        },

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
                    });
                    self.trivia_accumulator.clear();
                }
                '"' => {
                    let start_location = self.current_location;

                    self.advance(); // "
                    let mut is_escape = false;
                    while (!matches!(self.chars[self.current_location.index], '"') || is_escape)
                        && !self.done()
                    {
                        is_escape = self.chars[self.current_location.index] == '\\';

                        self.advance();

                        if self.current_location.index >= self.chars.len() {
                            break;
                        }
                    }
                    self.advance(); // "

                    let string_range = start_location.until(self.current_location);

                    let raw_lexeme = self.chars
                        [start_location.index + 1..self.current_location.index - 1]
                        .iter()
                        .collect::<String>();

                    let (lexeme, unknown_escape_sequences) =
                        unescape(&raw_lexeme, QuoteType::Double)
                            .map(|ok| (ok, vec![]))
                            .unwrap_or_else(|err| (raw_lexeme.clone(), err));

                    let range = start_location.until(self.current_location);

                    for esc_error_offset in unknown_escape_sequences {
                        let start = range.start.offset(esc_error_offset + 1, &self.chars);
                        let end = start.offset(2, &self.chars);
                        self.errors.push(ErrorKind::InvalidEscapeSequence {
                            string_range: string_range.named(self.file_name.clone()),
                            escape_range: start.until(end).named(self.file_name.clone()),
                            escape_sequence: raw_lexeme
                                .chars()
                                .skip(esc_error_offset)
                                .take(2)
                                .join(""),
                        });
                    }

                    self.tokens.push(Token {
                        range: range.named(self.file_name.clone()),
                        type_: TokenType::StringLiteral(lexeme),

                        leading_trivia: self.trivia_accumulator.clone(),
                        trailing_trivia: vec![],
                    });
                    self.trivia_accumulator.clear();
                }
                '\'' => {
                    let start_location = self.current_location;

                    self.advance(); // '
                    let mut is_escape = false;
                    while (!matches!(self.chars[self.current_location.index], '\'') || is_escape)
                        && !self.done()
                    {
                        is_escape = self.chars[self.current_location.index] == '\\';

                        self.advance();

                        if self.current_location.index >= self.chars.len() {
                            break;
                        }
                    }
                    self.advance(); // '

                    let string_range = start_location.until(self.current_location);

                    let raw_lexeme = self.chars
                        [start_location.index + 1..self.current_location.index - 1]
                        .iter()
                        .collect::<String>();

                    let (lexeme, unknown_escape_sequences) =
                        unescape(&raw_lexeme, QuoteType::Single)
                            .map(|ok| (ok, vec![]))
                            .unwrap_or_else(|err| (raw_lexeme.clone(), err));

                    let range = start_location.until(self.current_location);

                    for esc_error_offset in unknown_escape_sequences {
                        let start = range.start.offset(esc_error_offset + 1, &self.chars);
                        let end = start.offset(2, &self.chars);
                        self.errors.push(ErrorKind::InvalidEscapeSequence {
                            string_range: string_range.named(self.file_name.clone()),
                            escape_range: start.until(end).named(self.file_name.clone()),
                            escape_sequence: raw_lexeme
                                .chars()
                                .skip(esc_error_offset)
                                .take(2)
                                .join(""),
                        });
                    }

                    if lexeme.len() != 1 {
                        self.errors.push(ErrorKind::CharacterLiteralTooBig {
                            range: range.named(self.file_name.clone()),
                            actual_length: lexeme.len(),
                        });
                    }

                    self.tokens.push(Token {
                        range: range.named(self.file_name.clone()),
                        type_: TokenType::CharLiteral(lexeme),

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
                type_: TokenType::UnknownCharacters(chars),
                range,
                ..
            } = token
            {
                self.errors.push(ErrorKind::InvalidCharacters {
                    range: range.clone(),
                    characters: chars.to_string(),
                });
            }
        }
    }
    fn flush_trailing_trivia(&mut self) {
        let (Some(last_token), Some(trivia)) =
            (self.tokens.last_mut(), self.trivia_accumulator.last())
        else {
            return;
        };

        let same_line = last_token.range.end().line() == trivia.range.end.line;
        let trailing_with_newline = last_token.range.end().line() + 1 == trivia.range.end.line
            && trivia.range.end.column == 0;

        if same_line || trailing_with_newline {
            info!("Flushing {trivia:?} to {last_token:#?}");
            last_token
                .trailing_trivia
                .append(&mut self.trivia_accumulator);
        }
    }
}

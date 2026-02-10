use std::fmt::Display;

use itertools::Itertools;

use crate::{
    NewtypeDeref,
    ast::{BinaryOperation, UnaryOperation},
    ast_to_dm::AstToDocumentModelConverter,
    document_model::{DocumentElement, stringify_document},
    generate_c::CCodeGenerator,
    generate_glsl::GLSLCodeGenerator,
    parser::Parser,
    passes::{
        err_missing_type_in_parameter::ErrMissingTypeInParam,
        err_too_few_iterators::ErrTooFewIterators,
        fix_non_block_statements::FixNonBlockStatements,
        fix_trailing_comma::FixTrailingComma,
        lvalue_reducer::LValueReducer,
        pass_manager::{Errors, InputSource, PassError, PassManager, TypeSets},
        remove_parens::RemoveParensPass,
        runtime_type::{ConcreteRuntimeType, RuntimeType, RuntimeTypeKind},
        scope_analysis::ScopeAnalyzer,
        stat_tracker::StatTracker,
        store_pass::StorePass,
        type_concretisation_pass::TypeConcretisationPass,
        type_propagation::TypePropagator,
        union_find_set::UnionFindSetPtr,
    },
    tokenizer::{NamedSourceLocation, NamedSourceRange, SourceRange, Token, Tokenizer},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum ErrorSeverity {
    Note,
    Warning,
    Error,
}

#[derive(Clone, Debug)]
pub enum Lint {
    // "Unnecessary parentheses"
    UnnecessaryParentheses {
        opening: NamedSourceRange,
        closing: NamedSourceRange,
    },
    // "This code is unreachable"
    CodeUnreachable {
        range: NamedSourceRange,
    },
    // Casting {} to itself is redundant
    // Casting array of type {} to array of type {} is redundant because the element types are equal
    SelfCast {
        expression: NamedSourceRange,
        type_: PrefetchedType,
    },
    ToIdkCast {
        expression: NamedSourceRange,
        from_type: PrefetchedType,
        to_type: PrefetchedType,
    },
}

#[derive(Clone, Debug)]
pub struct PrefetchedType {
    pub definition_range: Option<NamedSourceRange>,
    pub kind: RuntimeTypeKind,
    pub str: String,
}

impl Display for PrefetchedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.str)
    }
}

impl PrefetchedType {
    pub fn fetch(ptr: UnionFindSetPtr<RuntimeType>, type_sets: &TypeSets) -> Self {
        let RuntimeType {
            kind,
            definition_range,
        } = type_sets.get(ptr).clone();
        let str = kind.stringify(type_sets);

        Self {
            definition_range,
            kind,
            str,
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnresolvedSymbol {
    pub name: String,
    pub use_range: NamedSourceRange,
}

impl Display for UnresolvedSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl UnresolvedSymbol {
    pub fn new(name: String, range: NamedSourceRange) -> Self {
        Self {
            name,
            use_range: range,
        }
    }
    pub fn from_token(name: String, token: &Token) -> Self {
        Self {
            name,
            use_range: token.range.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolDefinition {
    pub name: String,
    pub definition: NamedSourceRange,
}

impl Display for SymbolDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl SymbolDefinition {
    pub fn new(name: String, range: NamedSourceRange) -> Self {
        Self {
            name,
            definition: range,
        }
    }
    pub fn from_token(name: String, token: &Token) -> Self {
        Self {
            name,
            definition: token.range.clone(),
        }
    }

    pub fn with_use(self, use_range: NamedSourceRange) -> SymbolUse {
        let SymbolDefinition {
            name,
            definition: range,
        } = self;

        SymbolUse {
            name,
            use_range,
            definition: range,
        }
    }
}

#[derive(Clone, Debug)]
pub struct SymbolUse {
    pub name: String,
    pub use_range: NamedSourceRange,
    pub definition: NamedSourceRange,
}

impl Display for SymbolUse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    // Unclosed block comment
    UnclosedBlockComment {
        // the /* part of the comment, not really a token but who cares
        start_token: NamedSourceRange,
    },
    InvalidFloatLexeme {
        range: NamedSourceRange,
        lexeme: String,
    },
    InvalidIntLexeme {
        range: NamedSourceRange,
        lexeme: String,
    },
    // Unknown escape sequence {}
    InvalidEscapeSequence {
        string_range: NamedSourceRange,
        escape_range: NamedSourceRange,
        escape_sequence: String,
    },
    // Character literals may only contain a single character
    CharacterLiteralTooBig {
        range: NamedSourceRange,
        actual_length: usize,
    },
    // Unrecognized characters
    InvalidCharacters {
        range: NamedSourceRange,
        characters: String,
    },

    // recovering from error until one of []
    ParserRecovery {
        range: NamedSourceRange,
        stop_conditions: Vec<String>,
    },
    // Expected {}, but found {:?}
    ParserExpectationFailed {
        expectation: String,
        found_token: Token,
    },
    // Expected {}, but found End of File
    ParserExpectationEOF {
        expectation: String,
        last_token: Token,
    },
    // Unable to parse an expected {}
    ParserFailure {
        thing_to_parse: String,
        start_token: Token,
    },
    // Hit EOF while parsing an expected {}
    ParserFailureEOF {
        thing_to_parse: String,
        start_token: Token,
    },

    // Function `{}` doesn't have a return type yet
    FunctionMissingReturnType {
        function: SymbolDefinition,
    },
    // Function `{}` doesn't have parameter types yet
    FunctionMissingParameterTypes {
        function: SymbolDefinition,
    },
    // Variable `{}` doesn't have a type yet
    // The type of {name:?} cannot be inferred completely. Best effort: {}
    IncompleteTypeInferenceVariable {
        variable: SymbolDefinition,
        best_guess: Option<PrefetchedType>,
    },

    // Struct doesn't have member named {member_name:?}
    // Struct doesn't have member named {member_name:?}
    NonexistentStructMember {
        struct_type: PrefetchedType,
        value: NamedSourceRange,
        member: UnresolvedSymbol,
    },
    // Trying to index into non-array typed value
    // Trying to index into non-array typed value
    ArrayIndexNonArray {
        value: NamedSourceRange,
        wrong_type: PrefetchedType,
    },
    // Trying to use struct access on non-struct typed value
    // Trying to use struct access on non-struct typed value
    StructAccessNonStruct {
        value: NamedSourceRange,
        wrong_type: PrefetchedType,
        member: UnresolvedSymbol,
    },
    //Cannot struct-initialize value of type {defined_type_str}
    StructInitializeNonStruct {
        expression_range: NamedSourceRange,
        type_range: NamedSourceRange,
        wrong_type: PrefetchedType,
    },
    // Member {} has name {name:?}, but was expected to have name {this_defined_type:?}.
    StructMemberMismatch {
        member_index: usize,
        struct_type: PrefetchedType,
        expected: SymbolDefinition,
        actual: UnresolvedSymbol,
    },

    // {name:?} only has {num_expected_arguments} parameters
    FunctionCallWrongParameterCount {
        function: SymbolUse,
        defined_parameter_count: usize,
        difference: ParameterCountDifference,
    },
    // {name:?} only has {num_expected_arguments} parameters
    IntrinsicCallWrongParameterCount {
        intrinsic: UnresolvedSymbol,
        defined_parameter_count: usize,
        difference: ParameterCountDifference,
    },

    // No compiler function named {name:?} exists
    IntrinsicUnknown {
        intrinsic: UnresolvedSymbol,
    },
    IntrinsicNotImplemented {
        backend: Backend,
        intrinsic: UnresolvedSymbol,
    },
    // @zero isn't implemented for type {expected_type} in c backend
    InvalidIntrinsicType {
        backend: Backend,
        intrinsic: Intrinsic,
        intrinsic_sym: UnresolvedSymbol,
        type_: ConcreteRuntimeType,
    },

    // Break statements cannot appear outside loops
    LoopControlOutsideLoop {
        kind: LoopControl,
        range: NamedSourceRange,
    },

    GpuLimitationUsed(GpuLimitation),

    // @zero isn't implemented for array type {expected_type} in glsl backend (only 1D arrays with numbers are supported currently)
    ComplexZeroGlsl {
        zero: UnresolvedSymbol,
        type_: ConcreteRuntimeType,
    },
    ImpossibleCast {
        reason: ImpossibleCastReason,
        expression: NamedSourceRange,
        from: PrefetchedType,
        to: PrefetchedType,
    },

    // This expression isn't a valid lvalue
    ExpressionNotLValue {
        expression: NamedSourceRange,
    },

    // "Function parameters must always have a type",
    FunctionParameterUntyped {
        function: SymbolDefinition,
        parameter: SymbolDefinition,
    },
    OnStatementMissingIterator {
        on_range: NamedSourceRange,
    },

    // "For loops must always have a block as the body."
    BlockRequired {
        kind: BlockRequiredKind,
        stmt_range: NamedSourceRange,
    },

    // "This lvalue isn't available here"
    LValueUnavailable {
        value_range: NamedSourceRange,
    },

    // Multiple functions named {name:?} defined
    // "A variable named `{}` was already defined in this scope",
    Duplicate {
        kind: DuplicateKind,
        original: SymbolDefinition,
        new_range: NamedSourceRange,
    },

    // All code paths must return.
    PathDoesntReturn {
        function: SymbolDefinition,
        source_range: NamedSourceRange,
    },

    IncompleteTypeInference {
        range: NamedSourceRange,
        best_guess: Option<PrefetchedType>,
    },

    // Variables cannot have Unit type
    UnitVariable {
        variable: SymbolDefinition,
        type_range: Option<NamedSourceRange>,
        type_: PrefetchedType,
    },
    IteratorValueMaxMismatch {
        on_range: NamedSourceRange,
        iterator: SymbolDefinition,
        max_value_range: NamedSourceRange,
        type_range: Option<NamedSourceRange>,
        iterator_type: PrefetchedType,
        max_type: PrefetchedType,
    },
    // The iterator of an on-statement cannot be mutable
    MutableIterator {
        on_range: NamedSourceRange,
        iterator: SymbolDefinition,
    },
    // Iterators can only be unsigned integers for now
    ImpossibleIteratorType {
        on_range: NamedSourceRange,
        iterator: SymbolDefinition,
        type_range: Option<NamedSourceRange>,
        iterator_type: PrefetchedType,
        possible_iterator_type: PrefetchedType,
    },
    //Arrays can only have integer literals as a size for now
    NonLiteralArrayLength {
        type_range: NamedSourceRange,
        element_type: PrefetchedType,
        length_range: NamedSourceRange,
    },
    // Cannot have array of Unit
    ArrayOfUnit {
        type_range: NamedSourceRange,
        element_type: PrefetchedType,
    },
    // Structs cannot contain Unit
    StructOfUnit {
        member: SymbolDefinition,
        type_: PrefetchedType,
        type_range: NamedSourceRange,
    },

    TypeMismatch {
        kind: TypeMismatchKind,
        value_range: NamedSourceRange,
        expected_types: Vec<PrefetchedType>,
        actual_type: PrefetchedType,
    },

    // Variable {name:?} is constant and can't be used as an lvalue
    // Variable {name:?} is constant and can't be used as an lvalue
    ConstantVariableAsLValue {
        variable: SymbolUse,
    },
    // Only constant variables can be used here. This one isn't.
    ConstantVariableRequired {
        variable: SymbolUse,
    },

    // No function named {name:?} is defined
    // No type alias {name:?} defined
    NotDefined {
        kind: NotDefinedKind,
        item: UnresolvedSymbol,
    },

    //No main function was found.
    NoMainFunction {
        file_start: NamedSourceRange,
    },
    // The GPU backend is disabled for this build of Fleet
    GpuBackendDisabled {
        use_location: NamedSourceRange,
    },

    InternalError(InternalError),
    Lint(Lint),
}

// This basically just mirrors the lsp tags
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HighlightTag {
    Unnecessary,
    Deprecated,
}

#[derive(Clone, Debug)]
pub struct HighlightGroup {
    pub severity: ErrorSeverity,
    pub range: NamedSourceRange,
    pub message: String,
    pub tags: Vec<HighlightTag>,
}

#[derive(Clone, Debug)]
pub struct RenderedError {
    pub highlight_groups: Vec<HighlightGroup>,
    pub main_message: String,
    pub severity: ErrorSeverity,
}

pub trait JoinAnd {
    fn join_and(self) -> String;
}

impl<T: IntoIterator<Item = impl ToString>> JoinAnd for T {
    fn join_and(self) -> String {
        let strings = self.into_iter().map(|x| x.to_string()).collect_vec();
        let (last, start) = strings
            .split_last()
            .expect("Cannot join_and empty iterator");
        if start.is_empty() {
            last.clone()
        } else {
            start.join(", ") + " and " + last
        }
    }
}

pub trait JoinOr {
    fn join_or(self) -> String;
}

impl<T: IntoIterator<Item = impl ToString>> JoinOr for T {
    fn join_or(self) -> String {
        let strings = self.into_iter().map(|x| x.to_string()).collect_vec();
        let (last, start) = strings.split_last().expect("Cannot join_or empty iterator");
        if start.is_empty() {
            last.clone()
        } else {
            start.join(", ") + " or " + last
        }
    }
}

impl ErrorKind {
    pub fn severity(&self) -> ErrorSeverity {
        self.render().severity
    }

    pub fn render(&self) -> RenderedError {
        fn error(range: &NamedSourceRange, message: impl AsRef<str>) -> HighlightGroup {
            HighlightGroup {
                severity: ErrorSeverity::Error,
                range: range.clone(),
                message: message.as_ref().to_string(),
                tags: vec![],
            }
        }
        fn warning(range: &NamedSourceRange, message: impl AsRef<str>) -> HighlightGroup {
            HighlightGroup {
                severity: ErrorSeverity::Warning,
                range: range.clone(),
                message: message.as_ref().to_string(),
                tags: vec![],
            }
        }
        fn note(range: &NamedSourceRange, message: impl AsRef<str>) -> HighlightGroup {
            HighlightGroup {
                severity: ErrorSeverity::Note,
                range: range.clone(),
                message: message.as_ref().to_string(),
                tags: vec![],
            }
        }

        fn an(thing: impl AsRef<str>) -> String {
            let thing = thing.as_ref();
            if let 'a' | 'e' | 'i' | 'o' | 'u' = thing.chars().next().unwrap() {
                format!("an {thing}")
            } else {
                format!("a {thing}")
            }
        }
        fn nth(count: usize) -> String {
            match count {
                0 => panic!(
                    "Trying to refer to the zeroth of something. If this is actually an index, remember to add 1."
                ),
                n if n % 100 == 11 || n % 100 == 12 || n % 100 == 13 => format!("{n}th"),
                n if n % 10 == 1 => format!("{n}st"),
                n if n % 10 == 2 => format!("{n}nd"),
                n if n % 10 == 3 => format!("{n}rd"),
                n => format!("{n}th"),
            }
        }
        fn plural<T>(singular: T, plural: T, count: usize) -> T {
            match count {
                1 => singular,
                _ => plural,
            }
        }

        match self {
            ErrorKind::UnclosedBlockComment { start_token } => RenderedError {
                highlight_groups: vec![error(start_token, "comment starts here")],
                main_message: "This block comment doesn't have a matching `*/` to close it."
                    .to_string(),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::InvalidFloatLexeme { range, lexeme } => RenderedError {
                highlight_groups: vec![error(range, "invalid float literal")],
                main_message: format!(
                    "The characters `{lexeme}` don't form a valid float literal."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::InvalidIntLexeme { range, lexeme } => RenderedError {
                highlight_groups: vec![error(range, "invalid integer literal")],
                main_message: format!(
                    "The characters `{lexeme}` don't form a valid integer literal."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::InvalidEscapeSequence {
                string_range: _,
                escape_range,
                escape_sequence,
            } => RenderedError {
                highlight_groups: vec![error(escape_range, "unknown escape sequence")],
                main_message: format!("Unknown escape sequence `{escape_sequence}`"),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::CharacterLiteralTooBig {
                range,
                actual_length,
            } => RenderedError {
                highlight_groups: vec![error(range, "character literal too big")],
                main_message: format!(
                    "A character literal may only contain a single character. This one contains {actual_length}."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::InvalidCharacters { range, characters } => RenderedError {
                highlight_groups: vec![error(range, "invalid characters")],
                main_message: format!(
                    "The characters `{characters}` are not recognized as part of Fleets syntax."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ParserRecovery {
                range,
                stop_conditions,
            } => RenderedError {
                highlight_groups: vec![warning(range, "recovering parser")],
                main_message: format!(
                    "The parser ran into an error and is recovering to a known-good location indicated by {}",
                    stop_conditions.join_or()
                ),
                severity: ErrorSeverity::Warning,
            },
            ErrorKind::ParserExpectationFailed {
                expectation,
                found_token:
                    Token {
                        type_,
                        range,
                        leading_trivia: _,
                        trailing_trivia: _,
                    },
            } => RenderedError {
                highlight_groups: vec![error(range, "unexpected token")],
                main_message: format!("Expected {expectation}, but found {type_:?} instead."),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ParserExpectationEOF {
                expectation,
                last_token,
            } => RenderedError {
                highlight_groups: vec![error(&last_token.range, "unexpected EOF")],
                main_message: format!(
                    "Expected {expectation}, but hit the end of the file instead.",
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ParserFailureEOF {
                thing_to_parse,
                start_token,
            }
            | ErrorKind::ParserFailure {
                thing_to_parse,
                start_token,
            } => RenderedError {
                highlight_groups: vec![error(&start_token.range, "parser failure")],
                main_message: format!("Failed to parse {}.", an(thing_to_parse)),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::FunctionMissingReturnType { function } => RenderedError {
                highlight_groups: vec![note(&function.definition, "defined here")],
                main_message: format!(
                    "Function `{function}` doesn't have a return type after type inference."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::FunctionMissingParameterTypes { function } => RenderedError {
                highlight_groups: vec![note(&function.definition, "defined here")],
                main_message: format!(
                    "Function `{function}` doesn't have parameter types after type inference."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::IncompleteTypeInferenceVariable {
                variable,
                best_guess,
            } => RenderedError {
                highlight_groups: vec![note(&variable.definition, "defined here")],
                main_message: format!(
                    "The type of variable `{variable}` cannot be completely inferred.{}",
                    if let Some(best_guess) = best_guess {
                        format!(" The best guess is `{best_guess}`.")
                    } else {
                        "".to_string()
                    }
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::NonexistentStructMember {
                struct_type,
                value: _,
                member,
            } => RenderedError {
                highlight_groups: vec![
                    Some(error(&member.use_range, "unknown member")),
                    struct_type
                        .definition_range
                        .as_ref()
                        .map(|range| note(range, "defined here")),
                ]
                .into_iter()
                .flatten()
                .collect(),
                main_message: format!(
                    "Struct `{struct_type}` does not have a member named `{member}`.",
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ArrayIndexNonArray { value, wrong_type } => RenderedError {
                highlight_groups: vec![error(value, "not an array")],
                main_message: format!("Cannot index into value of type `{wrong_type}`."),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::StructAccessNonStruct {
                value,
                wrong_type,
                member,
            } => RenderedError {
                highlight_groups: vec![error(value, "not a struct")],
                main_message: format!(
                    "Cannot access member `{member}` on a value of type `{wrong_type}`."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::StructInitializeNonStruct {
                expression_range: _,
                type_range,
                wrong_type,
            } => RenderedError {
                highlight_groups: vec![error(type_range, "not a struct type")],
                main_message: format!("Cannot struct-initialize value of type `{wrong_type}`."),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::StructMemberMismatch {
                member_index,
                struct_type,
                expected,
                actual,
            } => RenderedError {
                highlight_groups: vec![
                    error(&actual.use_range, "wrong name used here"),
                    note(&expected.definition, "member defined here"),
                ],
                main_message: format!(
                    "The {index_nth} member of type `{struct_type}` is named `{expected}` and not `{actual}`",
                    index_nth = nth(member_index + 1),
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::FunctionCallWrongParameterCount {
                function,
                defined_parameter_count,
                difference: ParameterCountDifference::TooFewGiven(missing_params),
            } => RenderedError {
                highlight_groups: vec![
                    error(&function.use_range, "too few parameters"),
                    note(&function.definition, "function defined here"),
                ],
                main_message: {
                    let given_parameters = defined_parameter_count - missing_params.len();

                    format!(
                        "The function `{function}` expects {defined_parameter_count} {parameters}, but only {given_parameters} {are} given. \
                        Consider adding {missing_params_pretty}.",
                        parameters = plural("parameter", "parameters", *defined_parameter_count),
                        are = plural("is", "are", given_parameters),
                        missing_params_pretty = missing_params
                            .iter()
                            .map(|param| format!("`{}`", param.name))
                            .join_and()
                    )
                },
                severity: ErrorSeverity::Error,
            },
            ErrorKind::FunctionCallWrongParameterCount {
                function,
                defined_parameter_count,
                difference: ParameterCountDifference::TooManyGiven(extra_params),
            } => {
                let mut highlight_groups = vec![
                    error(&function.use_range, "too many parameters"),
                    note(&function.definition, "function defined here"),
                ];
                highlight_groups.extend(
                    extra_params
                        .iter()
                        .map(|param| error(&param.range, "extra parameter")),
                );
                let given_parameters = defined_parameter_count + extra_params.len();
                RenderedError {
                    highlight_groups,
                    main_message: format!(
                        "The function `{function}` expects only {defined_parameter_count} {parameters}, but {given_parameters} {are} given. \
                        Consider removing the last {extras}.",
                        parameters = plural("parameter", "parameters", *defined_parameter_count),
                        are = plural("is", "are", given_parameters),
                        extras = extra_params.len()
                    ),
                    severity: ErrorSeverity::Error,
                }
            }
            ErrorKind::IntrinsicCallWrongParameterCount {
                intrinsic,
                defined_parameter_count,
                difference: ParameterCountDifference::TooFewGiven(missing_params),
            } => RenderedError {
                highlight_groups: vec![error(&intrinsic.use_range, "too few parameters")],
                main_message: format!(
                    "The intrinsic `@{intrinsic}` expects {defined_parameter_count} parameters, but only {given_parameters} are given. \
                    Consider adding {missing_params_pretty}.",
                    given_parameters = defined_parameter_count - missing_params.len(),
                    missing_params_pretty = missing_params
                        .iter()
                        .map(|param| format!("`{}`", param.name))
                        .join_and()
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::IntrinsicCallWrongParameterCount {
                intrinsic,
                defined_parameter_count,
                difference: ParameterCountDifference::TooManyGiven(extra_params),
            } => {
                let mut highlight_groups = vec![error(&intrinsic.use_range, "too many parameters")];
                highlight_groups.extend(
                    extra_params
                        .iter()
                        .map(|param| error(&param.range, "extra parameter")),
                );
                RenderedError {
                    highlight_groups,
                    main_message: format!(
                        "The intrinsic `@{intrinsic}` expects only {defined_parameter_count} parameters, but {given_parameters} are given. \
                        Consider removing the last {extras}.",
                        given_parameters = defined_parameter_count + extra_params.len(),
                        extras = extra_params.len()
                    ),
                    severity: ErrorSeverity::Error,
                }
            }
            ErrorKind::IntrinsicUnknown { intrinsic } => RenderedError {
                highlight_groups: vec![error(&intrinsic.use_range, "unknown intrinsic")],
                main_message: format!("No intrinsic named `{intrinsic}` exists."),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::IntrinsicNotImplemented { backend, intrinsic } => RenderedError {
                highlight_groups: vec![error(&intrinsic.use_range, "unknown intrinsic")],
                main_message: format!(
                    "The intrinsic `@{intrinsic}` is not implemented for the {backend} backend."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::InvalidIntrinsicType {
                backend,
                intrinsic,
                intrinsic_sym,
                type_,
            } => RenderedError {
                highlight_groups: vec![error(
                    &intrinsic_sym.use_range,
                    "invalid type for intrinsic",
                )],
                main_message: format!(
                    "The intrinsic `@{intrinsic}` cannot have the type `{type_}` or it isn't supported by the {backend} backend yet."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::LoopControlOutsideLoop { kind, range } => RenderedError {
                highlight_groups: vec![error(
                    range,
                    match kind {
                        LoopControl::Skip => "`skip` outside of loop",
                        LoopControl::Break => "`break` outside of loop",
                    },
                )],
                main_message: format!(
                    "Loop control statements like `{kind}` can only appear inside of loops."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::GpuLimitationUsed(gpu_limitation) => match gpu_limitation {
                GpuLimitation::ExternalFunction { function } => RenderedError {
                    highlight_groups: vec![error(
                        &function.definition,
                        "external function used on GPU",
                    )],
                    main_message: format!(
                        "The external function `{function}` is (possibly indirectly) called from inside an on-statement running on the GPU.",
                    ),
                    severity: ErrorSeverity::Error,
                },
                GpuLimitation::OnStatement { statement } => RenderedError {
                    highlight_groups: vec![error(statement, "on-statement used on GPU")],
                    main_message: "This on-statement is (possibly indirectly) reachable from \
                        inside another on-statement running on the GPU."
                        .to_string(),
                    severity: ErrorSeverity::Error,
                },
                GpuLimitation::GpuFunction { function } => RenderedError {
                    highlight_groups: vec![
                        error(&function.use_range, "external function used on GPU"),
                        note(&function.definition, "function defined here"),
                    ],
                    main_message: format!(
                        "The function `{function}` is (possibly indirectly) called from inside an on-statement running on the GPU. \
                        Because it also uses the GPU, this would result in nested on-statements which aren't allowed.",
                    ),
                    severity: ErrorSeverity::Error,
                },
            },
            ErrorKind::ComplexZeroGlsl { zero, type_ } => RenderedError {
                highlight_groups: vec![error(&zero.use_range, "type too complicated")],
                main_message: format!(
                    "The GLSL backend doesn't yet support `@zero` intrinsics of type `{type_}`. \
                    Only primitive types and one-dimensional number arrays are supported.",
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ImpossibleCast {
                reason,
                expression,
                from,
                to,
            } => {
                let explanation = match reason {
                    ImpossibleCastReason::InvolvesUnit { direction } => format!(
                        "Casting {direction} `{}` is always impossible.",
                        ConcreteRuntimeType::Unit
                    ),
                    ImpossibleCastReason::ArrayElementsIncompatible => {
                        "These arrays have different element types.".to_string()
                    }
                    ImpossibleCastReason::ArrayLengthIncompatible { from_size, to_size } => {
                        format!(
                            "The source array has length {from_size} while \
                            the target has length {to_size}."
                        )
                    }
                    ImpossibleCastReason::ArrayAndNonArray {
                        direction: CastDirection::To,
                    } => "Casting from a non-array to an array is always impossible.".to_string(),
                    ImpossibleCastReason::ArrayAndNonArray {
                        direction: CastDirection::From,
                    } => "Casting from an array to a non-array is always impossible.".to_string(),
                    ImpossibleCastReason::DifferentStructOrigin => {
                        "Casting between structs with equal members \
                        that stem from different places is forbidden.\
                        This ensures forwards compatibility for when \
                        generics are implemented."
                            .to_string()
                    }
                    ImpossibleCastReason::StructMembersDiffer => {
                        "These are not the same struct because they have different members."
                            .to_string()
                    }
                    ImpossibleCastReason::StructAndNonStruct {
                        direction: CastDirection::To,
                    } => "Casting from a non-struct to a struct is always impossible.".to_string(),
                    ImpossibleCastReason::StructAndNonStruct {
                        direction: CastDirection::From,
                    } => "Casting from a struct to a non-struct is always impossible.".to_string(),
                };

                RenderedError {
                    highlight_groups: vec![error(expression, "impossible cast")],
                    main_message: format!(
                        "Casting from `{from}` to `{to}` is not allowed. {explanation}"
                    ),
                    severity: ErrorSeverity::Error,
                }
            }
            ErrorKind::ExpressionNotLValue { expression } => RenderedError {
                highlight_groups: vec![error(expression, "not an l-value")],
                main_message: "This expression isn't an l-value and cannot be assigned to."
                    .to_string(),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::FunctionParameterUntyped {
                function,
                parameter,
            } => RenderedError {
                highlight_groups: vec![error(&parameter.definition, "parameter missing type")],
                main_message: format!(
                    "Parameter `{parameter}` of function `{function}` doesn't have a type annotation."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::OnStatementMissingIterator { on_range } => RenderedError {
                highlight_groups: vec![error(on_range, "missing iterator")],
                main_message: "On-statements must always have at least one iterator. \
                    This requirement will be lifted in the future."
                    .to_string(),

                severity: ErrorSeverity::Error,
            },
            ErrorKind::BlockRequired { kind, stmt_range } => RenderedError {
                highlight_groups: vec![error(stmt_range, "not a block")],
                main_message: match kind {
                    BlockRequiredKind::Function => {
                        "Functions must always have a block as the body."
                    }
                    BlockRequiredKind::If => {
                        "If statements must always have a block as the if body."
                    }
                    BlockRequiredKind::Elif => {
                        "If statements must always have a block as elif bodies."
                    }
                    BlockRequiredKind::Else => {
                        "If statements must always have a block as the else body."
                    }
                    BlockRequiredKind::While => "While loops must always have a block as the body.",
                    BlockRequiredKind::For => "For loops must always have a block as the body.",
                }
                .to_string(),

                severity: ErrorSeverity::Error,
            },
            ErrorKind::LValueUnavailable { value_range } => RenderedError {
                highlight_groups: vec![error(value_range, "l-value unavailable")],
                main_message: "This l-value isn't available in this context.".to_string(),

                severity: ErrorSeverity::Error,
            },
            ErrorKind::Duplicate {
                kind,
                original,
                new_range,
            } => RenderedError {
                highlight_groups: vec![
                    error(new_range, "name is already in use"),
                    note(&original.definition, "previously defined here"),
                ],
                main_message: match kind {
                    DuplicateKind::Function => {
                        format!("There already exists a function named `{original}`.")
                    }
                    DuplicateKind::Variable => {
                        format!("There already exists a variable named `{original}` in this scope.")
                    }
                    DuplicateKind::TypeAlias => {
                        format!("There already exists a type alias named `{original}`.")
                    }
                    DuplicateKind::StructMember => {
                        format!("This struct already has a member named `{original}`.")
                    }
                },

                severity: ErrorSeverity::Error,
            },
            ErrorKind::PathDoesntReturn {
                function,
                source_range,
            } => RenderedError {
                highlight_groups: vec![error(source_range, "path doesn't return")],
                main_message: format!("Not all code paths in `{function}` return."),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::IncompleteTypeInference { range, best_guess } => RenderedError {
                highlight_groups: vec![error(range, "cannot completely infer type")],
                main_message: format!(
                    "The type of this expression cannot be completely inferred.{}",
                    if let Some(best_guess) = best_guess {
                        format!(" The best guess is `{best_guess}`.")
                    } else {
                        "".to_string()
                    }
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::UnitVariable {
                variable,
                type_range: _,
                type_,
            } => RenderedError {
                highlight_groups: vec![error(
                    &variable.definition,
                    format!("`{}` variable not allowed", ConcreteRuntimeType::Unit),
                )],
                main_message: format!(
                    "The variable `{variable}` has type `{type_}`, which isn't supported yet."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::IteratorValueMaxMismatch {
                on_range: _,
                iterator,
                max_value_range,
                type_range: _,
                iterator_type,
                max_type,
            } => RenderedError {
                highlight_groups: vec![error(max_value_range, "type mismatch")],
                main_message: format!(
                    "The iterator `{iterator}` has type `{iterator_type}`, but the max value has type `{max_type}`."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::MutableIterator {
                on_range: _,
                iterator,
            } => RenderedError {
                highlight_groups: vec![error(&iterator.definition, "mutable iterator")],
                main_message: format!(
                    "On-statement iterators must not be mutable, but `{iterator}` is."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ImpossibleIteratorType {
                on_range: _,
                iterator,
                type_range,
                iterator_type,
                possible_iterator_type,
            } => RenderedError {
                highlight_groups: vec![error(
                    type_range.as_ref().unwrap_or(&iterator.definition),
                    "invalid iterator type",
                )],
                main_message: format!(
                    "The iterator `{iterator}` has type `{iterator_type}`, but only `{possible_iterator_type}` is supported for now."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::NonLiteralArrayLength {
                type_range: _,
                element_type: _,
                length_range,
            } => RenderedError {
                highlight_groups: vec![error(length_range, "non-literal array length")],
                main_message: "Array type need to have an integer literal as the size for now."
                    .to_string(),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ArrayOfUnit {
                type_range,
                element_type,
            } => RenderedError {
                highlight_groups: vec![error(
                    type_range,
                    format!("`{}` array not allowed", element_type),
                )],
                main_message: format!("Arrays cannot have elements of type `{element_type}` yet."),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::StructOfUnit {
                member,
                type_,
                type_range,
            } => RenderedError {
                highlight_groups: vec![error(
                    type_range,
                    format!("`{}` member not allowed", type_),
                )],
                main_message: format!(
                    "Member `{member}` has an invalid type. Structs cannot have members of type `{type_}` yet."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::TypeMismatch {
                kind,
                value_range,
                expected_types,
                actual_type,
            } => {
                let expected_types_str = expected_types
                    .iter()
                    .map(|type_| format!("`{type_}`"))
                    .join_or();

                fn format_unary_op(op: UnaryOperation, operand: impl Display) -> String {
                    match op {
                        UnaryOperation::BitwiseNot => format!("bitwise negate {operand}"),
                        UnaryOperation::LogicalNot => format!("logically negate {operand}"),
                        UnaryOperation::Negate => format!("arithmetically negate {operand}"),
                    }
                }
                fn format_binary_op(
                    op: BinaryOperation,
                    left: impl Display,
                    right: impl Display,
                ) -> String {
                    match op {
                        BinaryOperation::Add => format!("add {right} to {left}"),
                        BinaryOperation::Subtract => format!("subtract {right} from {left}"),
                        BinaryOperation::Multiply => format!("multiply {left} by {right}"),
                        BinaryOperation::Divide => format!("divide {left} by {right}"),
                        BinaryOperation::Modulo => format!("module {left} by {right}"),
                        BinaryOperation::GreaterThan => format!("compare {left} > {right}"),
                        BinaryOperation::GreaterThanOrEqual => format!("compare {left} >= {right}"),
                        BinaryOperation::LessThan => format!("compare {left} < {right}"),
                        BinaryOperation::LessThanOrEqual => format!("compare {left} <= {right}"),
                        BinaryOperation::Equal => format!("compare {left} == {right}"),
                        BinaryOperation::NotEqual => format!("compare {left} != {right}"),
                        BinaryOperation::LogicalAnd => format!("logically AND {left} with {right}"),
                        BinaryOperation::LogicalOr => format!("logically OR {left} with {right}"),
                    }
                }

                let (mut highlight_groups, primary_hl_message, main_message) = match kind {
                    TypeMismatchKind::FunctionReturn { function } => (
                        expected_types
                            .iter()
                            .flat_map(|type_| {
                                Some(note(
                                    type_.definition_range.as_ref()?,
                                    "return type defined here",
                                ))
                            })
                            .collect_vec(),
                        "different return type used".to_string(),
                        format!(
                            "Function `{function}` is defined to return {expected_types_str}, but it returns `{actual_type}` here."
                        ),
                    ),
                    TypeMismatchKind::VariableInitializer { variable } => (
                        vec![],
                        "initializer doesn't match variable definition".to_string(),
                        format!(
                            "Variable `{variable}` is defined to have type {expected_types_str}, but its initializer has type `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::ArrayElement {
                        first_element_range,
                    } => (
                        vec![note(first_element_range, "inferred from here")],
                        "element type mismatch".to_string(),
                        format!(
                            "This array element should have type {expected_types_str}, but actually has type `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::StructMember {
                        member_index: _,
                        member,
                    } => (
                        vec![note(&member.definition, "member defined here")],
                        "different type here".to_string(),
                        format!(
                            "Struct member `{member}` should have type {expected_types_str}, but actually has type `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::StructFields { type_expression: _ } => (
                        vec![],
                        "mismatched struct fields".to_string(),
                        format!(
                            "This struct expression should have fields for type {expected_types_str}, but actually \
                            has fields that form type `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::FunctionCallParameter {
                        parameter_index,
                        parameter,
                        function,
                    } => {
                        let index_nth = nth(*parameter_index + 1);
                        (
                            vec![note(&parameter.definition, "parameter defined here")],
                            "type mismatch".to_string(),
                            format!(
                                "The {index_nth} parameter `{parameter}` of function `{function}` has type {expected_types_str}, \
                                but `{actual_type}` is passed here."
                            ),
                        )
                    }
                    TypeMismatchKind::IntrinsicCallParameter {
                        parameter_index,
                        parameter_name,
                        intrinsic,
                    } => {
                        let index_nth = nth(*parameter_index + 1);
                        (
                            vec![],
                            "type mismatch".to_string(),
                            format!(
                                "The {index_nth} parameter `{parameter_name}` of intrinsic `@{intrinsic}` has type {expected_types_str}, \
                                but `{actual_type}` is passed here."
                            ),
                        )
                    }
                    TypeMismatchKind::ArrayIndex { array: _ } => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Indexing into an array is done using {expected_types_str}, not `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::BinaryOperationLeft(binary_operation) => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Cannot {op}. Expected {expected_types_str}.",
                            op = format_binary_op(
                                *binary_operation,
                                format!("`{actual_type}`"),
                                "anything"
                            )
                        ),
                    ),
                    TypeMismatchKind::BinaryOperationRight(binary_operation) => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Cannot {op}. Expected `{expected_types_str}`.",
                            op = format_binary_op(
                                *binary_operation,
                                "anything",
                                format!("`{actual_type}`")
                            )
                        ),
                    ),
                    TypeMismatchKind::BinaryOperation {
                        operation,
                        right_expected_types,
                        right_actual_type,
                    } => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Cannot {op}. Expected {expected_types_str} and {right_expected_str}.",
                            op = format_binary_op(
                                *operation,
                                format!("`{actual_type}`"),
                                format!("`{right_actual_type}`")
                            ),
                            right_expected_str = right_expected_types
                                .iter()
                                .map(|type_| format!("`{type_}`"))
                                .join_or()
                        ),
                    ),
                    TypeMismatchKind::UnaryOperation(unary_operation) => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Cannot {op}. Expected {expected_types_str}.",
                            op = format_unary_op(*unary_operation, format!("`{actual_type}`")),
                        ),
                    ),
                    TypeMismatchKind::LValueAssignment { lvalue: _ } => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Cannot assign value of type {expected_types_str} to lvalue of type `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::GpuIndex { gpu: _ } => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Indexing into an array (including the available GPUs) is \
                            done using {expected_types_str}, not `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::ThreadIndex { thread: _ } => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Indexing into an array (including the available threads) is \
                            done using {expected_types_str}, not `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::IfCondition => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "If conditions always have type {expected_types_str}, not `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::ElifCondition => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "Elif conditions always have type {expected_types_str}, not `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::WhileCondition => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "While conditions always have type {expected_types_str}, not `{actual_type}`."
                        ),
                    ),
                    TypeMismatchKind::ForCondition => (
                        vec![],
                        "type mismatch".to_string(),
                        format!(
                            "For conditions always have type {expected_types_str}, not `{actual_type}`."
                        ),
                    ),
                };
                highlight_groups.insert(0, error(value_range, primary_hl_message));

                RenderedError {
                    highlight_groups,
                    main_message,
                    severity: ErrorSeverity::Error,
                }
            }
            ErrorKind::ConstantVariableAsLValue { variable } => RenderedError {
                highlight_groups: vec![error(
                    &variable.use_range,
                    "cannot assign to constant variable",
                )],
                main_message: format!(
                    "Variable `{variable}` is constant and cannot be used as an l-value."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::ConstantVariableRequired { variable } => RenderedError {
                highlight_groups: vec![error(&variable.use_range, "constant variable required")],
                main_message: format!(
                    "Variable `{variable}` isn't constant and cannot be used in this context."
                ),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::NotDefined { kind, item } => RenderedError {
                highlight_groups: vec![error(
                    &item.use_range,
                    match kind {
                        NotDefinedKind::Function => "function not defined",
                        NotDefinedKind::Variable => "variable not defined",
                        NotDefinedKind::TypeAlias => "type alias not defined",
                    },
                )],
                main_message: match kind {
                    NotDefinedKind::Function => {
                        format!("No function named `{item}` is defined.")
                    }
                    NotDefinedKind::Variable => {
                        format!("No variable named `{item}` is accessible from here.")
                    }
                    NotDefinedKind::TypeAlias => {
                        format!("No type alias named `{item}` is defined.")
                    }
                },

                severity: ErrorSeverity::Error,
            },
            ErrorKind::NoMainFunction { file_start } => RenderedError {
                highlight_groups: vec![error(file_start, "missing entry point")],
                main_message: "No `main` function could be found.".to_string(),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::GpuBackendDisabled { use_location } => RenderedError {
                highlight_groups: vec![error(use_location, "GPU backend disabled")],
                main_message: "The GPU backend is disabled for this build of Fleet.".to_string(),
                severity: ErrorSeverity::Error,
            },
            ErrorKind::InternalError(internal_error) => match internal_error {
                InternalError::LlvmRuntimeLinkFailure {
                    file_start,
                    error: error_msg,
                    module_dump,
                } => RenderedError {
                    highlight_groups: vec![error(file_start, "runtime link failure")],
                    main_message: format!(
                        "Linking with runtime library declarations failed.\nError: {error_msg}\nModule Dump:\n{module_dump}"
                    ),
                    severity: ErrorSeverity::Error,
                },
                InternalError::GlslGenerationFailed {
                    statement,
                    error: error_str,
                } => RenderedError {
                    highlight_groups: vec![error(statement, "GLSL generation failed")],
                    main_message: format!(
                        "Generating the GLSL shader for this on-statement failed.\n Error: {error_str}"
                    ),
                    severity: ErrorSeverity::Error,
                },
                InternalError::ShadercError {
                    statement,
                    glsl,
                    error: error_str,
                } => RenderedError {
                    highlight_groups: vec![error(statement, "shaderc error")],
                    main_message: format!(
                        "ShaderC failed to compile this on-statements shader.\nError: {error_str}\nGenerated GLSL:\n{glsl}"
                    ),
                    severity: ErrorSeverity::Error,
                },
                InternalError::LlvmModuleInvalid {
                    file_start,
                    error: error_msg,
                    module_dump,
                } => RenderedError {
                    highlight_groups: vec![error(file_start, "llvm module invalid")],
                    main_message: format!(
                        "The generated LLVM IR module is invalid.\nError: {error_msg}\nModule Dump:\n{module_dump}"
                    ),
                    severity: ErrorSeverity::Error,
                },
                InternalError::LlvmFunctionInvalid { function } => RenderedError {
                    highlight_groups: vec![error(&function.definition, "llvm ir invalid")],
                    main_message: "The generated LLVM IR for this function is invalid. \
                        Sadly, LLVM doesn't expose errors per function so you'll need to \
                        look at the module-wide error at the top of the file."
                        .to_string(),
                    severity: ErrorSeverity::Error,
                },
                InternalError::LlvmUnsupportedMainReturnType {
                    type_,
                    main_function,
                } => RenderedError {
                    highlight_groups: vec![error(
                        &main_function.definition,
                        "unsupported return type",
                    )],
                    main_message: format!("The main function cannot return type `{type_}`."),
                    severity: ErrorSeverity::Error,
                },
                InternalError::LlvmNumberLiteralMistyped { literal } => RenderedError {
                    highlight_groups: vec![error(literal, "invalid llvm type")],
                    main_message: "This number literal doesn't actually have a number type."
                        .to_string(),
                    severity: ErrorSeverity::Error,
                },
                InternalError::ScopeAnalysisMissedFunction { function } => RenderedError {
                    highlight_groups: vec![error(
                        &function.definition,
                        "missed during scope analysis",
                    )],
                    main_message: "Function `{function}` was not processed during scope analysis."
                        .to_string(),
                    severity: ErrorSeverity::Error,
                },
            },
            ErrorKind::Lint(Lint::SelfCast { expression, type_ }) => RenderedError {
                highlight_groups: vec![warning(expression, "unnecessary cast")],
                main_message: format!("Casting from type `{type_}` to itself is redundant."),
                severity: ErrorSeverity::Warning,
            },
            ErrorKind::Lint(Lint::ToIdkCast {
                expression,
                from_type,
                to_type,
            }) => RenderedError {
                highlight_groups: vec![warning(expression, "unnecessary cast")],
                main_message: format!(
                    "Casting from any type (including `{from_type}`) to `{to_type}` is redundant."
                ),
                severity: ErrorSeverity::Warning,
            },
            ErrorKind::Lint(Lint::CodeUnreachable { range }) => RenderedError {
                highlight_groups: vec![HighlightGroup {
                    severity: ErrorSeverity::Warning,
                    range: range.clone(),
                    message: "unreachable code".to_string(),
                    tags: vec![HighlightTag::Unnecessary],
                }],
                main_message: "This code can never be reached.".to_string(),
                severity: ErrorSeverity::Warning,
            },
            ErrorKind::Lint(Lint::UnnecessaryParentheses { opening, closing }) => RenderedError {
                highlight_groups: vec![
                    note(opening, "unnecessary parentheses"),
                    note(closing, "unnecessary parentheses"),
                ],
                main_message: "These parentheses are redundant and can be removed.".to_string(),
                severity: ErrorSeverity::Note,
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum ImpossibleCastReason {
    // Cannot cast to or from Unit
    InvolvesUnit { direction: CastDirection },
    // Casting array of type {} to array of type {} is impossible because the element types can't be cast
    ArrayElementsIncompatible,
    // Casting array with length {} to length {} is impossible
    ArrayLengthIncompatible { from_size: usize, to_size: usize },
    // Casting non-array of type {} to array of type {} is impossible
    ArrayAndNonArray { direction: CastDirection },
    // \n From {} (source_hash: {a_hash:x?}) to {} (source_hash: {b_hash:x?})
    DifferentStructOrigin,
    // Cannot cast between unrelated structs {} and {}
    StructMembersDiffer,
    // Cannot cast from or to struct type: trying to cast from {} to {}
    StructAndNonStruct { direction: CastDirection },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CastDirection {
    To,
    From,
}

impl Display for CastDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CastDirection::To => f.write_str("to"),
            CastDirection::From => f.write_str("from"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExtraParameter {
    pub range: NamedSourceRange,
    pub type_: PrefetchedType,
}
#[derive(Clone, Debug)]
pub struct MissingParameter {
    pub name: String,
    /// Intrinsics don't have a definition so this will be `None`
    pub name_range: Option<NamedSourceRange>,
    pub type_: PrefetchedType,
}

#[derive(Clone, Debug)]
pub enum ParameterCountDifference {
    TooManyGiven(Vec<ExtraParameter>),
    // {name:?} is missing parameter {:?} (Nr. {}) of type {}
    TooFewGiven(Vec<MissingParameter>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LoopControl {
    Skip,
    Break,
}

impl Display for LoopControl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoopControl::Skip => f.write_str("skip"),
            LoopControl::Break => f.write_str("break"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BlockRequiredKind {
    Function,
    If,
    Elif,
    Else,
    While,
    For,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NotDefinedKind {
    Function,
    Variable,
    TypeAlias,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DuplicateKind {
    Function,
    Variable,
    TypeAlias,
    StructMember,
}

#[derive(Clone, Debug)]
pub enum TypeMismatchKind {
    FunctionReturn {
        function: SymbolDefinition,
    },
    // Variable {:?} is defined as type {}, but the initializer value has type {}
    VariableInitializer {
        variable: SymbolDefinition,
    },
    // This item has type {this_item_type_str}, but was expected to have type {element_type_str} (inferred from the first element)
    ArrayElement {
        first_element_range: NamedSourceRange,
    },
    // Member {name:?} has type {this_type_str}, but was expected to have type {this_defined_type_str}.
    StructMember {
        member_index: usize,
        member: SymbolUse,
    },
    // This struct initializer has type {defined_type_str}, but has fields for type {constructed_type}.
    StructFields {
        type_expression: NamedSourceRange,
    },
    // {name:?} expects a value of type {} as argument {:?} (Nr. {}). Got {}
    FunctionCallParameter {
        parameter_index: usize,
        parameter: SymbolDefinition,
        function: SymbolUse,
    },
    // {name:?} expects a value of type {} as argument {param_name:?} (Nr. {}). Got {}
    IntrinsicCallParameter {
        parameter_index: usize,
        parameter_name: String,
        intrinsic: UnresolvedSymbol,
    },
    // Cannot index into array using index of type {index_type_str}. Expected an unsigned integer
    // Cannot index into array using index of type {index_type_str}. Expected an unsigned integer
    ArrayIndex {
        array: NamedSourceRange,
    },
    /*
            let (verb, l_expected, preposition, r_expected) = match operation {
                BinaryOperation::Add => ("add", "number", "to", "number"),
                BinaryOperation::Subtract => ("subtract", "number", "from", "number"),
                BinaryOperation::Multiply => ("multiply", "number", "by", "number"),
                BinaryOperation::Divide => ("divide", "number", "by", "number"),
                BinaryOperation::Modulo => ("modulo", "number", "by", "number"),
                BinaryOperation::GreaterThan => ("compare", "number", ">", "number"),
                BinaryOperation::GreaterThanOrEqual => ("compare", "number", ">=", "number"),
                BinaryOperation::LessThan => ("compare", "number", "<", "number"),
                BinaryOperation::LessThanOrEqual => ("compare", "number", "<=", "number"),
                BinaryOperation::Equal => {
                    ("compare", "number or boolean", "==", "number or boolean")
                }
                BinaryOperation::NotEqual => {
                    ("compare", "number or boolean", "!=", "number or boolean")
                }
                BinaryOperation::LogicalAnd => ("logically AND", "boolean", "with", "boolean"),
                BinaryOperation::LogicalOr => ("logically OR", "boolean", "with", "boolean"),
            };
    */
    // Cannot {verb} {left_type_str}. Expected {l_expected}.
    BinaryOperationLeft(BinaryOperation),
    // Cannot {verb} {preposition} {right_type_str}. Expected {r_expected}.
    BinaryOperationRight(BinaryOperation),
    // Cannot {verb} {left_type_str} {preposition} {right_type_str}. Expected {l_expected} and {r_expected}.
    BinaryOperation {
        operation: BinaryOperation,
        right_expected_types: Vec<PrefetchedType>,
        right_actual_type: PrefetchedType,
    },
    /*
            let (verb, expected) = match operation {
                UnaryOperation::BitwiseNot => ("bitwise negate", "an integer"),
                UnaryOperation::LogicalNot => ("logically negate", "a number or boolean"),
                UnaryOperation::Negate => ("arithmetically negate", "a signed number"),
            };
                format!("Cannot {verb} {type_str}. Expected {expected}."),
    */
    UnaryOperation(UnaryOperation),
    // Cannot assign value of type {right_type_str} to lvalue of type {left_type_str}
    LValueAssignment {
        lvalue: NamedSourceRange,
    },
    // Expected gpu index to be {}. Got value of type {} instead
    GpuIndex {
        gpu: NamedSourceRange,
    },
    // Expected thread index to be {}. Got value of type {} instead
    ThreadIndex {
        thread: NamedSourceRange,
    },
    // Expected if condition to be {}. Got value of type {} instead
    IfCondition,
    // Expected elif condition to be {}. Got value of type {} instead
    ElifCondition,
    // Expected while  condition to be {}. Got value of type {} instead
    WhileCondition,
    // Expected for  condition to be {}. Got value of type {} instead
    ForCondition,
}

#[derive(Clone, Debug)]
pub enum InternalError {
    // Linking with runtime library declarations failed: {}\nModule dump:\n{}
    LlvmRuntimeLinkFailure {
        file_start: NamedSourceRange,
        error: String,
        module_dump: String,
    },
    GlslGenerationFailed {
        statement: NamedSourceRange,
        error: String,
    },
    // {source}\n----------\nInternal shaderc error: {err}
    ShadercError {
        statement: NamedSourceRange,
        glsl: String,
        error: String,
    },

    // LLVM module is invalid: {}\nModule dump:\n{}
    LlvmModuleInvalid {
        file_start: NamedSourceRange,
        error: String,
        module_dump: String,
    },
    // The IR generated for this function is invalid. See the global error at the start of the file for more info
    LlvmFunctionInvalid {
        function: SymbolDefinition,
    },

    // Main function returns unsupported type {other_type}
    LlvmUnsupportedMainReturnType {
        type_: ConcreteRuntimeType,
        main_function: SymbolDefinition,
    },

    // This was neither a float nor int in the llvm backend
    LlvmNumberLiteralMistyped {
        literal: NamedSourceRange,
    },

    ScopeAnalysisMissedFunction {
        function: SymbolDefinition,
    },
}

#[derive(Clone, Debug)]
pub enum GpuLimitation {
    ExternalFunction { function: SymbolDefinition },
    // Cannot use on-statements on the GPU
    OnStatement { statement: NamedSourceRange },
    // This function (possibly indirectly) uses the gpu and can therefore not itself be called from the gpu
    GpuFunction { function: SymbolUse },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Intrinsic {
    Zero,
    Sqrt,
    Sin,
    Cos,
}

impl Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Intrinsic::Zero => f.write_str("@zero"),
            Intrinsic::Sqrt => f.write_str("@sqrt"),
            Intrinsic::Sin => f.write_str("@sin"),
            Intrinsic::Cos => f.write_str("@cos"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Backend {
    C,
    Glsl,
    Llvm,
}

impl Display for Backend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Backend::C => f.write_str("C"),
            Backend::Glsl => f.write_str("GLSL"),
            Backend::Llvm => f.write_str("LLVM"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FleetError {
    /// ranges are guaranteed to be sorted and non-overlapping
    highlight_groups: Vec<(NamedSourceRange, ErrorSeverity)>,
    pub message: String,
    pub main_severity: ErrorSeverity,
}

impl FleetError {
    pub fn highlight_groups(&self) -> &Vec<(NamedSourceRange, ErrorSeverity)> {
        &self.highlight_groups
    }

    pub fn to_string(&self, source: &str) -> String {
        self.to_string_impl(source, false)
    }
    pub fn to_string_ansi(&self, source: &str) -> String {
        self.to_string_impl(source, true)
    }

    pub fn start(&self) -> NamedSourceLocation {
        self.highlight_groups
            .iter()
            .map(|(range, _severity)| range.start())
            .min()
            .expect("FleetError without highlight group")
    }
    pub fn end(&self) -> NamedSourceLocation {
        self.highlight_groups
            .iter()
            .map(|(range, _severity)| range.end())
            .max()
            .expect("FleetError without highlight group")
    }

    fn to_string_impl(&self, source: &str, use_ansi: bool) -> String {
        let color_gen = |severity: ErrorSeverity| {
            let nr = match severity {
                ErrorSeverity::Error => 31,
                ErrorSeverity::Warning => 33,
                ErrorSeverity::Note => 34,
            };

            if use_ansi {
                (format!("\x1B[{nr}m"), "\x1B[0m")
            } else {
                ("".to_string(), "")
            }
        };

        let self_start = self.start();
        let self_end = self.end();

        let num_before_error_lines = 3;
        let num_after_error_lines = 3;

        let max_line_number_len = (self_end.line() + num_after_error_lines).to_string().len();

        let pad_with_line_number =
            |(line, text): (usize, &str)| format!("{line:<max_line_number_len$}| {text}");

        let source_lines = source
            .split("\n")
            .enumerate()
            .map(|(line, text)| (line + 1, text));

        let before_err = source_lines.clone().take(self_start.line() - 1);
        let err = source_lines
            .clone()
            .skip(self_start.line() - 1)
            .take(self_end.line() - self_start.line() + 1)
            .map(|(line, text)| {
                let mut text = text.to_string();

                let mut offset = 0;

                for (named_range, severity) in &self.highlight_groups {
                    let SourceRange {
                        start: hl_start,
                        end: hl_end,
                    } = named_range.range;

                    assert!(hl_start <= hl_end);
                    if line > hl_end.line || line < hl_start.line {
                        continue;
                    }

                    let start_col = if line == hl_start.line {
                        hl_start.column + offset
                    } else {
                        0
                    };
                    let end_col = if line == hl_end.line {
                        (hl_end.column + offset).min(text.len())
                    } else {
                        text.len()
                    };

                    let (enable_color, disable_color) = color_gen(*severity);

                    let new_text = format!(
                        "{}{enable_color}{}{disable_color}{}",
                        &text[..start_col],
                        &text[start_col..end_col],
                        &text[end_col..]
                    );

                    offset += new_text.len() - text.len();
                    text = new_text;
                }

                pad_with_line_number((line, &text))
            })
            .join("\n");
        let after_err = source_lines.skip(self_end.line());

        let before_err_trunc = before_err
            .skip(self_start.line().saturating_sub(num_before_error_lines + 1))
            .skip_while(|(_line, text)| text.trim() == "")
            .map(pad_with_line_number)
            .join("\n");
        let after_err_trunc = after_err
            .take(num_after_error_lines)
            .map(pad_with_line_number)
            .join("\n");

        assert!(
            !self.message.ends_with("\n"),
            "Message of error ends with newline: {self:#?}"
        );

        let (enable_color, disable_color) = color_gen(self.main_severity);
        let mut output = format!(
            "{enable_color}[{}] {}:{}:{}: {}{disable_color}",
            match self.main_severity {
                ErrorSeverity::Error => "ERROR",
                ErrorSeverity::Warning => "WARNING",
                ErrorSeverity::Note => "NOTE",
            },
            self.start().name.0,
            self.start().line(),
            self.start().column(),
            self.message
        );

        if !before_err_trunc.is_empty() {
            output.push('\n');
            output.push_str(&before_err_trunc);
        }
        if !err.is_empty() {
            output.push('\n');
            output.push_str(&err);
        }
        if !after_err_trunc.is_empty() {
            output.push('\n');
            output.push_str(&after_err_trunc);
        }

        output
    }
}

pub fn insert_fix_passes(pm: &mut PassManager) {
    pm.insert::<RemoveParensPass>();
    pm.insert::<FixNonBlockStatements>();
    pm.insert::<FixTrailingComma>();
    pm.insert::<ErrMissingTypeInParam>();
    pm.insert::<ErrTooFewIterators>();
}

pub fn insert_minimal_pipeline(pm: &mut PassManager) {
    pm.insert::<Tokenizer>();
    pm.insert::<Parser>();
}
pub fn insert_compile_passes(pm: &mut PassManager) {
    pm.insert::<ScopeAnalyzer>();
    pm.insert::<TypePropagator>();
    pm.insert::<TypeConcretisationPass>();
    pm.insert::<LValueReducer>();
    pm.insert::<StatTracker>();
    pm.insert::<GLSLCodeGenerator>();
}

pub fn insert_c_passes(pm: &mut PassManager) {
    pm.insert::<CCodeGenerator>();
}

NewtypeDeref!(ParseErrorsOnly, Errors);

pub fn format(source: InputSource) -> Result<String, PassError> {
    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    pm.insert::<StorePass<Errors, ParseErrorsOnly>>();
    insert_fix_passes(&mut pm);
    pm.insert::<AstToDocumentModelConverter>();

    pm.state.insert(source);
    pm.state.insert_default::<Errors>();

    pm.run()?;

    let de = pm
        .state
        .get::<DocumentElement>()
        .expect("Formatting passes failed")
        .clone();

    if pm
        .state
        .get::<ParseErrorsOnly>()
        .unwrap()
        .iter()
        .any(|err| err.severity() == ErrorSeverity::Error)
    {
        return Err(PassError::InvalidInput {
            producing_pass: "Formatting function".to_string(),
            source: "Not formatting malformed input".into(),
        });
    }

    Ok(stringify_document(de))
}

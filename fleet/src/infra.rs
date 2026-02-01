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
}

#[derive(Clone, Debug)]
pub struct PrefetchedType {
    pub definition_range: Option<NamedSourceRange>,
    pub kind: RuntimeTypeKind,
    pub str: String,
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
        wrong_type: PrefetchedType,
    },
    // Member {} has name {name:?}, but was expected to have name {this_defined_type:?}.
    StructMemberMismatch {
        member_index: usize,
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
        kind: UniquelyNamed,
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
    },
    //Arrays can only have integer literals as a size for now
    NonLiteralArrayLength {
        local_expr: NamedSourceRange,
        element_type: PrefetchedType,
        length_range: NamedSourceRange,
    },
    // Cannot have array of Unit
    ArrayOfUnit {
        local_expr: NamedSourceRange,
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
        kind: UniquelyNamed,
        item: UnresolvedSymbol,
    },

    //No main function was found.
    NoMainFunction,
    // The GPU backend is disabled for this build of Fleet
    GpuBackendDisabled {
        use_location: NamedSourceRange,
    },

    InternalError(InternalError),
    Lint(Lint),
}

impl ErrorKind {
    pub fn severity(&self) -> ErrorSeverity {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub enum ImpossibleCastReason {
    // Cannot cast to or from Unit
    InvolvesUnit { direction: CastDirection },
    // Casting array of type {} to array of type {} is impossible because the element types can't be cast
    ArrayElementsIncompatible,
    // Casting array with length {} to length {} is impossible
    ArrayLengthIncompatible,
    // Casting non-array of type {} to array of type {} is impossible
    ArrayAndNonArray { direction: CastDirection },
    // Casting between structs with equal members that stem from different places is forbidden.\n From {} (source_hash: {a_hash:x?}) to {} (source_hash: {b_hash:x?})
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
pub enum UniquelyNamed {
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
    },
    // {name:?} expects a value of type {} as argument {param_name:?} (Nr. {}). Got {}
    IntrinsicCallParameter {
        parameter_index: usize,
        parameter_name: String,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Backend {
    C,
    Glsl,
    Llvm,
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

use std::cell::RefMut;

use itertools::Itertools;

use crate::{
    ast::{
        AstVisitor, FunctionDefinition, Program, SimpleBinding, StructMemberDefinition, StructType,
        TypeAlias,
    },
    error_reporting::{ErrorKind, Errors, Lint, SymbolDefinition},
    passes::pass_manager::{Pass, PassFactory, PassResult},
};

use super::{partial_visitor::PartialAstVisitor, pass_manager::GlobalState};

pub struct LintVariableNaming<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,
}

impl PassFactory for LintVariableNaming<'_> {
    type Output<'state> = LintVariableNaming<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        Ok(Self::Output {
            errors: state.get_mut_named::<Errors>()?,
            program: Some(state.get_mut_named::<Program>()?),
        })
    }
}
impl Pass for LintVariableNaming<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = &mut self.program.take().unwrap();

        self.visit_program(program);

        Ok(())
    }
}

impl LintVariableNaming<'_> {
    pub fn is_snake_case(name: &str) -> bool {
        !name.chars().any(|c| c.is_uppercase())
            && !name.contains("__")
            // TODO: lint for used variables starting with an underscrore that can have the
            // underscore removed while remaining a valid identifier
            // && !name.starts_with('_')
            && !name.ends_with('_')
    }
    pub fn is_camel_case(name: &str) -> bool {
        Self::words_to_camel_case(Self::split_words(name)) == name
    }
    pub fn split_words(name: &str) -> Vec<String> {
        let mut words = Vec::new();
        let mut current = String::new();

        let mut chars = name.chars().peekable();
        let mut prev: Option<char> = None;
        let mut prev_prev: Option<char> = None;

        enum Boundary {
            Before,
            After,
            None,
        }

        while let Some(c) = chars.next() {
            let next = chars.peek().copied();

            let boundary = match (prev, c, next) {
                // lower -> upper
                (Some(p), c, _) if (p.is_lowercase() || p.is_numeric()) && c.is_uppercase() => {
                    Boundary::Before
                }

                // acronym end: upper -> upper -> lower
                (Some(p), c, Some(n))
                    if p.is_uppercase() && c.is_uppercase() && n.is_lowercase() =>
                {
                    if prev_prev.is_some_and(|pp| pp.is_uppercase()) {
                        Boundary::Before
                    } else {
                        Boundary::After
                    }
                }

                // digit -> lowercase  (AB6d -> ab | 6d)
                (_, c, Some(n)) if c.is_numeric() && n.is_lowercase() => Boundary::Before,
                (_, c, Some(n)) if c.is_numeric() && n.is_uppercase() => Boundary::After,

                _ => Boundary::None,
            };

            match boundary {
                Boundary::Before => {
                    if !current.is_empty() {
                        words.push(current);
                    }
                    current = c.to_lowercase().to_string();
                }
                Boundary::After => {
                    current.extend(c.to_lowercase());
                    words.push(current);
                    current = String::new();
                }
                Boundary::None => {
                    current.extend(c.to_lowercase());
                }
            }

            prev_prev = prev;
            prev = Some(c);
        }

        if !current.is_empty() {
            words.push(current);
        }

        words
    }

    pub fn words_to_snake_case(words: Vec<String>) -> String {
        words.join("_")
    }
    pub fn words_to_camel_case(words: Vec<String>) -> String {
        // approximately follows Googles style guide:
        // https://google.github.io/styleguide/javaguide.html#s5.3-camel-case
        words
            .into_iter()
            .map(|x| {
                let chars = x.chars().collect_vec();
                let [first, rest @ ..] = chars.as_slice() else {
                    // empty or only one char
                    return x.to_uppercase();
                };
                if first.is_numeric() {
                    x
                } else {
                    first.to_uppercase().to_string() + &rest.iter().join("")
                }
            })
            .join("")
    }
}

impl PartialAstVisitor for LintVariableNaming<'_> {
    // may get lifted in the future if lambdas are implemented
    fn partial_visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name,
            name_token,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) {
        if !Self::is_snake_case(name) {
            self.errors
                .push(ErrorKind::Lint(Lint::FunctionNameNotSnakeCase {
                    function: SymbolDefinition::from_token(name.clone(), name_token),
                    suggestion: Self::words_to_snake_case(Self::split_words(name)),
                }));
        }
        for (param, _comma) in parameters {
            self.visit_simple_binding(param);
        }

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }

        self.visit_function_body(body);
    }

    fn partial_visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token,
            name,
            type_,
            id: _,
        }: &mut SimpleBinding,
    ) {
        if !Self::is_snake_case(name) {
            self.errors
                .push(ErrorKind::Lint(Lint::VariableNameNotSnakeCase {
                    variable: SymbolDefinition::from_token(name.clone(), name_token),
                    suggestion: Self::words_to_snake_case(Self::split_words(name)),
                }));
        }

        if let Some((_colon, type_)) = type_ {
            self.visit_type(type_);
        }
    }
    fn partial_visit_type_alias(
        &mut self,
        TypeAlias {
            let_token: _,
            name,
            name_token,
            equal_token: _,
            type_,
            semicolon_token: _,
            id: _,
        }: &mut TypeAlias,
    ) {
        if !Self::is_camel_case(name) {
            self.errors
                .push(ErrorKind::Lint(Lint::TypeNameNotCamelCase {
                    type_: SymbolDefinition::from_token(name.clone(), name_token),
                    suggestion: Self::words_to_camel_case(Self::split_words(name)),
                }));
        }

        self.visit_type(type_);
    }

    fn partial_visit_struct_type(
        &mut self,
        StructType {
            struct_token: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        }: &mut StructType,
    ) {
        for (
            StructMemberDefinition {
                name,
                name_token,
                colon_token: _,
                type_,
            },
            _comma,
        ) in members
        {
            if !Self::is_snake_case(name) {
                self.errors
                    .push(ErrorKind::Lint(Lint::StructMemberNotSnakeCase {
                        member: SymbolDefinition::from_token(name.clone(), name_token),
                        suggestion: Self::words_to_snake_case(Self::split_words(name)),
                    }));
            }

            self.visit_type(type_);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::passes::lint_variable_names::LintVariableNaming;

    #[test]
    fn word_splitting() {
        // The goal is to transform all correct and incorrect cases sensibly. Taken from the Google
        // style guide: https://google.github.io/styleguide/javaguide.html#s5.3-camel-case

        assert_eq!(
            LintVariableNaming::split_words("XmlHttpRequest"),
            vec!["xml", "http", "request"]
        );

        // this one isn't perfect, but without a wordlist, it's the best we can do
        assert_eq!(
            LintVariableNaming::split_words("XMLHTTPRequest"),
            vec!["xmlhttp", "request"]
        );

        assert_eq!(
            LintVariableNaming::split_words("newCustomerId"),
            vec!["new", "customer", "id"]
        );
        assert_eq!(
            LintVariableNaming::split_words("newCustomerID"),
            vec!["new", "customer", "id"]
        );

        assert_eq!(
            LintVariableNaming::split_words("innerStopwatch"),
            vec!["inner", "stopwatch"]
        );
        assert_eq!(
            LintVariableNaming::split_words("innerStopWatch"),
            vec!["inner", "stop", "watch"]
        );

        assert_eq!(
            LintVariableNaming::split_words("supportsIpv6OnIos"),
            vec!["supports", "ipv6", "on", "ios"]
        );
        // acronyms must be at least two characters, so "i_pv6" is invalid
        assert_eq!(
            LintVariableNaming::split_words("supportsIPv6OnIos"),
            vec!["supports", "ip", "v6", "on", "ios"]
        );
        assert_eq!(
            LintVariableNaming::split_words("supportsIPV6OnIos"),
            vec!["supports", "ipv6", "on", "ios"]
        );

        assert_eq!(
            LintVariableNaming::split_words("YouTubeImporter"),
            vec!["you", "tube", "importer"]
        );
        assert_eq!(
            LintVariableNaming::split_words("YoutubeImporter"),
            vec!["youtube", "importer"]
        );

        assert_eq!(
            LintVariableNaming::split_words("turnOn2sv"),
            vec!["turn", "on", "2sv"]
        );
        // would also need a wordlist to detect that 2SV belongs together
        assert_eq!(
            LintVariableNaming::split_words("turnOn2Sv"),
            vec!["turn", "on2", "sv"]
        );
    }

    #[test]
    fn test_words_to_snake_case() {
        assert_eq!(
            LintVariableNaming::words_to_snake_case(vec!["hello".into(), "world".into()]),
            "hello_world"
        );
        assert_eq!(
            LintVariableNaming::words_to_snake_case(vec!["rust".into(), "is".into(), "fun".into()]),
            "rust_is_fun"
        );
        assert_eq!(LintVariableNaming::words_to_snake_case(vec![]), "");
        assert_eq!(
            LintVariableNaming::words_to_snake_case(vec!["single".into()]),
            "single"
        );
    }

    #[test]
    fn test_words_to_camel_case() {
        assert_eq!(
            LintVariableNaming::words_to_camel_case(vec!["hello".into(), "world".into()]),
            "HelloWorld"
        );
        assert_eq!(
            LintVariableNaming::words_to_camel_case(vec!["rust".into(), "is".into(), "fun".into()]),
            "RustIsFun"
        );
        assert_eq!(LintVariableNaming::words_to_camel_case(vec![]), "");
        assert_eq!(
            LintVariableNaming::words_to_camel_case(vec!["1number".into(), "word".into()]),
            "1numberWord"
        );
        assert_eq!(
            LintVariableNaming::words_to_camel_case(vec!["word".into(), "2number".into()]),
            "Word2number"
        );
        assert_eq!(
            LintVariableNaming::words_to_camel_case(vec!["a".into(), "b".into()]),
            "AB"
        );
    }
}

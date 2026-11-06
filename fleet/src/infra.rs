use crate::{
    NewtypeDeref,
    document_model::{DocumentElement, stringify_document},
    error_reporting::{ErrorSeverity, Errors},
    passes::{
        err::{
            limit_lvalues::ErrLimitLValues, missing_type_in_parameter::ErrMissingTypeInParam,
            too_few_iterators::ErrTooFewIterators,
        },
        fix::{non_block_statements::FixNonBlockStatements, trailing_comma::FixTrailingComma},
        lint::{removable_parens::LintRemovableParens, variable_naming::LintVariableNaming},
        misc::store_pass::StorePass,
        out::{ast_to_dm::OutAstToDM, generate_c::OutGenerateC, generate_glsl::OutGenerateGlsl},
        pass_manager::{PassError, PassManager},
        proc::{
            parser::Parser,
            scope_analyzer::ScopeAnalyzer,
            stat_tracker::StatTracker,
            tokenizer::{InputSource, Tokenizer},
            type_checker::TypeChecker,
            type_concretizer::TypeConcretizer,
        },
    },
};

pub fn insert_fix_passes(pm: &mut PassManager) {
    pm.insert::<LintRemovableParens>();
    pm.insert::<FixNonBlockStatements>();
    pm.insert::<FixTrailingComma>();
    pm.insert::<ErrMissingTypeInParam>();
    pm.insert::<ErrTooFewIterators>();
    pm.insert::<LintVariableNaming>();
}

pub fn insert_minimal_pipeline(pm: &mut PassManager) {
    pm.insert::<Tokenizer>();
    pm.insert::<Parser>();
}
pub fn insert_compile_passes(pm: &mut PassManager) {
    pm.insert::<ScopeAnalyzer>();
    pm.insert::<TypeChecker>();
    pm.insert::<TypeConcretizer>();
    pm.insert::<ErrLimitLValues>();
    pm.insert::<StatTracker>();
    pm.insert::<OutGenerateGlsl>();
}

pub fn insert_c_passes(pm: &mut PassManager) {
    pm.insert::<OutGenerateC>();
}

NewtypeDeref!(ParseErrorsOnly, Errors);

pub fn format(source: InputSource) -> Result<String, PassError> {
    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    pm.insert::<StorePass<Errors, ParseErrorsOnly>>();
    insert_fix_passes(&mut pm);
    pm.insert::<OutAstToDM>();

    pm.state.insert(source).unwrap();
    pm.state.insert_default::<Errors>().unwrap();

    pm.run()?;

    if pm
        .state
        .get::<ParseErrorsOnly>()
        .unwrap()
        .iter()
        .any(|err| err.severity() == ErrorSeverity::Error)
    {
        return Err(PassError::InvalidInput {
            producing_pass: "format() function".to_string(),
            source: "Not formatting malformed input".into(),
        });
    }
    let de = pm.state.get::<DocumentElement>().unwrap();

    Ok(stringify_document(de.clone()))
}

use crate::{
    NewtypeDeref,
    ast_to_dm::AstToDocumentModelConverter,
    document_model::{DocumentElement, stringify_document},
    error_reporting::{ErrorSeverity, Errors},
    generate_c::CCodeGenerator,
    generate_glsl::GLSLCodeGenerator,
    parser::Parser,
    passes::{
        err_missing_type_in_parameter::ErrMissingTypeInParam,
        err_too_few_iterators::ErrTooFewIterators,
        fix_non_block_statements::FixNonBlockStatements,
        fix_trailing_comma::FixTrailingComma,
        lvalue_reducer::LValueReducer,
        pass_manager::{InputSource, PassError, PassManager},
        remove_parens::RemoveParensPass,
        scope_analysis::ScopeAnalyzer,
        stat_tracker::StatTracker,
        store_pass::StorePass,
        type_concretisation_pass::TypeConcretisationPass,
        type_propagation::TypePropagator,
    },
    tokenizer::Tokenizer,
};

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

use std::{cell::Ref, path::PathBuf};

#[cfg(feature = "llvm_backend")]
use inkwell::{
    module::Module,
    targets::{FileType, TargetMachine},
};
use log::info;

use crate::passes::pass_manager::{
    CCodeOutput, GlobalState, Pass, PassError, PassFactory, PassResult,
};

#[derive(Copy, Clone, Debug)]
pub enum ArtifactType {
    #[cfg(feature = "llvm_backend")]
    Object,
    #[cfg(feature = "llvm_backend")]
    Assembly,
    #[cfg(feature = "llvm_backend")]
    LlvmIr,
    CCode,
}

pub struct SaveArtifactPass<'state> {
    #[cfg(feature = "llvm_backend")]
    module: Option<Ref<'state, Module<'static>>>,
    #[cfg(feature = "llvm_backend")]
    target_machine: Option<Ref<'state, TargetMachine>>,
    ccode: Option<Ref<'state, CCodeOutput>>,

    filename: PathBuf,
    artifact_type: ArtifactType,
}

impl PassFactory for SaveArtifactPass<'_> {
    type Output<'state> = SaveArtifactPass<'state>;
    type Params = (PathBuf, ArtifactType);

    fn try_new<'state>(
        state: &'state mut GlobalState,
        (filename, artifact_type): Self::Params,
    ) -> ::core::result::Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        Ok(match artifact_type {
            #[cfg(feature = "llvm_backend")]
            ArtifactType::Object => Self::Output {
                module: Some(state.get_named()?),
                target_machine: Some(state.get_named()?),
                ccode: None,

                filename,
                artifact_type,
            },
            #[cfg(feature = "llvm_backend")]
            ArtifactType::Assembly => Self::Output {
                module: Some(state.get_named()?),
                target_machine: Some(state.get_named()?),
                ccode: None,

                filename,
                artifact_type,
            },
            #[cfg(feature = "llvm_backend")]
            ArtifactType::LlvmIr => Self::Output {
                module: Some(state.get_named()?),
                target_machine: None,
                ccode: None,

                filename,
                artifact_type,
            },

            ArtifactType::CCode => Self::Output {
                #[cfg(feature = "llvm_backend")]
                module: None,
                #[cfg(feature = "llvm_backend")]
                target_machine: None,
                ccode: Some(state.get_named()?),

                filename,
                artifact_type,
            },
        })
    }
}
impl Pass for SaveArtifactPass<'_> {
    fn run<'state>(self: Box<Self>) -> PassResult {
        match self.artifact_type {
            #[cfg(feature = "llvm_backend")]
            ArtifactType::Object => self
                .target_machine
                .unwrap()
                .write_to_file(&self.module.unwrap(), FileType::Object, &self.filename)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?,
            #[cfg(feature = "llvm_backend")]
            ArtifactType::Assembly => self
                .target_machine
                .unwrap()
                .write_to_file(&self.module.unwrap(), FileType::Assembly, &self.filename)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?,
            #[cfg(feature = "llvm_backend")]
            ArtifactType::LlvmIr => {
                std::fs::write(&self.filename, self.module.unwrap().to_string()).map_err(|err| {
                    PassError::CompilerError {
                        producing_pass: Self::name(),
                        source: err.into(),
                    }
                })?
            }
            ArtifactType::CCode => std::fs::write(&self.filename, self.ccode.unwrap().to_string())
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?,
        }
        info!("Wrote {:?} to {:?}", self.artifact_type, self.filename);

        Ok(())
    }
}

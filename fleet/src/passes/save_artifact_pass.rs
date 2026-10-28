use std::path::PathBuf;

#[cfg(feature = "llvm_backend")]
use inkwell::{
    module::Module,
    targets::{FileType, TargetMachine},
};
use log::info;

use crate::passes::{
    global_state::{FetchError, Fetched, GlobalState},
    pass_manager::{CCodeOutput, Pass, PassError, PassFactory, PassResult},
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
    module: Option<&'state Module<'static>>,
    #[cfg(feature = "llvm_backend")]
    target_machine: Option<&'state TargetMachine>,
    ccode: Option<&'state CCodeOutput>,

    filename: PathBuf,
    artifact_type: ArtifactType,
}

impl PassFactory for SaveArtifactPass<'_> {
    type Output<'state> = SaveArtifactPass<'state>;
    type Params = (PathBuf, ArtifactType);

    fn try_new<'state>(
        state: &'state mut GlobalState,
        (filename, artifact_type): Self::Params,
    ) -> Result<Self::Output<'state>, FetchError>
    where
        Self: Sized,
    {
        Ok(match artifact_type {
            #[cfg(feature = "llvm_backend")]
            ArtifactType::Object => {
                let Fetched {
                    read: (module, target_machine),
                    write: (),
                    empty: (),
                } = state.fetch()?;

                Self::Output {
                    module: Some(module),
                    target_machine: Some(target_machine),
                    ccode: None,

                    filename,
                    artifact_type,
                }
            }
            #[cfg(feature = "llvm_backend")]
            ArtifactType::Assembly => {
                let Fetched {
                    read: (module, target_machine),
                    write: (),
                    empty: (),
                } = state.fetch()?;

                Self::Output {
                    module: Some(module),
                    target_machine: Some(target_machine),
                    ccode: None,

                    filename,
                    artifact_type,
                }
            }
            #[cfg(feature = "llvm_backend")]
            ArtifactType::LlvmIr => {
                let Fetched {
                    read: (module,),
                    write: (),
                    empty: (),
                } = state.fetch()?;

                Self::Output {
                    module: Some(module),
                    target_machine: None,
                    ccode: None,

                    filename,
                    artifact_type,
                }
            }

            ArtifactType::CCode => {
                let Fetched {
                    read: (ccode,),
                    write: (),
                    empty: (),
                } = state.fetch()?;

                Self::Output {
                    #[cfg(feature = "llvm_backend")]
                    module: None,
                    #[cfg(feature = "llvm_backend")]
                    target_machine: None,
                    ccode: Some(ccode),

                    filename,
                    artifact_type,
                }
            }
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
                .write_to_file(self.module.unwrap(), FileType::Object, &self.filename)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?,
            #[cfg(feature = "llvm_backend")]
            ArtifactType::Assembly => self
                .target_machine
                .unwrap()
                .write_to_file(self.module.unwrap(), FileType::Assembly, &self.filename)
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

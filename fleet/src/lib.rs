#![feature(associated_type_defaults)]

pub mod ast;
pub mod document_model;
pub mod error_reporting;
pub mod escape;
pub mod global_state;
pub mod infra;
pub mod natural_language;
pub mod passes;
pub mod runtime_type;
pub mod union_find_set;

#[cfg(feature = "llvm_backend")]
pub extern crate inkwell;

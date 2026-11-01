#![feature(associated_type_defaults)]

pub mod ast;
pub mod ast_to_dm;
pub mod document_model;
pub mod error_reporting;
pub mod escape;
pub mod generate_c;
pub mod generate_glsl;
pub mod infra;
pub mod natural_language;
pub mod node_data;
pub mod parser;
pub mod passes;
pub mod tokenizer;

#[cfg(feature = "llvm_backend")]
pub mod ir_generator;
#[cfg(feature = "llvm_backend")]
pub extern crate inkwell;

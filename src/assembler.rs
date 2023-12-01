//! Assembles the specified file.
//!
//! Will be completed once I actually fix the assembler.

mod ascii;
mod diagnostic;
mod eval;
mod lex;
mod parse;
mod token;
pub use diagnostic::{Diagnostic, OptionalScream, ResultScream};

use std::sync::OnceLock;

use clap::Args;
use clap_verbosity_flag::{Level, WarnLevel};
use clio::{Input, Output};

#[derive(Debug, Args)]
pub struct AssemblerArgs {
    /// CPU frequency in HZ.
    ///
    /// Assigned to the `CPU_FREQUENCY` variable.
    #[clap(short, long, default_value_t = 500_000)]
    frequency: u64,

    #[clap(value_parser, default_value = "-")]
    input: Input,
    #[clap(value_parser, default_value = "-")]
    output: Output,
}

pub type Errors = Vec<Diagnostic>;

pub static VERBOSITY: OnceLock<Verbosity> = OnceLock::new();

pub async fn assemble(
    _args: AssemblerArgs,
    verbosity: clap_verbosity_flag::Verbosity<WarnLevel>,
) -> Result<(), Errors> {
    let verbose = match verbosity.log_level() {
        Some(level) => match level {
            Level::Error => Verbosity::Error,
            Level::Warn => Verbosity::Warn,
            Level::Info => Verbosity::Help,
            Level::Debug | Level::Trace => Verbosity::Info,
        },
        None => Verbosity::Quiet,
    };
    VERBOSITY.set(verbose).expect("verbosity should be empty");

    todo!()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Verbosity {
    Quiet = 0,
    Error = 1,
    Warn = 2,
    Help = 3,
    Info = 4,
}

impl PartialOrd for Verbosity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (*self as u8).partial_cmp(&(*other as u8))
    }
}

impl Ord for Verbosity {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (*self as u8).cmp(&(*other as u8))
    }
}

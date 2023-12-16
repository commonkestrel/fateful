//! Assembles the specified file.
//!
//! Will be completed once I actually fix the assembler.

mod ascii;
mod assemble;
mod eval;
mod include;
pub mod lex;
mod parse;
mod token;
pub use crate::diagnostic::{Diagnostic, OptionalScream, ResultScream};
use crate::{ note, error };

use colored::Colorize;

use std::{
    time::Instant,
    sync::OnceLock
};

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
    #[clap(short, long, value_parser, default_value = "-")]
    output: Output,
}

pub type Errors = Vec<Diagnostic>;

pub static VERBOSITY: OnceLock<Verbosity> = OnceLock::new();

pub async fn assemble(
    mut args: AssemblerArgs,
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

    let start = Instant::now();
    let input = format!("{}", args.input);

    let lexed = lex::lex(args.input)?;
    let parsed = parse::parse(lexed)?;
    let assembled = assemble::assemble(parsed)?;

    args.output
        .lock()
        .write_all(&assembled)
        .map_err(|err| vec![error!("failed to write to output: {err}")])?;
    args.output
        .finish()
        .map_err(|err| vec![error!("failed to finalize output: {err}")])?;
    
    let elapsed = start.elapsed().as_millis();
    let seconds = elapsed / 1000;
    let millis = elapsed % 1000;
    println!("    {} assembling `{}` in {seconds}.{millis:03}s", "Finished".green().bold(), input.trim_matches('"'));

    Ok(())
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

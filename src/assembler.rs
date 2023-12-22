//! Assembles the specified file.
//!
//! Will be completed once I actually fix the assembler.

mod ascii;
pub mod assemble;
mod eval;
mod include;
pub mod lex;
pub mod parse;
mod token;
pub use crate::diagnostic::{Diagnostic, OptionalScream, ResultScream};
use crate::error;

pub mod tests {
    pub use super::{
        assemble,
        lex::{self, Token, TokenStream},
        parse,
    };
}

use std::time::Instant;

use clap::Args;
use clio::{Input, Output};
use colored::Colorize;

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

pub async fn assemble(mut args: AssemblerArgs) -> Result<(), Errors> {
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
    println!(
        "    {} assembling `{}` in {seconds}.{millis:03}s",
        "Finished".green().bold(),
        input.trim_matches('"')
    );

    Ok(())
}

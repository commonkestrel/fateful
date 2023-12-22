mod emulator;
use std::sync::OnceLock;

use emulator::{EmulatorArgs, EmulatorError};
mod deploy;
use deploy::{DeployArgs, DeployError};
mod assembler;
use assembler::AssemblerArgs;
mod tests;
use tests::TestArgs;

mod diagnostic;

use clap::{Parser, Subcommand};
use clap_verbosity_flag::{Level, WarnLevel};
use shadow_rs::shadow;
use thiserror::Error;

shadow!(build);

/// Enables program creation for the F8ful CPU.
#[derive(Parser, Debug)]
#[command(name = "Fateful", author, version = build::CLAP_LONG_VERSION, about)]
struct Args {
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity<WarnLevel>,

    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Run the F8ful emulator
    Emulate(EmulatorArgs),
    /// Deploy a program to a connected CPU
    Deploy(DeployArgs),
    /// Assemble a Fate program
    Assemble(AssemblerArgs),
    /// Quickly test Fate assembly programs
    Test(TestArgs),
}

#[derive(Debug, Error)]
enum Error {
    #[error(transparent)]
    Emulator(#[from] EmulatorError),
    #[error(transparent)]
    Deploy(#[from] DeployError),
    #[error("assembly failed due to previous errors")]
    Assembler,
    #[error("one of the tests failed")]
    TestFailed,
}

pub static VERBOSITY: OnceLock<Verbosity> = OnceLock::new();

#[async_std::main]
async fn main() -> Result<(), Error> {
    let cli = Args::parse();

    let verbose = match cli.verbose.log_level() {
        Some(level) => match level {
            Level::Error => Verbosity::Error,
            Level::Warn => Verbosity::Warn,
            Level::Info => Verbosity::Help,
            Level::Debug | Level::Trace => Verbosity::Info,
        },
        None => Verbosity::Quiet,
    };
    VERBOSITY.set(verbose).expect("verbosity should be empty");

    match cli.command {
        Command::Emulate(args) => emulator::emulate(args).await?,
        Command::Deploy(args) => deploy::deploy(args).await?,
        Command::Assemble(args) => match assembler::assemble(args).await {
            Ok(_) => {}
            Err(errors) => {
                for err in errors {
                    err.emit();
                }

                error!("assembly failed due to previous errors").emit();

                return Err(Error::Assembler);
            }
        },
        Command::Test(args) => tests::test_all(args).await.map_err(|_| Error::TestFailed)?,
    }

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

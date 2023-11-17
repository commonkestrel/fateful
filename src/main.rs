mod emulator;
use emulator::{EmulatorArgs, EmulatorError};
mod deploy;
use deploy::{DeployArgs, DeployError};
mod assembler;
use assembler::AssemblerArgs;

#[cfg(test)]
mod tests;

use clap::{Parser, Subcommand};
use clap_verbosity_flag::{Level, WarnLevel};
use env_logger::Env;
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
    Emulate(EmulatorArgs),
    Deploy(DeployArgs),
    Assemble(AssemblerArgs),
}

#[derive(Debug, Error)]
enum Error {
    #[error(transparent)]
    Emulator(#[from] EmulatorError),
    #[error(transparent)]
    Deploy(#[from] DeployError),
    #[error("assembly failed due to previous errors")]
    Assembler,
}

#[async_std::main]
async fn main() -> Result<(), Error> {
    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();

    let cli = Args::parse();

    env_logger::Builder::new()
        .filter_level(cli.verbose.log_level_filter())
        .init();

    match cli.command {
        Command::Emulate(args) => emulator::emulate(args).await?,
        Command::Deploy(args) => deploy::deploy(args).await?,
        Command::Assemble(args) => {
            match assembler::assemble(args, cli.verbose).await {
                Ok(_) => {},
                Err(errors) => {
                    for err in errors {
                        err.emit();
                    }
                }
            }
        }
    }

    Ok(())
}

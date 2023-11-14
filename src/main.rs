mod emulator;
use emulator::{EmulatorArgs, EmulatorError};
mod deploy;
use deploy::{DeployArgs, DeployError};
#[cfg(test)]
mod tests;

use clap::{Parser, Subcommand};
use env_logger::Env;
use shadow_rs::shadow;
use thiserror::Error;

shadow!(build);

/// Enables program creation for the F8ful CPU.
#[derive(Parser, Debug)]
#[command(name = "Fateful", author, version = build::CLAP_LONG_VERSION, about)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Emulate(EmulatorArgs),
    Deploy(DeployArgs),
}

#[derive(Debug, Error)]
enum Error {
    #[error(transparent)]
    Emulator(#[from] EmulatorError),
    #[error(transparent)]
    Deploy(#[from] DeployError),
}

#[async_std::main]
async fn main() -> Result<(), Error> {
    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();

    let args = Args::parse();
    match args.command {
        Command::Emulate(args) => emulator::emulate(args).await?,
        Command::Deploy(args) => deploy::deploy(args).await?,
    }

    Ok(())
}

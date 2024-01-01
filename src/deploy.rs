//! Deploys assembled programs to the CPU
//!
//! Will be completed once the protocol is solidified.

use clap::Args;
use thiserror::Error;

#[derive(Debug, Args)]
pub struct DeployArgs {}

#[derive(Debug, Error)]
pub enum DeployError {

}

pub fn deploy(_args: DeployArgs) -> Result<(), DeployError> {
    todo!()
}

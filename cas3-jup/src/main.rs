use std::path::PathBuf;

use clap::{Parser, Subcommand};
use jupyter::{
    InstallAction, JupyterKernelProtocol, JupyterResult, LanguageInfo, OpenAction, StartAction,
    UninstallAction,
};

// #[derive(Debug, Parser)]
// struct Args {}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct JupyterApplication {
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
    #[command(subcommand)]
    command: JupyterCommands,
}

#[derive(Debug, Subcommand)]
enum JupyterCommands {
    Open(Box<OpenAction>),
    Start(Box<StartAction>),
    Install(Box<InstallAction>),
    Uninstall(Box<UninstallAction>),
}

impl JupyterApplication {
    pub fn run<T>(&self, protocol: T) -> JupyterResult<()>
    where
        T: JupyterKernelProtocol + 'static,
    {
        match &self.command {
            JupyterCommands::Open(v) => v.run(),
            JupyterCommands::Start(v) => v.run(protocol),
            JupyterCommands::Install(v) => v.run(protocol),
            JupyterCommands::Uninstall(v) => v.run(protocol),
        }
    }
}

struct Cas3JupyterProtocol {}

impl Cas3JupyterProtocol {
    pub fn new() -> Self {
        todo!()
    }
}

impl JupyterKernelProtocol for Cas3JupyterProtocol {
    fn language_info(&self) -> LanguageInfo {
        LanguageInfo::new("cas3", "cas3")
            .with_file_extensions(".sexp", "text/plain")
            .with_version("0.1.0")
    }
}

fn main() {
    let protocol = Cas3JupyterProtocol::new();
    JupyterApplication::parse().run(protocol);
}

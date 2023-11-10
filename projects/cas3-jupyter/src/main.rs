#![deny(missing_debug_implementations, missing_copy_implementations)]
#![warn(missing_docs, rustdoc::missing_crate_level_docs)]
#![doc = include_str!("../readme.md")]
#![doc(html_logo_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]
#![doc(html_favicon_url = "https://raw.githubusercontent.com/oovm/shape-rs/dev/projects/images/Trapezohedron.svg")]

use std::fmt::{Debug, Formatter};

use std::path::PathBuf;

use crate::executor::ValkyrieExecutor;
use clap::{Parser, Subcommand};
use jupyter::{InstallAction, JupyterResult, OpenAction, StartAction, UninstallAction};
use std::io::Write;
use url::Url;

mod config;
mod executor;
mod protocol;

pub use crate::protocol::display::{DisplayKeywords, DisplayNumber, DisplayText};

///
#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct JupyterApplication {
    /// Sets a custom config file
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,
    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,
    #[command(subcommand)]
    command: JupyterCommands,
}

/// The subcommands of the application
#[derive(Subcommand)]
enum JupyterCommands {
    Open(Box<OpenAction>),
    Start(Box<StartAction>),
    Install(Box<InstallAction>),
    Uninstall(Box<UninstallAction>),
}

impl Debug for JupyterCommands {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        unreachable!("{}", f.alternate())
    }
}

impl JupyterApplication {
    /// Run the application
    pub fn run(&self) -> JupyterResult<()> {
        let config = ValkyrieExecutor::default();
        match &self.command {
            JupyterCommands::Open(v) => v.run(),
            JupyterCommands::Start(v) => v.run(config),
            JupyterCommands::Install(v) => v.run(config),
            JupyterCommands::Uninstall(v) => v.run(config),
        }
    }
}

fn main() -> JupyterResult<()> {
    std::panic::set_hook(Box::new(|info| {
        let mut host_stderr = std::io::stderr().lock();
        match info.location() {
            None => {}
            Some(s) => match Url::from_file_path(s.file()) {
                Ok(o) => {
                    writeln!(host_stderr, "{}:{}:{}", o, s.line(), s.column()).ok();
                }
                Err(_) => {}
            },
        }
    }));
    tracing_subscriber::fmt::init();
    JupyterApplication::parse().run()
}

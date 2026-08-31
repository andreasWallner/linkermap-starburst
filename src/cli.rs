use std::path::PathBuf;

use clap::Parser;
use eyre::Result;

#[derive(Parser, Debug)]
#[command(
    about = "Visualize a linker map file as a pie chart or stdout tree",
    after_help = "Examples:
  linkermap-visualize memory.map                # Output tree to stdout
  linkermap-visualize memory.map -f pie.html    # Output pie chart to pie.html
  linkermap-visualize -x .bss* memory.map       # Exclude .bss sections"
)]
pub struct Cli {
    /// The linker map file to parse
    pub map_file: String,

    /// Debug info file to parse for inliner info
    pub debug_file: Option<String>,

    /// Output tree visualization to stdout (default true, unless -f is given).
    /// Both -s and -f can be given to output both to stdout and file.
    #[arg(short = 's', long)]
    pub stdout: bool,

    /// Output HTML to this file
    #[arg(short = 'f', long, value_name = "FILE")]
    pub html: Option<PathBuf>,

    /// Exclude symbols whose section matches pattern (exact or prefix with *).
    /// May be repeated to exclude multiple sections.
    #[arg(short = 'x', long = "exclude", value_name = "PATTERN")]
    pub exclude: Vec<String>,
}

pub struct Args {
    pub map_file: String,
    pub stdout: bool,
    pub file: Option<PathBuf>,
    pub exclude: Vec<String>,
}

pub fn parse_args() -> Result<Args> {
    let cli = Cli::parse();

    Ok(Args {
        map_file: cli.map_file,
        stdout: cli.stdout || cli.html.is_none(),
        html: cli.html,
        exclude: cli.exclude,
    })
}

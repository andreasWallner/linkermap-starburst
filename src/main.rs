mod cli;

use eyre::Result;
use linkermap_visualize::{parse_file, pie_chart, stdout};
use std::fs::File;

fn main() -> Result<()> {
    let args = cli::parse_args()?;

    let file = File::open(&args.map_file)?;
    let tree = parse_file(file, &args.exclude)?;

    if args.stdout {
        stdout::visualize_stdout(&tree);
    }
    if let Some(outfile) = args.file {
        pie_chart::visualize(&outfile, &tree)?;
    }
    Ok(())
}

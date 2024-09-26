# Linker-Map Starburst Visualizer

*WARNING* THIS SW IS IN ALPHA STATE

Takes a linker map output from Rust compilation and creates a Starburst graph
using the D3 Javascript library.

# How to use

Either run it directly:

    cargo run --release -- <path to linker map>

or install it, and run the installed

    cargo install
    linkermap-visualize <path to linker map>

In both cases the graphic is output as `pie.html`.

# How to get a map file

To get a map file, you'll have to instruct the linker to output one. The currently
recommended way is to emit the needed setting from your `build.rs`, e.g.:

    use std::env;
    use std::fs;
    use std::path::PathBuf;

    fn main() {
        let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

        // other stuff like copying/setting a linker map

        // output map file
        println!(
            "cargo:rustc-link-arg=-Map={}",
            out_dir.join("memory.map").to_string_lossy()
        );

        println!("cargo:rerun-if-changed=build.rs");
    }


# Inspirations

Kudos to these projects that I used as inspiration

- https://observablehq.com/@d3/zoomable-sunburst?intent=fork
- https://observablehq.com/@kerryrodden/sequences-sunburst
- https://observablehq.com/@kerryrodden/sequences-icicle
- https://www.meriac.com/projects/tfm-explorer

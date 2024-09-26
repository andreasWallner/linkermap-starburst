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

# Inspirations

Kudos to these projects that I used as inspiration

- https://observablehq.com/@d3/zoomable-sunburst?intent=fork
- https://observablehq.com/@kerryrodden/sequences-sunburst
- https://observablehq.com/@kerryrodden/sequences-icicle
- https://www.meriac.com/projects/tfm-explorer

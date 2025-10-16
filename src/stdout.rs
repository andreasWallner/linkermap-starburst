use crate::{Hierarchy, Result, parse_file};
use std::fs::File;

// ANSI color codes
const GREY: &str = "\x1b[90m";
const CYAN: &str = "\x1b[96m"; // Light cyan for leaf nodes
const YELLOW: &str = "\x1b[93m"; // Light yellow for module names
const RESET: &str = "\x1b[0m";

pub fn visualize_stdout(filename: &str) -> Result<()> {
    let file = File::open(filename)?;

    let tree = parse_file(file)?;
    print_tree(&tree, "", true);

    Ok(())
}

fn format_bytes(bytes: u64) -> String {
    let s = bytes.to_string();
    let chars: Vec<char> = s.chars().collect();
    let mut result = String::new();

    for (i, ch) in chars.iter().enumerate() {
        if i > 0 && (chars.len() - i).is_multiple_of(3) {
            result.push(',');
        }
        result.push(*ch);
    }

    result
}

fn print_tree(hierarchy: &Hierarchy, prefix: &str, is_last: bool) {
    // Print current node
    let connector = if is_last { "└── " } else { "├── " };
    let size_bytes = hierarchy.size();
    let formatted_size = format_bytes(size_bytes);

    if hierarchy.name.is_empty() {
        println!("Root {}({} bytes){}", GREY, formatted_size, RESET);
    } else {
        println!(
            "{}{}{}{} {}({} bytes){}",
            prefix, connector, YELLOW, hierarchy.name, GREY, formatted_size, RESET
        );
    }

    // Calculate new prefix for children
    let new_prefix = if hierarchy.name.is_empty() {
        prefix.to_string()
    } else {
        format!("{}{}", prefix, if is_last { "    " } else { "│   " })
    };

    // Collect and sort children by size (largest first)
    let mut children: Vec<_> = hierarchy.sublevels.values().collect();
    children.sort_by_key(|child| std::cmp::Reverse(child.size()));

    // Print sublevels
    for (i, child) in children.iter().enumerate() {
        let is_last_child = i == children.len() - 1 && hierarchy.symbols.is_empty();
        print_tree(child, &new_prefix, is_last_child);
    }

    // Print symbols (if any) - sorted by size
    if !hierarchy.symbols.is_empty() {
        let mut symbols = hierarchy.symbols.clone();
        symbols.sort_by(|a, b| b.size.cmp(&a.size));

        for (i, symbol) in symbols.iter().enumerate() {
            let is_last_symbol = i == symbols.len() - 1;
            let connector = if is_last_symbol {
                "└── "
            } else {
                "├── "
            };

            // Include section name if it's not empty
            let section_info = if !symbol.section.is_empty() {
                format!(" [{}]", symbol.section)
            } else {
                String::new()
            };

            let formatted_size = format_bytes(symbol.size);
            println!(
                "{}{}{}{} {}({} bytes){}{}",
                new_prefix, connector, CYAN, symbol.name, GREY, formatted_size, section_info, RESET
            );
        }
    }
}

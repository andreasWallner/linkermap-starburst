mod error;
mod pie_chart;
mod stdout;
use error::Result;
use eyre::eyre;
mod parser;
use parser::{parse, Addressed, Data, Line};

use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead},
};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Symbol {
    pub vma: u64,
    pub lma: u64,
    pub size: u64,
    pub align: u64,
    pub module: Vec<String>,
    pub name: String,
    pub section: String,
    pub filename: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Hierarchy {
    pub name: String,
    pub symbols: Vec<Symbol>,
    pub sublevels: HashMap<String, Hierarchy>,
}

impl Hierarchy {
    pub fn add(&mut self, path: &[String], symbol: Symbol) {
        fn recurse(s: &mut Hierarchy, path: &[String], symbol: Symbol) {
            let [item, remaining @ ..] = path else {
                s.symbols.push(symbol);
                return;
            };
            let subsection = s.sublevels.entry((*item).to_owned()).or_insert(Hierarchy {
                name: (*item).to_owned(),
                symbols: vec![],
                sublevels: HashMap::new(),
            });

            recurse(subsection, remaining, symbol);
        }

        recurse(self, path, symbol);
    }

    pub fn size(&self) -> u64 {
        self.symbols.iter().map(|s| s.size).sum::<u64>()
            + self.sublevels.values().map(|s| s.size()).sum::<u64>()
    }
}

pub fn find_closing_bracket(text: &str, start: usize) -> Option<usize> {
    match text.chars().nth(start) {
        Some('<') => (),
        _ => return None,
    }

    let mut additional_open_brackets = 0;

    for (n, c) in text[start + 1..].char_indices() {
        match c {
            '<' => additional_open_brackets += 1,
            '>' if additional_open_brackets == 0 => return Some(n + start + 1),
            '>' => additional_open_brackets -= 1,
            _ => (),
        }
    }
    None
}

pub fn split_name(n: &str) -> Result<(Vec<String>, String)> {
    // Check if we have an impl block anywhere in the path
    if let Some(impl_start) = n.find("_<") {
        // Find the matching closing bracket for the impl block
        let closing_index = find_closing_bracket(n, impl_start + 1)
            .ok_or_else(|| eyre!("Name with unexpected shape, no closing: {n}"))?;

        // Split the path before the impl block
        let before_impl = &n[..impl_start];
        let impl_block = &n[impl_start..=closing_index];
        let after_impl = &n[closing_index + 1..];

        // Parse the parts before the impl block
        let mut module_parts: Vec<String> = if before_impl.is_empty() {
            Vec::new()
        } else {
            before_impl
                .trim_end_matches("::")
                .split("::")
                .map(|s| s.to_owned())
                .collect()
        };

        // Add the entire impl block as a single module component
        module_parts.push(impl_block.to_owned());

        // Parse the parts after the impl block to get the function name
        let after_parts = after_impl
            .trim_start_matches("::")
            .split("::")
            .collect::<Vec<_>>();
        let function_name = if after_parts.len() >= 2 {
            // Usually the pattern is ::function_name::hash, so take the first part after ::
            after_parts[0].to_owned()
        } else if !after_parts.is_empty() {
            after_parts[0].to_owned()
        } else {
            "unknown".to_owned()
        };

        Ok((module_parts, function_name))
    } else {
        // normal symbol
        // e.g. nci::comm::packets::Packetizer::get_mut::panic_cold_explicit::h84576c2c34ef900f

        let parts = n.split("::").collect::<Vec<_>>();
        Ok(if let [module @ .., name, _hash] = &parts[..] {
            (
                module.iter().map(|&s| s.to_owned()).collect(),
                (*name).to_owned(),
            )
        } else {
            (Vec::new(), n.to_owned())
        })
    }
}

pub fn split_sections(symbol: &Symbol) -> Vec<String> {
    symbol.section.split('.').map(|s| s.to_owned()).collect()
}
pub fn split_modules(symbol: &Symbol) -> Vec<String> {
    symbol.module.clone()
}
#[allow(clippy::ptr_arg)]
pub fn unescape_name(name: &String) -> String {
    // TODO fix handling of all `$u` escapes
    name.replace("$LT$", "<")
        .replace("$GT$", ">")
        .replace("..", "::")
        .replace("$LP$", "(")
        .replace("$RP$", ")")
        .replace("$u20$", " ")
        .replace("$u7b$", "{")
        .replace("$u7d$", "}")
        .replace("$u5b$", "[")
        .replace("$u5d$", "]")
        .replace("$u3b$", ";")
        .replace("$C$", ",")
        .replace("$RF$", "&")
}

fn parse_file(file: File) -> Result<Hierarchy> {
    let mut tree = Hierarchy::default();
    let mut filename = "".to_owned();
    let mut section = "".to_owned();
    let mut result = vec![];

    for l in io::BufReader::new(file).lines() {
        let l = l?;

        match parse(&l)? {
            Line::AddressedSymbol(Addressed {
                entry: Data::Section(s),
                ..
            }) => section = s,
            Line::AddressedSymbol(Addressed {
                entry: Data::File(f),
                ..
            }) => filename = f,
            Line::AddressedSymbol(Addressed {
                vma,
                lma,
                size,
                align,
                entry: Data::Symbol(ref id),
            }) => {
                if id.starts_with(".L") || id.starts_with('$') {
                    // ignore local symbols
                    continue;
                }
                let (module, name) = split_name(&unescape_name(id))?;
                let symbol = Symbol {
                    vma,
                    lma,
                    size,
                    align,
                    module,
                    name,
                    section: section.clone(),
                    filename: filename.clone(),
                };
                result.push(symbol.clone());
                tree.add(&split_modules(&symbol), symbol);
            }
            _ => {}
        }
    }
    Ok(tree)
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        2 => {
            // Default behavior: output to file
            pie_chart::visualize(&args[1])
        }
        3 => {
            match args[1].as_str() {
                "--stdout" | "-s" => {
                    // Output tree to stdout
                    stdout::visualize_stdout(&args[2])
                }
                "--file" | "-f" => {
                    // Output to file (explicit)
                    pie_chart::visualize(&args[2])
                }
                _ => {
                    print_usage(&args[0]);
                    std::process::exit(-1);
                }
            }
        }
        _ => {
            print_usage(&args[0]);
            std::process::exit(-1);
        }
    }
}

fn print_usage(program_name: &str) {
    eprintln!("Usage: {} [OPTIONS] <map_file>", program_name);
    eprintln!("Options:");
    eprintln!("  -s, --stdout    Output tree visualization to stdout");
    eprintln!("  -f, --file      Output HTML plot to file (default)");
    eprintln!("Examples:");
    eprintln!(
        "  {} memory.map           # Output HTML to pie.html",
        program_name
    );
    eprintln!(
        "  {} --stdout memory.map  # Output tree to stdout",
        program_name
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_split_name_unescape() {
        // test adding parsed "   38c58    38c58       6e     1                 nci::messages::common::_$LT$impl$u20$core..convert..TryFrom$LT$nci..messages..common..Bitrate$GT$$u20$for$u20$iso14443..Bitrate$GT$::try_from::hddb4e9709a7a10d4"
        let name = "nci::messages::common::_$LT$impl$u20$core..convert..TryFrom$LT$nci..messages..common..Bitrate$GT$$u20$for$u20$iso14443..Bitrate$GT$::try_from::hddb4e9709a7a10d4".to_owned();
        let (modules, func) = split_name(&unescape_name(&name)).unwrap();
        assert_eq!(
            modules,
            vec![
                "nci",
                "messages",
                "common",
                "_<impl core::convert::TryFrom<nci::messages::common::Bitrate> for iso14443::Bitrate>"
            ]
            .iter().map(|s| s.to_string())
            .collect::<Vec<_>>()
        );
        assert_eq!(func, "try_from");
    }
}

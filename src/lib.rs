pub mod demangle;
mod error;
mod parser;
pub mod pie_chart;
mod split;
pub mod stdout;
pub mod util;

use crate::{
    parser::{Addressed, Data, Line, parse},
    split::split_module,
};
use error::Result;
use eyre::WrapErr;

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
    pub line: String,
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

pub fn matches_section_pattern(section: &str, pattern: &str) -> bool {
    if let Some(prefix) = pattern.strip_suffix('*') {
        section.starts_with(prefix)
    } else {
        section == pattern
    }
}

pub fn parse_file(file: File, exclude_sections: &[String]) -> Result<Hierarchy> {
    let mut tree = Hierarchy::default();
    let mut filename = "".to_owned();
    let mut section = "".to_owned();
    let mut result = vec![];

    for (line_idx, l) in io::BufReader::new(file).lines().enumerate() {
        let l = l?;

        match parse(&l).wrap_err_with(|| format!("parse error at line {}", line_idx + 1))? {
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
                let symbol = match split::recognize(id)? {
                    split::Symbol::FreeStanding {
                        module,
                        function,
                        generics,
                    } if generics.is_none() => Symbol {
                        vma,
                        lma,
                        size,
                        align,
                        module: split_module(module)?
                            .into_iter()
                            .map(|s| s.to_owned())
                            .collect(),
                        name: function.to_owned(),
                        section: section.clone(),
                        filename: filename.clone(),
                        line: l,
                    },
                    split::Symbol::FreeStanding {
                        module,
                        function,
                        generics,
                    } => Symbol {
                        vma,
                        lma,
                        size,
                        align,
                        module: split_module(module)?
                            .into_iter()
                            .map(|s| s.to_owned())
                            .collect(),
                        name: format!("{}::{}", function, generics.unwrap()),
                        section: section.clone(),
                        filename: filename.clone(),
                        line: l,
                    },
                    split::Symbol::Method {
                        module,
                        ty,
                        function,
                    } => Symbol {
                        vma,
                        lma,
                        size,
                        align,
                        module: split_module(module)?
                            .into_iter()
                            .map(|s| s.to_owned())
                            .collect(),
                        name: format!("{}::{}", ty, function),
                        section: section.clone(),
                        filename: filename.clone(),
                        line: l,
                    },
                    split::Symbol::Impl {
                        ty,
                        trait_,
                        function,
                    } => Symbol {
                        vma,
                        lma,
                        size,
                        align,
                        module: split_module(ty)?
                            .into_iter()
                            .map(|s| s.to_owned())
                            .collect(),
                        name: format!("impl {}::{}", trait_, function),
                        section: section.clone(),
                        filename: filename.clone(),
                        line: l,
                    },
                };

                if exclude_sections
                    .iter()
                    .any(|p| matches_section_pattern(&symbol.section, p))
                {
                    continue;
                }
                result.push(symbol.clone());

                tree.add(&symbol.module.clone(), symbol);
            }
            _ => {}
        }
    }
    Ok(tree)
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert2::assert;

    #[test]
    fn test_matches_section_pattern() {
        // Exact match
        assert!(matches_section_pattern(".bss", ".bss"));
        assert!(!matches_section_pattern(".bss.foo", ".bss"));
        assert!(!matches_section_pattern(".text", ".bss"));

        // Prefix wildcard
        assert!(matches_section_pattern(".bss", ".bss*"));
        assert!(matches_section_pattern(".bss.foo", ".bss*"));
        assert!(matches_section_pattern(".bss_data", ".bss*"));
        assert!(!matches_section_pattern(".text", ".bss*"));
        assert!(!matches_section_pattern(".data", ".bss*"));

        // Wildcard at end should not match unrelated sections
        assert!(!matches_section_pattern(".text.bss", ".bss*"));
    }
}

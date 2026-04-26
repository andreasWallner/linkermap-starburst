mod error;
mod parser;
pub mod pie_chart;
pub mod stdout;

use crate::parser::{Addressed, Data, Line, parse};
use error::Result;
use eyre::{WrapErr, eyre};

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

/// Split name of trait impls
///
/// Example: _<nci::messages::common::ParameterId as num_enum::TryFromPrimitive>::try_from_primitive::h86bf34f9546266ff
fn split_pure_trait_impl(n: &str) -> Result<(Vec<String>, String)> {
    assert!(
        n.starts_with("_<") && n.contains(" as "),
        "Invalid input for split_trait_impl: {n}"
    );

    let closing_idx = find_closing_bracket(n, 1)
        .ok_or_else(|| eyre!("Name with unexpected shape, no closing: {n}"))?;
    let inner = &n[2..closing_idx];
    let after = &n[closing_idx + 1..];

    let Some((type_, _trait)) = inner.split_once(" as ") else {
        return Err(eyre!("Name with unexpected shape, no 'as' part: {n}"));
    };

    let type_parts = type_.split("::").map(|s| s.to_owned()).collect();

    // Usually the pattern is ::function_name::hash, so take the first part after ::
    let func_name = match after
        .trim_start_matches("::")
        .split("::")
        .collect::<Vec<_>>()
        .as_slice()
    {
        [func, _hash] => func.to_owned(),
        _ => return Err(eyre!("Name with unexpected function part: {n}")),
    };

    Ok((type_parts, func_name.to_owned()))
}

/// Split name of generic trait impls
///
/// Example: nci::messages::common::_<impl core::convert::TryFrom<nci::messages::common::Bitrate> for iso14443::Bitrate>::try_from::hddb4e9709a7a10d4
fn split_generic_trait_impl(n: &str) -> Result<(Vec<String>, String)> {
    assert!(
        n.contains("_<impl ") && n.contains(" for "),
        "Invalid input for split_generic_trait_impl: {n}"
    );

    let impl_start = n.find("_<impl ").unwrap();
    let closing_idx = find_closing_bracket(n, impl_start + 1)
        .ok_or_else(|| eyre!("Name with unexpected shape, no closing: {n}"))?;
    let before = &n[..impl_start];
    let inner = &n[impl_start + 6..closing_idx]; // Skip "_<impl "
    let after = &n[closing_idx + 1..];

    let module = before
        .trim_end_matches("::")
        .split("::")
        .filter(|s| !s.is_empty())
        .map(|s| s.to_owned())
        .collect();

    let Some((trait_, type_)) = inner.split_once(" for ") else {
        return Err(eyre!("Name with unexpected shape, no 'for' part: {n}"));
    };
    // Usually the pattern is ::function_name::hash, so take the first part after ::
    let func_name = match after
        .trim_start_matches("::")
        .split("::")
        .collect::<Vec<_>>()
        .as_slice()
    {
        [func, _hash] => func.to_owned(),
        _ => return Err(eyre!("Name with unexpected shape, no function part: {n}")),
    };

    Ok((module, format!("{trait_}::{func_name} for {type_}")))
}

/// Split normal symbol names, which are just module paths and function names
///
/// Example: nci::comm::nci_comm::NciComm<T,D,C>::run::hf2c210bafa76e014
fn split_normal_name(n: &str) -> Result<(Vec<String>, String)> {
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

pub fn split_name(n: &str) -> Result<(Vec<String>, String)> {
    if n.contains("_<impl") && n.contains(" for ") {
        split_generic_trait_impl(n)
    } else if n.starts_with("_<") && n.contains(" as ") {
        split_pure_trait_impl(n)
    } else {
        split_normal_name(n)
    }
}

pub fn split_sections(symbol: &Symbol) -> Vec<String> {
    symbol.section.split('.').map(|s| s.to_owned()).collect()
}

pub fn unescape_name(input: &str) -> String {
    if input.is_empty() {
        return String::new();
    }

    let mut result = String::with_capacity(input.len() * 2); // Generous capacity
    let bytes = input.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        let remaining = &bytes[i..];

        // Try pattern matching on the remaining slice, returning replacement string and consumed bytes
        let (replacement, consumed) = match remaining {
            [b'$', b'u', ..] => {
                if let Some((unicode_char, pattern_len)) = parse_unicode_escape(remaining) {
                    result.push(unicode_char);
                    i += pattern_len;
                    continue; // Skip the normal handling since we already pushed and incremented
                } else {
                    (None, 0)
                }
            }
            [b'.', b'.', ..] => (Some("::"), 2),
            [b'$', b'L', b'T', b'$', ..] => (Some("<"), 4),
            [b'$', b'G', b'T', b'$', ..] => (Some(">"), 4),
            [b'$', b'L', b'P', b'$', ..] => (Some("("), 4),
            [b'$', b'R', b'P', b'$', ..] => (Some(")"), 4),
            [b'$', b'C', b'$', ..] => (Some(","), 3),
            [b'$', b'R', b'F', b'$', ..] => (Some("&"), 4),
            _ => (None, 0),
        };

        // Handle the replacement if we found a match
        if let Some(repl) = replacement {
            result.push_str(repl);
            i += consumed;
        } else {
            // If no pattern matched, handle as regular character
            match remaining {
                [byte, ..] if byte.is_ascii() => {
                    result.push(*byte as char);
                    i += 1;
                }
                _ => {
                    // Handle multi-byte UTF-8 characters
                    let remaining_str = &input[i..];
                    let ch = remaining_str.chars().next().unwrap();
                    result.push(ch);
                    i += ch.len_utf8();
                }
            }
        }
    }

    result
}

/// Parse a $u..$ unicode escape sequence and return the character and pattern length
/// Pattern: $u followed by hex digits followed by $
/// Examples: $u20$ -> ' ', $u7b$ -> '{', $u3b$ -> ';'
pub fn parse_unicode_escape(bytes: &[u8]) -> Option<(char, usize)> {
    // Must start with $u
    if bytes.len() < 4 || bytes[0] != b'$' || bytes[1] != b'u' {
        return None;
    }

    let mut hex_end = 2;
    // Find hex digits after $u
    while hex_end < bytes.len() && bytes[hex_end].is_ascii_hexdigit() {
        hex_end += 1;
    }

    // Must end with $ and have at least one hex digit
    if hex_end == 2 || hex_end >= bytes.len() || bytes[hex_end] != b'$' {
        return None;
    }

    // Parse hex digits
    let hex_str = std::str::from_utf8(&bytes[2..hex_end]).ok()?;
    let code_point = u32::from_str_radix(hex_str, 16).ok()?;

    // Convert to char if valid Unicode
    let unicode_char = char::from_u32(code_point)?;

    Some((unicode_char, hex_end + 1)) // +1 to include the closing $
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
                let (module, name) = split_name(&unescape_name(id))?;
                println!("{} -> {module:?} -- {name:?}", unescape_name(id));
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

    #[test]
    fn test_split_name_generic_impl() {
        let name = "nci::messages::common::_<impl core::convert::TryFrom<nci::messages::common::Bitrate> for iso14443::Bitrate>::try_from::hddb4e9709a7a10d4";
        let (modules, func) = split_name(&name).unwrap();
        assert!(modules == ["nci", "messages", "common",]);
        assert_eq!(
            func,
            "core::convert::TryFrom<nci::messages::common::Bitrate>::try_from for iso14443::Bitrate"
        );
    }

    #[test]
    fn test_split_name_pure_impl() {
        let name = "_<nci::messages::common::ParameterId as num_enum::TryFromPrimitive>::try_from_primitive::h86bf34f9546266ff";
        let (modules, func) = split_name(name).unwrap();
        assert!(modules == ["nci", "messages", "common", "ParameterId"]);
        assert!(func == "try_from_primitive");
    }

    #[test]
    fn test_split_name_normal() {
        let name = "nci::comm::nci_comm::NciComm<T,D,C>::run::hf2c210bafa76e014";
        let (modules, func) = split_name(name).unwrap();
        assert!(modules == ["nci", "comm", "nci_comm", "NciComm<T,D,C>"]);
        assert!(func == "run");
    }

    #[test]
    fn test_unescape_name() {
        // Test individual replacements
        assert_eq!(unescape_name("$LT$hello$GT$"), "<hello>");
        assert_eq!(unescape_name("$LP$test$RP$"), "(test)");
        assert_eq!(unescape_name("core..convert"), "core::convert");

        // Test programmatic $u..$ patterns
        assert_eq!(unescape_name("$u20$space$u20$"), " space ");
        assert_eq!(unescape_name("$u7b$hello$u7d$"), "{hello}");
        assert_eq!(unescape_name("$u5b$test$u5d$"), "[test]");
        assert_eq!(unescape_name("$u3b$semicolon"), ";semicolon");

        // Test complex example
        let input = "core..convert..TryFrom$LT$nci..messages..Bitrate$GT$$u20$for$u20$iso14443";
        let expected = "core::convert::TryFrom<nci::messages::Bitrate> for iso14443";
        assert_eq!(unescape_name(input), expected);

        // Test no replacements needed
        assert_eq!(unescape_name("simple_name"), "simple_name");

        // Test empty string
        assert_eq!(unescape_name(""), "");

        // Test mixed fixed and unicode escapes
        assert_eq!(unescape_name("$LT$$u20$$GT$"), "< >");

        // Test edge cases for unicode parsing
        assert_eq!(unescape_name("$u41$"), "A"); // ASCII 'A'
        assert_eq!(unescape_name("$u0$"), "\0"); // null character
        assert_eq!(unescape_name("$ux$"), "$ux$"); // invalid hex should be left as-is
        assert_eq!(unescape_name("$u20"), "$u20"); // missing closing $ should be left as-is
    }

    #[test]
    fn test_parse_unicode_escape() {
        // Test valid patterns
        assert_eq!(parse_unicode_escape(b"$u20$"), Some((' ', 5)));
        assert_eq!(parse_unicode_escape(b"$u7b$"), Some(('{', 5)));
        assert_eq!(parse_unicode_escape(b"$u7d$"), Some(('}', 5)));
        assert_eq!(parse_unicode_escape(b"$u5b$"), Some(('[', 5)));
        assert_eq!(parse_unicode_escape(b"$u5d$"), Some((']', 5)));
        assert_eq!(parse_unicode_escape(b"$u3b$"), Some((';', 5)));
        assert_eq!(parse_unicode_escape(b"$u41$"), Some(('A', 5)));

        // Test longer hex codes
        assert_eq!(parse_unicode_escape(b"$u1234$"), Some(('\u{1234}', 7)));

        // Test invalid patterns
        assert_eq!(parse_unicode_escape(b"$x20$"), None); // wrong prefix
        assert_eq!(parse_unicode_escape(b"$u20"), None); // missing closing $
        assert_eq!(parse_unicode_escape(b"$u$"), None); // no hex digits
        assert_eq!(parse_unicode_escape(b"$ux$"), None); // invalid hex
        assert_eq!(parse_unicode_escape(b"$u"), None); // too short

        // Test invalid Unicode code points
        assert_eq!(parse_unicode_escape(b"$u110000$"), None); // beyond Unicode range
    }
}

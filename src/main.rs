mod cli;
mod error;
mod pie_chart;
mod stdout;
use error::Result;
use eyre::{WrapErr, eyre};
mod parser;
use parser::{Addressed, Data, Line, parse};

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

fn unescape_name(input: &str) -> String {
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
fn parse_unicode_escape(bytes: &[u8]) -> Option<(char, usize)> {
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

fn matches_section_pattern(section: &str, pattern: &str) -> bool {
    if let Some(prefix) = pattern.strip_suffix('*') {
        section.starts_with(prefix)
    } else {
        section == pattern
    }
}

fn parse_file(file: File, exclude_sections: &[String]) -> Result<Hierarchy> {
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
                tree.add(&split_modules(&symbol), symbol);
            }
            _ => {}
        }
    }
    Ok(tree)
}

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

#[cfg(test)]
mod tests {
    use super::*;
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

mod error;

use error::Result;
use eyre::{eyre, OptionExt};

use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead},
};

use nom::{
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{hex_digit1, space0, space1},
    IResult,
};
use serde::{Deserialize, Serialize};
use tera::{Context, Tera};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Line {
    Headline,
    ProvidedSymbol { vma: u64, lma: u64, text: String },
    AddressedSymbol(Addressed),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Addressed {
    pub vma: u64,
    pub lma: u64,
    pub size: u64,
    pub align: u64,
    pub entry: Data,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Data {
    Section(String),
    File(String),
    Symbol(String),
    Absolute(String),
    Relative(String),
    Align(usize),
    Empty,
}

fn parse(s: String) -> Result<Line> {
    if s.trim_start_matches(' ').starts_with("VMA") {
        return Ok(Line::Headline);
    }

    match parse_line(&s) {
        Ok((_, line)) => Ok(line),
        Err(_) => panic!("Failed to parse line: {}", s),
    }
}

fn parse_hex(input: &str) -> IResult<&str, u64> {
    let (input, hex_str) = hex_digit1(input)?;
    let value = u64::from_str_radix(hex_str, 16).map_err(|_| {
        nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::MapRes))
    })?;
    Ok((input, value))
}

// Parse exactly N spaces
fn parse_spaces(n: usize) -> impl Fn(&str) -> IResult<&str, &str> {
    move |input| {
        if input.len() >= n && input[..n].chars().all(|c| c == ' ') {
            Ok((&input[n..], &input[..n]))
        } else {
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            )))
        }
    }
}

// Parse PROVIDE symbol content - handles nested parentheses
fn parse_provide_content(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("PROVIDE ( ")(input)?;

    // Find the matching closing " )" by counting parentheses
    let mut paren_count = 0;
    let mut end_pos = None;

    for (i, c) in input.char_indices() {
        match c {
            '(' => paren_count += 1,
            ')' => {
                if paren_count == 0 && i > 0 && input.chars().nth(i - 1) == Some(' ') {
                    end_pos = Some(i - 1);
                    break;
                }
                paren_count -= 1;
            }
            _ => {}
        }
    }

    if let Some(pos) = end_pos {
        let content = &input[..pos];
        let remaining = &input[pos + 2..]; // Skip " )"
        Ok((remaining, content))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::TakeUntil,
        )))
    }
}

// Parse ALIGN construct
fn parse_align_construct(input: &str) -> IResult<&str, usize> {
    let (input, _) = tag(". = ALIGN ( ")(input)?;
    let (input, num_str) = take_while1(|c: char| c.is_ascii_digit())(input)?;
    let (input, _) = tag(" )")(input)?;
    let value = num_str.parse().map_err(|_| {
        nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::MapRes))
    })?;
    Ok((input, value))
}

// Parse ABSOLUTE construct
fn parse_absolute_construct(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag(". = ABSOLUTE ( ")(input)?;
    let (input, content) = take_until(" )")(input)?;
    let (input, _) = tag(" )")(input)?;
    Ok((input, content))
}

// Parse relative construct (. += )
fn parse_relative_construct(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag(". += ")(input)?;
    let (input, content) = take_while1(|c: char| !c.is_whitespace())(input)?;
    Ok((input, content))
}

// Parse assignment (symbol = .)
fn parse_assignment(input: &str) -> IResult<&str, &str> {
    let (input, name) = take_until(" = .")(input)?;
    let (input, _) = tag(" = .")(input)?;
    Ok((input, name))
}

fn parse_line(input: &str) -> IResult<&str, Line> {
    let (input, _) = space0(input)?;
    let (input, vma) = parse_hex(input)?;
    let (input, _) = space1(input)?;
    let (input, lma) = parse_hex(input)?;
    let (input, _) = space1(input)?;
    let (input, size) = parse_hex(input)?;
    let (input, _) = space1(input)?;
    let (input, align) = parse_hex(input)?;
    // Consume one space after align (minimal required), but leave the rest for indented_entry
    let (input, _) = if let Some(stripped) = input.strip_prefix(' ') {
        (stripped, ' ')
    } else {
        (input, ' ')
    };
    let (remaining, indented_entry) = ("", input);

    let entry = indented_entry.trim_matches(' ');

    // Handle PROVIDE symbols at the top level
    if let Ok((_, content)) = parse_provide_content(entry) {
        return Ok((
            remaining,
            Line::ProvidedSymbol {
                vma,
                lma,
                text: content.to_owned(),
            },
        ));
    }

    // Handle assignment symbols like "_sdata = ."
    if let Ok((_, name)) = parse_assignment(entry) {
        return Ok((
            remaining,
            Line::ProvidedSymbol {
                vma,
                lma,
                text: name.to_owned(),
            },
        ));
    }

    let data = if entry.is_empty() {
        Data::Empty
    } else if parse_spaces(16)(indented_entry).is_ok() {
        // 16 spaces - symbol level
        Data::Symbol(entry.to_owned())
    } else if parse_spaces(8)(indented_entry).is_ok() {
        // 8 spaces - file level or special constructs
        if let Ok((_, val)) = parse_align_construct(entry) {
            Data::Align(val)
        } else if let Ok((_, content)) = parse_absolute_construct(entry) {
            Data::Absolute(content.to_owned())
        } else if let Ok((_, content)) = parse_relative_construct(entry) {
            Data::Relative(content.to_owned())
        } else if let Ok((_, name)) = parse_assignment(entry) {
            return Ok((
                remaining,
                Line::ProvidedSymbol {
                    vma,
                    lma,
                    text: name.to_owned(),
                },
            ));
        } else {
            Data::File(entry.to_owned())
        }
    } else {
        Data::Section(entry.to_owned())
    };

    Ok((
        remaining,
        Line::AddressedSymbol(Addressed {
            vma,
            lma,
            size,
            align,
            entry: data,
        }),
    ))
}

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

fn serialize<T>(section: &Hierarchy, writer: &mut T) -> Result<()>
where
    T: io::Write,
{
    if section.symbols.is_empty() && section.sublevels.len() == 1 {
        let Some(sublevel) = section.sublevels.iter().next() else {
            unreachable!();
        };
        serialize(sublevel.1, writer)
    } else {
        writer.write_fmt(format_args!("name: \"{}\"", section.name))?;
        if !section.symbols.is_empty() || !section.sublevels.is_empty() {
            writer.write_all(", children: [".as_bytes())?;
            let mut first = true;
            for section in section.sublevels.values() {
                if !first {
                    writer.write_all(", ".as_bytes())?;
                } else {
                    first = false;
                }
                writer.write_all("{".as_bytes())?;
                serialize(section, writer)?;
                writer.write_all("}".as_bytes())?;
            }

            for symbol in section.symbols.iter() {
                if !first {
                    writer.write_all(", ".as_bytes())?;
                } else {
                    first = false;
                }

                writer.write_fmt(format_args!(
                    "{{ name: \"{}\", size: {} }}",
                    symbol.name, symbol.size
                ))?;
            }

            writer.write_all("]".as_bytes())?;
        }

        Ok(())
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
    if n.starts_with("_<") {
        // the impl of an interface...
        // e.g. _<nci::messages::Command as core::convert::TryFrom<(u16,&[u8])>>::try_from::h1321c64737577399

        let closing_index = find_closing_bracket(n, 1)
            .ok_or_else(|| eyre!("Name with unexpected shape, no closing: {n}"))?;
        let imp = &n[2..closing_index];
        let impl_pieces = imp.split(" as ").collect::<Vec<_>>();
        assert!(
            impl_pieces.len() == 2,
            "Name with unexpected shape, too few pieces: {}",
            n
        );

        let module = impl_pieces[0].split("::").map(|s| s.to_owned()).collect();

        let func = &n[closing_index + 1..];
        let func_pieces = func.split("::").collect::<Vec<_>>();

        Ok((module, impl_pieces[1].to_owned() + "::" + func_pieces[1]))
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

    let lines = io::BufReader::new(file)
        .lines()
        .map(|l| l.map(parse))
        .collect::<std::result::Result<Result<Vec<Line>>, _>>()??;

    for l in lines {
        match l {
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
                dbg!(&module, &name);
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

fn generate_plot(section: &Hierarchy, target_filename: &str) -> Result<()> {
    // TODO root node name
    let file = File::create(target_filename)?;
    let writer = io::BufWriter::new(file);

    let mut tera = Tera::default();
    tera.add_raw_template("pie", include_str!("../templates/pie.html.tera"))?;

    let mut context = Context::new();
    context.insert("sections", section);
    context.insert("title", "TODO");

    let mut string: Vec<u8> = Vec::new();
    //let string_writer = io::BufWriter::new(string);
    serialize(section, &mut string)?;
    context.insert("serialized", &String::from_utf8(string)?);

    tera.render_to("pie", &context, writer)?;

    Ok(())
}

fn visualize(filename: &str) -> Result<()> {
    let file = File::open(filename)?;

    let tree = parse_file(file)?;
    //println!("{:#?}", tree);
    generate_plot(&tree, "pie.html")?;

    Ok(())
}

fn main() -> Result<()> {
    let Some(map_file) = std::env::args().nth(1) else {
        return Err(eyre!("Usage: linkermap-starburst <map_file>"));
    };

    visualize(&map_file)
}

#[cfg(test)]
mod test_parse {
    use super::*;

    #[test]
    fn test_headline() {
        let line = parse(Ok(
            "     VMA      LMA     Size Align Out     In      Symbol".to_owned(),
        ));
        assert_eq!(line, Line::Headline);
    }

    #[test]
    fn test_provided_symbol() {
        assert_eq!(
            parse(Ok(
                "       0        0        0     1 PROVIDE ( _stext = ORIGIN ( REGION_TEXT ) )"
                    .to_owned(),
            )),
            Line::ProvidedSymbol {
                vma: 0,
                lma: 0,
                text: "_stext = ORIGIN ( REGION_TEXT )".to_owned()
            }
        );

        assert_eq!(
            parse(Ok(
                "   10000    10000        0     1         PROVIDE ( __global_pointer$ = . + 0x800 )".to_owned(),
            )),
            Line::ProvidedSymbol {
                vma: 0x10000,
                lma: 0x10000,
                text: "__global_pointer$ = . + 0x800".to_owned()
            }
        );

        assert_eq!(
            parse(Ok(
                "   10000    10000        0     1         _sdata = .".to_owned(),
            )),
            Line::ProvidedSymbol {
                vma: 0x10000,
                lma: 0x10000,
                text: "_sdata".to_owned()
            }
        );
    }

    #[test]
    fn test_section() {
        assert_eq!(
            parse(Ok(
                "   20000    20000        0     1 .text.dummy".to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x20000,
                lma: 0x20000,
                size: 0,
                align: 1,
                entry: Data::Section(".text.dummy".to_owned()),
            })
        );
    }

    #[test]
    fn test_absolute() {
        assert_eq!(
            parse(Ok(
                "   20000    20000        0     1         . = ABSOLUTE ( _stext )".to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x20000,
                lma: 0x20000,
                size: 0,
                align: 1,
                entry: Data::Absolute("_stext".to_owned()),
            })
        );
    }

    #[test]
    fn test_relative() {
        assert_eq!(
            parse(Ok(
                "   10000    10000        0     1         . += _heap_size".to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x10000,
                lma: 0x10000,
                size: 0,
                align: 1,
                entry: Data::Relative("_heap_size".to_owned()),
            })
        );
    }

    #[test]
    fn test_file() {
        assert_eq!(
            parse(Ok(
                r"   20000    20000       9c     1         C:\work\git\ric-radio\ric-test-fw\target\riscv32imc-unknown-none-elf\release\deps\ric_test_fw-324ec8e9e7d3d14f.ric_test_fw.534aac6062c7601b-cgu.0.rcgu.o:(.init)"
                    .to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x20000,
                lma: 0x20000,
                size: 0x9c,
                align: 1,
                entry: Data::File(r"C:\work\git\ric-radio\ric-test-fw\target\riscv32imc-unknown-none-elf\release\deps\ric_test_fw-324ec8e9e7d3d14f.ric_test_fw.534aac6062c7601b-cgu.0.rcgu.o:(.init)".to_owned()),
            })
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(
            parse(Ok(
                "   2009c    2009c       6a     1                 _start_rust".to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x2009c,
                lma: 0x2009c,
                size: 0x6a,
                align: 1,
                entry: Data::Symbol("_start_rust".to_owned()),
            })
        );

        assert_eq!(
            parse(Ok(
                "   205a8    205a8       30     1                 __INTERRUPTS".to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x205a8,
                lma: 0x205a8,
                size: 0x30,
                align: 1,
                entry: Data::Symbol("__INTERRUPTS".to_owned()),
            })
        );
    }

    #[test]
    fn test_align() {
        assert_eq!(
            parse(Ok(
                "   20106    20106        0     4         . = ALIGN ( 4 )".to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x20106,
                lma: 0x20106,
                size: 0,
                align: 4,
                entry: Data::Align(4),
            })
        );
    }

    #[test]
    fn test_empty() {
        assert_eq!(
            parse(Ok(
                "   2046c    2046c        0     1                 ".to_owned(),
            )),
            Line::AddressedSymbol(Addressed {
                vma: 0x2046c,
                lma: 0x2046c,
                size: 0,
                align: 1,
                entry: Data::Empty,
            })
        );
    }
}

mod error;

use error::Result;
use eyre::{eyre, OptionExt};

use std::{
    collections::HashMap,
    fs::File,
    io::{self, BufRead},
};

use regex::Regex;
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

    let re = Regex::new(
        r"^\s*(?P<vma>[0-9a-fA-F]+)\s+(?P<lma>[0-9a-fA-F]+)\s+(?P<size>[0-9a-fA-F]+)\s+(?P<align>[0-9a-fA-F]+)\s?(?P<indented_entry>.+)$",
    )?;
    re.captures(&s)
        .map(|cap| -> Result<_> {
            let vma = u64::from_str_radix(cap.name("vma").ok_or_eyre("VMA capture")?.as_str(), 16)?;
            let lma = u64::from_str_radix(cap.name("lma").ok_or_eyre("LMA capture")?.as_str(), 16)?;
            let size =
                u64::from_str_radix(cap.name("size").ok_or_eyre("Size capture")?.as_str(), 16)?;
            let align =
                u64::from_str_radix(cap.name("align").ok_or_eyre("Align capture")?.as_str(), 16)?;
            let indented_entry = cap
                .name("indented_entry")
                .ok_or_eyre("indented_entry capture")?
                .as_str();
            let entry = indented_entry.trim_matches(' ');

            if entry.starts_with("PROVIDE ( ") {
                let val = entry
                    .strip_prefix("PROVIDE ( ")
                    .unwrap()
                    .strip_suffix(" )")
                    .unwrap();
                return Ok(Line::ProvidedSymbol {
                    vma,
                    lma,
                    text: val.to_owned(),
                });
            }

            let data = if entry.is_empty() {
                Data::Empty
            } else if indented_entry.starts_with("                ") {
                Data::Symbol(entry.to_owned())
            } else if indented_entry.starts_with("        ") {
                if let Some(segment) = entry.strip_prefix(". = ALIGN ( ") {
                    let val = segment.strip_suffix(" )").ok_or_eyre("no suffix")?;
                    Data::Align(val.parse()?)
                } else if let Some(segment) = entry.strip_prefix(". = ABSOLUTE ( ") {
                    let val = segment.strip_suffix(" )").ok_or_eyre("no suffix")?;
                    Data::Absolute(val.parse().unwrap())
                } else if let Some(segment) = entry.strip_prefix(". += ") {
                    let val = segment;
                    Data::Relative(val.parse().unwrap())
                } else if let Some(segment) = entry.strip_suffix(" = .") {
                    return Ok(Line::ProvidedSymbol {
                        vma,
                        lma,
                        text: segment.to_owned(),
                    });
                } else {
                    Data::File(entry.to_owned())
                }
            } else {
                Data::Section(entry.to_owned())
            };

            Ok(Line::AddressedSymbol(Addressed {
                vma,
                lma,
                size,
                align,
                entry: data,
            }))
        })
        .ok_or_eyre("Regex failed to capture")?
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
mod test {
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

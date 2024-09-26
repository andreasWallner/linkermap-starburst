mod error;
use error::Result;

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
    ProvidedSymbol(String),
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
    Align(usize),
    Empty,
}

fn parse(s: String) -> Line {
    if s.trim_start_matches(' ').starts_with("VMA") {
        return Line::Headline;
    }

    let re = Regex::new(r"^\s*(?P<vma>[0-9a-fA-F]+)\s+(?P<lma>[0-9a-fA-F]+)\s+(?P<size>[0-9a-fA-F]+)\s+(?P<align>[0-9a-fA-F]+)\s?(?P<indented_entry>.+)$").unwrap();
    re.captures(&s)
        .map(|cap| {
            let vma = u64::from_str_radix(cap.name("vma").unwrap().as_str(), 16)
                .expect("VMA parsing failed");
            let lma = u64::from_str_radix(cap.name("lma").unwrap().as_str(), 16)
                .expect("LMA parsing failed");
            let size = u64::from_str_radix(cap.name("size").unwrap().as_str(), 16)
                .expect("Size parsing failed");
            let align = u64::from_str_radix(cap.name("align").unwrap().as_str(), 16)
                .expect("Align parsing failed");
            let indented_entry = cap.name("indented_entry").unwrap().as_str();
            let entry = indented_entry.trim_matches(' ');

            let data = if entry.is_empty() {
                Data::Empty
            } else if indented_entry.starts_with("                ") {
                Data::Symbol(entry.to_owned())
            } else if indented_entry.starts_with("        ") {
                if let Some(segment) = entry.strip_prefix(". = ALIGN ( ") {
                    let val = segment.strip_suffix(" )").unwrap();
                    Data::Align(val.parse().unwrap())
                } else if let Some(segment) = entry.strip_prefix(". = ABSOLUTE ( ") {
                    let val = segment.strip_suffix(" )").unwrap();
                    Data::Absolute(val.parse().unwrap())
                } else {
                    Data::File(entry.to_owned())
                }
            } else {
                Data::Section(entry.to_owned())
            };

            Line::AddressedSymbol(Addressed {
                vma,
                lma,
                size,
                align,
                entry: data,
            })
        })
        .unwrap()
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

fn serialize<T>(section: &Hierarchy, writer: &mut T)
where
    T: io::Write,
{
    if section.symbols.is_empty() && section.sublevels.len() == 1 {
        serialize(section.sublevels.iter().next().unwrap().1, writer);
    } else {
        writer
            .write_fmt(format_args!("name: \"{}\"", section.name))
            .unwrap();
        if !section.symbols.is_empty() || !section.sublevels.is_empty() {
            writer.write_all(", children: [".as_bytes()).unwrap();
            let mut first = true;
            for section in section.sublevels.values() {
                if !first {
                    writer.write_all(", ".as_bytes()).unwrap();
                } else {
                    first = false;
                }
                writer.write_all("{".as_bytes()).unwrap();
                serialize(section, writer);
                writer.write_all("}".as_bytes()).unwrap();
            }

            for symbol in section.symbols.iter() {
                if !first {
                    writer.write_all(", ".as_bytes()).unwrap();
                } else {
                    first = false;
                }

                writer
                    .write_fmt(format_args!(
                        "{{ name: \"{}\", size: {} }}",
                        symbol.name, symbol.size
                    ))
                    .unwrap();
            }

            writer.write_all("]".as_bytes()).unwrap();
        }
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

pub fn split_name(n: &str) -> (Vec<String>, String) {
    if n.starts_with("_<") {
        // the impl of an interface...
        // e.g. _<nci::messages::Command as core::convert::TryFrom<(u16,&[u8])>>::try_from::h1321c64737577399

        let closing_index = find_closing_bracket(n, 1)
            .unwrap_or_else(|| panic!("Name with unexpected shape, no closing: {}", n));
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

        (module, impl_pieces[1].to_owned() + "::" + func_pieces[1])
    } else {
        // normal symbol
        // e.g. nci::comm::packets::Packetizer::get_mut::panic_cold_explicit::h84576c2c34ef900f

        let parts = n.split("::").collect::<Vec<_>>();
        if let [module @ .., name, _hash] = &parts[..] {
            (
                module.iter().map(|&s| s.to_owned()).collect(),
                (*name).to_owned(),
            )
        } else {
            (Vec::new(), n.to_owned())
        }
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
        .collect::<std::result::Result<Vec<Line>, _>>()?;

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
                let (module, name) = split_name(&unescape_name(id));
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

fn generate_plot(section: &Hierarchy, target_filename: &str) {
    // TODO root node name
    let file = File::create(target_filename).unwrap();
    let writer = io::BufWriter::new(file);

    let mut tera = Tera::default();
    tera.add_raw_template("pie", include_str!("../templates/pie.html.tera"))
        .unwrap();
    let mut context = Context::new();
    context.insert("sections", section);
    context.insert("title", "TODO");

    let mut string: Vec<u8> = Vec::new();
    //let string_writer = io::BufWriter::new(string);
    serialize(section, &mut string);
    context.insert("serialized", &String::from_utf8(string).unwrap());

    tera.render_to("pie", &context, writer).unwrap();
}

fn visualize(filename: &str) -> Result<()> {
    let file = File::open(filename)?;

    let tree = parse_file(file)?;
    //println!("{:#?}", tree);
    generate_plot(&tree, "pie.html");

    Ok(())
}

fn main() -> Result<()> {
    if std::env::args().len() != 2 {
        eprintln!("Must be called with only the map file as parameter");
        std::process::exit(-1);
    }
    visualize(&std::env::args().nth(1).unwrap())
}

use eyre::Result;
use nom::{
    IResult,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{hex_digit1, space0, space1},
};
use serde::{Deserialize, Serialize};

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

pub fn parse(l: &str) -> Result<Line> {
    if l.trim_start_matches(' ').starts_with("VMA") {
        return Ok(Line::Headline);
    }

    parse_line(l)
        .map(|(_, line)| line)
        .map_err(|e| eyre::eyre!(e.to_string()))
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_headline() {
        let line = parse("     VMA      LMA     Size Align Out     In      Symbol").unwrap();
        assert_eq!(line, Line::Headline);
    }

    #[test]
    fn test_provided_symbol() {
        assert_eq!(
            parse("       0        0        0     1 PROVIDE ( _stext = ORIGIN ( REGION_TEXT ) )")
                .unwrap(),
            Line::ProvidedSymbol {
                vma: 0,
                lma: 0,
                text: "_stext = ORIGIN ( REGION_TEXT )".to_owned()
            }
        );

        assert_eq!(
            parse("   10000    10000        0     1         PROVIDE ( __global_pointer$ = . + 0x800 )",).unwrap(),
            Line::ProvidedSymbol {
                vma: 0x10000,
                lma: 0x10000,
                text: "__global_pointer$ = . + 0x800".to_owned()
            }
        );

        assert_eq!(
            parse("   10000    10000        0     1         _sdata = .").unwrap(),
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
            parse("   20000    20000        0     1 .text.dummy").unwrap(),
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
            parse("   20000    20000        0     1         . = ABSOLUTE ( _stext )").unwrap(),
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
            parse("   10000    10000        0     1         . += _heap_size").unwrap(),
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
            parse(
                r"   20000    20000       9c     1         C:\work\git\ric-radio\ric-test-fw\target\riscv32imc-unknown-none-elf\release\deps\ric_test_fw-324ec8e9e7d3d14f.ric_test_fw.534aac6062c7601b-cgu.0.rcgu.o:(.init)",
            ).unwrap(),
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
            parse("   2009c    2009c       6a     1                 _start_rust").unwrap(),
            Line::AddressedSymbol(Addressed {
                vma: 0x2009c,
                lma: 0x2009c,
                size: 0x6a,
                align: 1,
                entry: Data::Symbol("_start_rust".to_owned()),
            })
        );

        assert_eq!(
            parse("   205a8    205a8       30     1                 __INTERRUPTS").unwrap(),
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
            parse("   20106    20106        0     4         . = ALIGN ( 4 )").unwrap(),
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
            parse("   2046c    2046c        0     1                 ").unwrap(),
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

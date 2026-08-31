//! A simple example of parsing `.debug_info`.
//!
//! This example demonstrates how to parse the `.debug_info` section of a
//! DWARF object file and iterate over the compilation units and their DIEs.
//! It also demonstrates how to find the DWO unit for each CU in a DWP file.
//!
//! Most of the complexity is due to loading the sections from the object
//! file and DWP file, which is not something that is provided by gimli itself.

// style: allow verbose lifetimes
#![allow(clippy::needless_lifetimes)]

use gimli::{
    AttributeValue, DW_AT_abstract_origin, DW_AT_inline, DW_AT_linkage_name, DW_AT_name,
    DW_INL_declared_inlined, DW_INL_inlined, DW_INL_not_inlined, DW_TAG_inlined_subroutine, DwAt,
};
use object::{Object, ObjectSection};
use std::{borrow, env, error, fs};

// This is a simple wrapper around `object::read::RelocationMap` that implements
// `gimli::read::Relocate` for use with `gimli::RelocateReader`.
// You only need this if you are parsing relocatable object files.
#[derive(Debug, Default)]
struct RelocationMap(object::read::RelocationMap);

impl<'a> gimli::read::Relocate for &'a RelocationMap {
    fn relocate_address(&self, offset: usize, value: u64) -> gimli::Result<u64> {
        Ok(self.0.relocate(offset as u64, value))
    }

    fn relocate_offset(&self, offset: usize, value: usize) -> gimli::Result<usize> {
        <usize as gimli::ReaderOffset>::from_u64(self.0.relocate(offset as u64, value as u64))
    }
}

// The section data that will be stored in `DwarfSections` and `DwarfPackageSections`.
#[derive(Default)]
struct Section<'data> {
    data: borrow::Cow<'data, [u8]>,
    relocations: RelocationMap,
}

// The reader type that will be stored in `Dwarf` and `DwarfPackage`.
// `RelocateReader` is used so relocatable object files also work.
type Reader<'data> =
    gimli::RelocateReader<gimli::EndianSlice<'data, gimli::RunTimeEndian>, &'data RelocationMap>;

// A newtype around a borrowed `DebuggingInformationEntry`. Because we own the wrapper type we
// can attach a string helper that returns a `'data`-lived `Cow`. The `Reader` trait's
// `to_string_lossy` is bounded by `&self`, so we go through `RelocateReader::inner()` to reach
// the `EndianSlice`'s inherent method, which returns a `Cow<'data, str>` directly.
#[derive(Clone, Debug)]
struct Entry<'data, 'a>(&'a gimli::DebuggingInformationEntry<Reader<'data>, usize>);

impl<'data, 'a> std::ops::Deref for Entry<'data, 'a> {
    type Target = gimli::DebuggingInformationEntry<Reader<'data>, usize>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'data, 'a> Entry<'data, 'a> {
    fn attr_string_lossy(
        &self,
        dw_at: DwAt,
        unit: &gimli::UnitRef<'data, Reader<'data>>,
    ) -> Result<Option<borrow::Cow<'data, str>>, gimli::Error> {
        let Some(attr) = self.attr_value(dw_at) else {
            return Ok(None);
        };
        let s = unit.attr_string(attr)?;
        Ok(Some(s.inner().to_string_lossy()))
    }

    fn pos_and_size(&self) -> Result<Option<(u64, u64)>, gimli::Error> {
        // see DWARFv5, 2.17.1-2.17.3, p. 52
        let low_pc = self.attr_value(gimli::DW_AT_low_pc);
        let high_pc = self.attr_value(gimli::DW_AT_high_pc);
        let (pc, size) = match (&low_pc, high_pc) {
            (Some(AttributeValue::Addr(l)), None) => (*l, 0),
            (Some(AttributeValue::Addr(l)), Some(AttributeValue::Addr(h))) => (*l, h - *l),
            (Some(AttributeValue::Addr(l)), Some(AttributeValue::Udata(h))) => (*l, h),
            (None, None) => return Ok(None),
            (Some(AttributeValue::RangeListsRef(_)), _)
            | (_, Some(AttributeValue::RangeListsRef(_))) => todo!("handle range list"),
            _ => return Err(gimli::Error::TypeMismatch),
        };

        Ok(Some((pc, size)))
    }

    fn abstract_origin(
        &self,
        unit: &gimli::UnitRef<'data, Reader<'data>>,
    ) -> Result<Option<gimli::DebuggingInformationEntry<Reader<'data>, usize>>, gimli::Error> {
        let Some(offset) = self.attr_value(DW_AT_abstract_origin) else {
            return Ok(None);
        };
        let offset = match offset {
            AttributeValue::UnitRef(x) => x,
            _ => panic!("abtract_origin is not a UnitRef: {:?}", offset),
        };

        unit.entry(offset).map(|x| Some(x))
    }
}

fn main() {
    let mut args = env::args();
    if args.len() != 2 && args.len() != 3 {
        println!("Usage: {} <file> [dwp]", args.next().unwrap());
        return;
    }
    args.next().unwrap();
    let path = args.next().unwrap();
    let dwp_path = args.next();

    let file = fs::File::open(path).unwrap();
    // SAFETY: This is not safe. `gimli` does not mitigate against modifications to the
    // file while it is being read. See the `memmap2` documentation and take your own
    // precautions. `fs::read` could be used instead if you don't mind loading the entire
    // file into memory.
    let mmap = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let object = object::File::parse(&*mmap).unwrap();
    let endian = if object.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };

    if let Some(dwp_path) = dwp_path {
        let dwp_file = fs::File::open(dwp_path).unwrap();
        let dwp_mmap = unsafe { memmap2::Mmap::map(&dwp_file).unwrap() };
        let dwp_object = object::File::parse(&*dwp_mmap).unwrap();
        assert_eq!(dwp_object.is_little_endian(), object.is_little_endian());

        dump_file(&object, Some(&dwp_object), endian).unwrap();
    } else {
        dump_file(&object, None, endian).unwrap();
    }
}

fn dump_file(
    object: &object::File,
    dwp_object: Option<&object::File>,
    endian: gimli::RunTimeEndian,
) -> Result<(), Box<dyn error::Error>> {
    // Load a `Section` that may own its data.
    fn load_section<'data>(
        object: &object::File<'data>,
        name: &str,
    ) -> Result<Section<'data>, Box<dyn error::Error>> {
        Ok(match object.section_by_name(name) {
            Some(section) => Section {
                data: section.uncompressed_data()?,
                relocations: section.relocation_map().map(RelocationMap)?,
            },
            None => Default::default(),
        })
    }

    // Borrow a `Section` to create a `Reader`.
    fn borrow_section<'data>(
        section: &'data Section<'data>,
        endian: gimli::RunTimeEndian,
    ) -> Reader<'data> {
        let slice = gimli::EndianSlice::new(borrow::Cow::as_ref(&section.data), endian);
        gimli::RelocateReader::new(slice, &section.relocations)
    }

    // Load all of the sections.
    let dwarf_sections = gimli::DwarfSections::load(|id| load_section(object, id.name()))?;
    let dwp_sections = dwp_object
        .map(|dwp_object| {
            gimli::DwarfPackageSections::load(|id| load_section(dwp_object, id.dwo_name().unwrap()))
        })
        .transpose()?;

    let empty_relocations = RelocationMap::default();
    let empty_section =
        gimli::RelocateReader::new(gimli::EndianSlice::new(&[], endian), &empty_relocations);

    // Create `Reader`s for all of the sections and do preliminary parsing.
    // Alternatively, we could have used `Dwarf::load` with an owned type such as `EndianRcSlice`.
    let dwarf = dwarf_sections.borrow(|section| borrow_section(section, endian));
    let dwp = dwp_sections
        .as_ref()
        .map(|dwp_sections| {
            dwp_sections.borrow(|section| borrow_section(section, endian), empty_section)
        })
        .transpose()?;

    // Iterate over the compilation units.
    let mut iter = dwarf.units();
    while let Some(header) = iter.next()? {
        println!("Unit at <.debug_info+0x{:x}>", header.offset().0);
        let unit = dwarf.unit(header)?;
        let unit_ref = unit.unit_ref(&dwarf);
        dump_unit(unit_ref)?;

        // Check for a DWO unit.
        let Some(dwp) = &dwp else { continue };
        let Some(dwo_id) = unit.dwo_id else { continue };
        println!("DWO Unit ID {:x}", dwo_id.0);
        let Some(dwo) = dwp.find_cu(dwo_id, &dwarf)? else {
            continue;
        };
        let Some(header) = dwo.units().next()? else {
            continue;
        };
        let unit = dwo.unit(header)?;
        let unit_ref = unit.unit_ref(&dwo);
        dump_unit(unit_ref)?;
    }

    Ok(())
}

struct NamespaceTracker {
    ns: Vec<String>,
}

impl NamespaceTracker {
    pub fn set(&mut self, depth: isize, name: String) {
        let depth: usize = depth.try_into().expect("depth doesn't fit into usize");
        let depth = depth
            .checked_sub(1)
            .expect(&format!("depth is not > 0: {}", name));
        if depth > self.ns.len() {
            panic!("unexpected depth: {} {} .. {:?}", depth, name, self.ns)
        }
        self.ns.truncate(depth);
        self.ns.push(name);
    }

    pub fn current(&self) -> &[String] {
        self.ns.as_slice()
    }
}

impl Default for NamespaceTracker {
    fn default() -> Self {
        Self { ns: vec![] }
    }
}

fn dump_unit<'data>(unit: gimli::UnitRef<'data, Reader<'data>>) -> Result<(), gimli::Error> {
    let mut ns = NamespaceTracker::default();
    // Iterate over the Debugging Information Entries (DIEs) in the unit.
    let mut entries = unit.entries();
    while let Some(entry) = entries.next_dfs()? {
        let entry = Entry(entry);
        if entry.tag == gimli::DW_TAG_namespace {
            ns.set(
                entry.depth(),
                entry
                    .attr_string_lossy(DW_AT_name, &unit)?
                    .expect("namespace without name")
                    .to_string(),
            );
        }
        if entry.tag == gimli::DW_TAG_subprogram {
            let inline = entry
                .attr_value(DW_AT_inline)
                .map(|a| match a {
                    AttributeValue::Inline(i) => i,
                    _ => panic!("DW_AT_inline not Inline"),
                })
                .unwrap_or(DW_INL_not_inlined);
            let pos_and_size = entry.pos_and_size()?;

            let skip = inline == DW_INL_declared_inlined
                || inline == DW_INL_inlined
                || pos_and_size.is_none();

            let name = entry
                .attr_string_lossy(DW_AT_name, &unit)?
                .unwrap_or(borrow::Cow::Borrowed("---"));
            let linkage_name = entry
                .attr_string_lossy(DW_AT_linkage_name, &unit)?
                .map(|ln| {
                    borrow::Cow::Owned(format!(
                        "{}",
                        linkermap_visualize::demangle::demangle_v0(&ln).unwrap()
                    ))
                })
                .unwrap_or(borrow::Cow::Borrowed("---"));

            println!(
                "{}{}::{}  {}",
                if skip { "SKIP " } else { "" },
                ns.current().join("::"),
                name,
                linkage_name
            );

            if !skip {
                let pos_and_size = pos_and_size.expect("checked above");
                println!(
                    "   addr: {:08x}, size: {:04x}",
                    pos_and_size.0, pos_and_size.1
                );
                for attr in entry.attrs() {
                    print!("   {}: {:?}", attr.name(), attr.value());
                    if let Ok(s) = unit.attr_string(attr.value()) {
                        print!(" '{}'", s.inner().to_string_lossy());
                    }
                    println!();
                }
            }
        }
        if entry.tag == DW_TAG_inlined_subroutine {
            let pos_and_size = entry.pos_and_size()?.expect("inlined w/o pos and size");
            let inlined = entry
                .abstract_origin(&unit)?
                .expect("inlined without abstract_origin");
            let inlined = Entry(&inlined);
            let linkage_name = inlined
                .attr_string_lossy(gimli::DW_AT_linkage_name, &unit)?
                .map(|ln| {
                    borrow::Cow::Owned(format!(
                        "{}",
                        linkermap_visualize::demangle::demangle_v0(&ln).unwrap()
                    ))
                })
                .unwrap_or(borrow::Cow::Borrowed("---"));
            let name = inlined
                .attr_string_lossy(gimli::DW_AT_name, &unit)?
                .unwrap_or(borrow::Cow::Borrowed("---"));
            println!("      [{}] {name} - {linkage_name}", entry.depth());
            println!(
                "      addr: {:08x}, size: {:04x}",
                pos_and_size.0, pos_and_size.1
            );
            /*for attr in inlined.attrs() {
                print!("       {}: {:?}", attr.name(), attr.value());
                if let Ok(s) = unit.attr_string(attr.value()) {
                    print!(" '{}'", s.inner().to_string_lossy());
                }
                println!();
            }*/
            println!();
        }
    }
    Ok(())
}

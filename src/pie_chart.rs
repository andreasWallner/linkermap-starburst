use crate::{Hierarchy, Result, parse_file};
use std::{fs::File, io};
use tera::{Context, Tera};

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

pub fn visualize(filename: &str) -> Result<()> {
    let file = File::open(filename)?;

    let tree = parse_file(file)?;
    generate_plot(&tree, "pie.html")?;

    Ok(())
}

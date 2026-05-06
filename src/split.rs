use crate::error::Result;
use eyre::eyre;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbol<'a> {
    FreeStanding {
        module: &'a str,
        function: &'a str,
        generics: Option<&'a str>,
    },
    Method {
        module: &'a str,
        ty: &'a str,
        function: &'a str,
    },
    Impl {
        ty: &'a str,
        trait_: &'a str,
        function: &'a str,
    },
}

pub fn recognize(s: &str) -> Result<Symbol<'_>> {
    if s.starts_with('<') {
        // <type>::function  OR  <type as trait>::function
        let close_pos = find_matching_close(s)?;
        let inner = &s[1..close_pos];
        let function = s[close_pos + 1..]
            .strip_prefix("::")
            .ok_or_else(|| eyre!("expected '::' after closing '>' in: {s}"))?;

        if let Some(as_pos) = find_as_depth0(inner) {
            Ok(Symbol::Impl {
                ty: &inner[..as_pos],
                trait_: &inner[as_pos + 4..], // skip the 4-byte " as "
                function,
            })
        } else {
            let (module, ty) = split_type_module(inner)?;
            Ok(Symbol::Method {
                module,
                ty,
                function,
            })
        }
    } else {
        // module::function  OR  module::function::<generics>
        let (base, generics) = split_trailing_generics(s)?;
        match base.rfind("::") {
            None => Ok(Symbol::FreeStanding {
                module: &"",
                function: &base,
                generics,
            }),
            Some(sep) => Ok(Symbol::FreeStanding {
                module: &base[..sep],
                function: &base[sep + 2..],
                generics,
            }),
        }
    }
}

#[allow(unused)]
/// Splits `s` at every `::` that appears at angle-bracket depth 0.
///
/// `a::b<x::y>::c` → `["a", "b<x::y>", "c"]`
pub fn split_colons(s: &str) -> Vec<&str> {
    let bytes = s.as_bytes();
    let mut depth: i32 = 0;
    let mut start = 0;
    let mut parts = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'<' => depth += 1,
            b'>' => depth -= 1,
            b':' if depth == 0 && i + 1 < bytes.len() && bytes[i + 1] == b':' => {
                parts.push(&s[start..i]);
                i += 2; // skip both ':'
                start = i;
                continue;
            }
            _ => {}
        }
        i += 1;
    }
    parts.push(&s[start..]);
    parts
}

/// Returns the byte index of the `>` that closes the `<` at position 0.
fn find_matching_close(s: &str) -> Result<usize> {
    let mut depth: i32 = 0;
    for (i, b) in s.bytes().enumerate() {
        match b {
            b'<' => depth += 1,
            b'>' => {
                depth -= 1;
                if depth == 0 {
                    return Ok(i);
                }
            }
            _ => {}
        }
    }
    Err(eyre!("unmatched '<' in: {s}"))
}

/// Splits a qualified type like `a::b::Type<...>` into `("a::b", "Type<...>")`
/// by locating the last `::` that appears at angle-bracket depth 0.
fn split_type_module(ty: &str) -> Result<(&str, &str)> {
    let bytes = ty.as_bytes();
    let mut depth: i32 = 0;
    let mut last_sep: Option<usize> = None;
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'<' => depth += 1,
            b'>' => depth -= 1,
            b':' if depth == 0 && i + 1 < bytes.len() && bytes[i + 1] == b':' => {
                last_sep = Some(i);
                i += 1; // skip the second ':'
            }
            _ => {}
        }
        i += 1;
    }
    let sep = last_sep.ok_or_else(|| eyre!("type has no module path: {ty}"))?;
    Ok((&ty[..sep], &ty[sep + 2..]))
}

fn find_as_depth0(s: &str) -> Option<usize> {
    let mut depth: i32 = 0;
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'<' => depth += 1,
            b'>' => depth -= 1,
            b' ' if depth == 0 && s[i..].starts_with(" as ") => return Some(i),
            _ => {}
        }
        i += 1;
    }
    None
}

/// Splits `module::function::<generics>` into `("module::function", Some("<generics>"))`.
/// Returns `(s, None)` when there are no trailing generics.
/// Returns an error when the string ends with `>` but has no `"::<"` separator,
/// which means a bare `<` is embedded in the symbol name without a `::` prefix.
fn split_trailing_generics(s: &str) -> Result<(&str, Option<&str>)> {
    if !s.ends_with('>') {
        return Ok((s, None));
    }
    let bytes = s.as_bytes();
    let mut depth: i32 = 0;
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'<' => depth += 1,
            b'>' => depth -= 1,
            // "::<" at depth 0 separates function name from its generic arguments
            b':' if depth == 0
                && i + 2 < bytes.len()
                && bytes[i + 1] == b':'
                && bytes[i + 2] == b'<' =>
            {
                return Ok((&s[..i], Some(&s[i + 2..])));
            }
            _ => {}
        }
        i += 1;
    }
    Err(eyre!(
        "symbol ends with '>' but has no '::' before '<': {s}"
    ))
}

pub fn split_module(s: &str) -> Result<Vec<&str>> {
    let mut result = vec![];
    let mut start = 0usize;

    let bytes = s.as_bytes();
    let mut depth: i32 = 0;
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'<' => depth += 1,
            b'>' if depth == 0 => depth -= 1,
            b'>' => depth -= 1,
            b':' if depth == 0 && bytes.get(i + 1) == Some(&b':') => {
                let piece = &s[start..i];

                if piece.len() > 0 {
                    result.push(piece);
                }

                start = i + 2;
                i += 1;
            }
            _ => {}
        }
        i += 1;
    }
    if bytes.len() > start {
        result.push(&s[start..])
    }

    if depth != 0 {
        return Err(eyre!("symbol does not have balanced '<' and '>': {s}"));
    }

    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;
    use assert2::assert;
    use test_case::test_case;

    #[test_case("_start", Symbol::FreeStanding { module: &"", function: &"_start", generics: None })]
    #[test_case(
        "nci::sm::error_notification",
        Symbol::FreeStanding { module: &"nci::sm", function: &"error_notification", generics: None })
    ]
    #[test_case(
        "iso14443::aca::activate::<ric_test_fw::driver::RicDriver>",
        Symbol::FreeStanding { module: &"iso14443::aca", function: &"activate", generics: Some(&"<ric_test_fw::driver::RicDriver>")})
    ]
    #[test_case(
        "<nci::messages::rf_interface_activated::NotificationBuilderRfTechnologyNotSet>::set_rf_protocol",
        Symbol::Method { module: &"nci::messages::rf_interface_activated", ty: &"NotificationBuilderRfTechnologyNotSet", function: &"set_rf_protocol" })
    ]
    #[test_case(
        "<ric_hal::timer::timeout_timer::TimeoutTimer<ric_hal::timer::timeout_timer::Initialized>>::new",
        Symbol::Method { module: &"ric_hal::timer::timeout_timer", ty: &"TimeoutTimer<ric_hal::timer::timeout_timer::Initialized>", function: &"new"})
    ]
    #[test_case(
        "<nci::comm::nci_comm::NciComm<nci::comm::transport::SpiTransport<ric_test_fw::nci_spi::RicNciSpi, ric_test_fw::nci_spi::RicNciOutputPin, nci::comm::software_crc::SwCrcFactory>, ric_test_fw::driver::RicDriver, ric_test_fw::clock::TimerClock>>::run",
        Symbol::Method { module: &"nci::comm::nci_comm", ty: &"NciComm<nci::comm::transport::SpiTransport<ric_test_fw::nci_spi::RicNciSpi, ric_test_fw::nci_spi::RicNciOutputPin, nci::comm::software_crc::SwCrcFactory>, ric_test_fw::driver::RicDriver, ric_test_fw::clock::TimerClock>", function: &"run"})
    ]
    #[test_case(
        "<nci::sm::TypeASettings as core::default::Default>::default",
        Symbol::Impl { ty: &"nci::sm::TypeASettings", trait_: &"core::default::Default", function: &"default" })
    ]
    #[test_case(
        "<nci::comm::UncheckedMessageId as core::convert::From<u16>>::from",
        Symbol::Impl { ty: &"nci::comm::UncheckedMessageId", trait_: &"core::convert::From<u16>", function: &"from" })
    ]
    #[test_case(
        "<u16 as core::convert::From<nci::comm::FullMessageId>>::from",
        Symbol::Impl { ty: &"u16", trait_: &"core::convert::From<nci::comm::FullMessageId>", function: &"from" })
    ]
    #[test_case(
        "<iso14443::frames::Ats as core::convert::TryFrom<&[u8]>>::try_from",
        Symbol::Impl { ty: &"iso14443::frames::Ats", trait_: &"core::convert::TryFrom<&[u8]>", function: &"try_from" })
    ]
    fn test_split(s: &str, expected: Symbol<'static>) {
        assert!(recognize(s).unwrap() == expected)
    }

    #[test_case("a::b<yyy>::c", &[&"a", &"b<yyy>", &"c"])]
    fn test_split_module(s: &str, expected: &[&str]) {
        assert!(split_module(s).unwrap() == expected)
    }
}

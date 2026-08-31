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

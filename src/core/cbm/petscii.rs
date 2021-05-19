/// Taken from https://github.com/simmons/cbm which I don't want to include as a full dependency since it brings in an
/// older version of clap.
use std::char;
use std::fmt;
use std::fmt::Write;
use std::iter::IntoIterator;
use std::ops::Index;

// The Unicode code point we use for untranslatable PETSCII characters.
const NONE: char = char::REPLACEMENT_CHARACTER;

// The PETSCII character we use for untranslatable Unicode code points.
// This is the "upper left to lower right diagonal lines" graphic.
const PETSCII_NONE: u8 = 0x7F;

// From: http://style64.org/petscii/
#[cfg_attr(rustfmt, rustfmt_skip)]
static PETSCII_TO_CHAR_MAP: [char; 256] = [
    // control codes
    NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
    NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
    // punctuation, numbers, a-z
    ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
    '@', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    // [, british pound, ], up arrow, left arrow, horizontal line
    '[', '\u{00A3}', ']', '\u{2191}', '\u{2190}', '\u{2501}',
    // A-Z
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    // box vert/horiz, left checkerboard, box vert, checkerboard-0, \-diag lines,
    '\u{254b}', NONE, '\u{2503}', '\u{2592}', NONE,
    // control codes
    NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
    NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
    // non-breaking space, left half block, lower half block, upper 1/8 block
    '\u{00a0}', '\u{258c}', '\u{2584}', '\u{2594}',
    // lower 1/8 block, left 1/4 block, checkerboard-1, right 1/4 block -> 1/8
    '\u{2581}', '\u{258e}', '\u{2592}', '\u{2595}',
    // lower half checkerboard, /-diag lines, right 1/4 block -> 1/8, box vert+right
    NONE, NONE, '\u{2595}', '\u{2523}',
    // quadrant lower right, box up+right, box down+left, lower 1/4 block
    '\u{2597}', '\u{2517}', '\u{2513}', '\u{2582}',
    // box down+right, box up+horiz, box down+horiz, box vertical+left
    '\u{250f}', '\u{253b}', '\u{2533}', '\u{252b}',
    // left 1/4 block, left 3/8 block, right 3/8 block -> 1/8, upper 1/4 block -> 1/8
    '\u{258e}', '\u{258d}', '\u{2595}', '\u{2594}',
    // upper 3/8 block -> 1/8, lower 3/8 block, check mark, quadrant lower left
    '\u{2594}', '\u{2583}', '\u{2713}', '\u{2596}',
    // quadrant upper right, box up+left, quadrant upper left, quadrant upper left and lower right
    '\u{259d}', '\u{2518}', '\u{2598}', '\u{259a}',
    // box horiz
    '\u{2501}',
    // A-Z
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    // box vert/horiz, left checkerboard, box vert, checkerboard-0, \-diag lines,
    '\u{254b}', NONE, '\u{2503}', '\u{2592}', NONE,
    // non-breaking space, left half block, lower half block, upper 1/8 block
    '\u{00a0}', '\u{258c}', '\u{2584}', '\u{2594}',
    // lower 1/8 block, left 1/4 block, checkerboard-1, right 1/4 block -> 1/8
    '\u{2581}', '\u{258e}', '\u{2592}', '\u{2595}',
    // lower half checkerboard, /-diag lines, right 1/4 block -> 1/8, box vert+right
    NONE, NONE, '\u{2595}', '\u{2523}',
    // quadrant lower right, box up+right, box down+left, lower 1/4 block
    '\u{2597}', '\u{2517}', '\u{2513}', '\u{2582}',
    // box down+right, box up+horiz, box down+horiz, box vertical+left
    '\u{250f}', '\u{253b}', '\u{2533}', '\u{252b}',
    // left 1/4 block, left 3/8 block, right 3/8 block -> 1/8, upper 1/4 block -> 1/8
    '\u{258e}', '\u{258d}', '\u{2595}', '\u{2594}',
    // upper 3/8 block -> 1/8, lower 3/8 block, check mark, quadrant lower left
    '\u{2594}', '\u{2583}', '\u{2713}', '\u{2596}',
    // quadrant upper right, box up+left, quadrant upper left, checkerboard-0
    '\u{259d}', '\u{2518}', '\u{2598}', '\u{2592}',
];

fn petscii_to_unicode_char(byte: u8) -> char {
    PETSCII_TO_CHAR_MAP[byte as usize]
}

fn petscii_to_unicode_string(petscii: &[u8]) -> String {
    let mut string = String::with_capacity(petscii.len());
    for petscii_char in petscii {
        string.push(petscii_to_unicode_char(*petscii_char));
    }
    string
}

/// Commodore's 8-bit computers used an unusual variant of ASCII commonly known as "PETSCII".
/// A PETSCII string can be represented by this `Petscii` struct, and its functions help handle
/// PETSCII strings and perform lossy conversions between PETSCII and Unicode.
#[derive(Clone, PartialEq)]
pub struct Petscii(Vec<u8>);

impl Petscii {
    pub fn from_bytes(bytes: &[u8]) -> Petscii {
        Petscii(bytes.to_owned())
    }

    pub fn from_padded_bytes(bytes: &[u8], pad_byte: u8) -> Petscii {
        let mut end = bytes.len();
        for offset in (0..end).rev() {
            if bytes[offset] == pad_byte {
                end -= 1;
            } else {
                break;
            }
        }
        Petscii((&bytes[..end]).to_owned())
    }

    /// We only translate Unicode code points that happen to be present in our
    /// PETSCII mapping. This includes letters, numbers, punctuation, and a
    /// handful of block graphic code points.
    pub fn from_str(string: &str) -> Petscii {
        let mut petscii_bytes = Vec::with_capacity(string.len());
        'outer: for c in string.chars() {
            // This is inefficient, but hopefully seldom used.
            for p in 0..PETSCII_TO_CHAR_MAP.len() {
                if PETSCII_TO_CHAR_MAP[p] == c {
                    petscii_bytes.push(p as u8);
                    continue 'outer;
                }
            }
            petscii_bytes.push(PETSCII_NONE);
        }
        Petscii(petscii_bytes.to_owned())
    }

    pub fn as_bytes<'a>(&'a self) -> &'a [u8] {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn to_string(&self) -> String {
        petscii_to_unicode_string(&self.0[..])
    }

    pub fn write_bytes_with_padding(&self, bytes: &mut [u8], pad_byte: u8) -> Result<(), ()> {
        let src_len = self.0.len();
        let dst_len = bytes.len();
        if src_len > dst_len {
            return Err(());
        } else {
            &bytes[..src_len].copy_from_slice(&self.0);
            for i in src_len..dst_len {
                bytes[i] = pad_byte;
            }
        }
        Ok(())
    }
}

impl Into<String> for Petscii {
    fn into(self) -> String {
        self.to_string()
    }
}

impl From<String> for Petscii {
    fn from(string: String) -> Petscii {
        Self::from_str(&string)
    }
}

impl<'a> From<&'a String> for Petscii {
    fn from(string: &String) -> Petscii {
        Self::from_str(string)
    }
}

impl<'a> From<&'a str> for Petscii {
    fn from(string: &str) -> Petscii {
        Self::from_str(string)
    }
}

impl AsRef<Petscii> for Petscii {
    fn as_ref(&self) -> &Petscii {
        &self
    }
}

impl AsRef<[u8]> for Petscii {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl fmt::Display for Petscii {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for petscii_char in self.0.iter() {
            let c = petscii_to_unicode_char(*petscii_char);
            f.write_char(c)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Petscii {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "\"")?;
        for petscii_char in self.0.iter() {
            let c = petscii_to_unicode_char(*petscii_char);
            if c == '"' {
                f.write_str("\\\"")?;
            } else {
                f.write_char(c)?;
            }
        }
        write!(f, "\"")
    }
}

impl Index<usize> for Petscii {
    type Output = u8;
    fn index(&self, index: usize) -> &u8 {
        &self.0[index]
    }
}

impl IntoIterator for Petscii {
    type Item = u8;
    type IntoIter = ::std::vec::IntoIter<u8>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Petscii {
    type Item = &'a u8;
    type IntoIter = ::std::slice::Iter<'a, u8>;
    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test]
    fn dump_petscii_chart() {
        for a in 0..16u8 {
            for b in 0..16u8 {
                let c = a * 16 + b;
                print!("{} ", petscii_to_unicode_char(c));
            }
            println!("");
        }
    }

    #[test]
    fn test_petscii_chars() {
        assert_eq!(petscii_to_unicode_char(0x00), char::REPLACEMENT_CHARACTER);
        assert_eq!(petscii_to_unicode_char(0x20), ' ');
        assert_eq!(petscii_to_unicode_char(0x21), '!');
        assert_eq!(petscii_to_unicode_char(0x2f), '/');
        assert_eq!(petscii_to_unicode_char(0x30), '0');
        assert_eq!(petscii_to_unicode_char(0x31), '1');
        assert_eq!(petscii_to_unicode_char(0x3f), '?');
        assert_eq!(petscii_to_unicode_char(0x40), '@');
        assert_eq!(petscii_to_unicode_char(0x41), 'a');
        assert_eq!(petscii_to_unicode_char(0x5a), 'z');
        assert_eq!(petscii_to_unicode_char(0x61), 'A');
        assert_eq!(petscii_to_unicode_char(0x7a), 'Z');
        assert_eq!(
            petscii_to_unicode_char(0x7b),
            char::from_u32(0x254b).unwrap()
        );
        assert_eq!(petscii_to_unicode_char(0x80), char::REPLACEMENT_CHARACTER);
        assert_eq!(petscii_to_unicode_char(0x9f), char::REPLACEMENT_CHARACTER);
        assert_eq!(
            petscii_to_unicode_char(0xa0),
            char::from_u32(0x00a0).unwrap()
        );
        assert_eq!(
            petscii_to_unicode_char(0xbf),
            char::from_u32(0x259a).unwrap()
        );
        assert_eq!(
            petscii_to_unicode_char(0xc0),
            char::from_u32(0x2501).unwrap()
        );
        assert_eq!(petscii_to_unicode_char(0xc1), 'A');
        assert_eq!(petscii_to_unicode_char(0xda), 'Z');
        assert_eq!(
            petscii_to_unicode_char(0xff),
            char::from_u32(0x2592).unwrap()
        );
    }

    #[test]
    fn test_petscii() {
        let bytes = &[0x41, 0x42, 0x43, 0x61, 0x62, 0x63, 0xc1, 0xc2, 0xc3, 0x00];
        let petscii = Petscii::from_bytes(bytes);

        // Test Index<usize> for Petscii
        assert_eq!(petscii[0], 0x41);
        assert_eq!(petscii[8], 0xc3);
        assert_eq!(petscii[9], 0x00);

        // Test IntoIterator for Petscii
        let mut bytes_expected = (&bytes[..]).to_vec();
        for byte in petscii.clone() {
            assert!(bytes_expected.len() > 0);
            let byte_expected = bytes_expected.remove(0);
            assert_eq!(byte_expected, byte);
        }

        // Test IntoIterator for &Petscii
        let mut bytes_expected = (&bytes[..]).to_vec();
        for byte in &petscii {
            assert!(bytes_expected.len() > 0);
            let byte_expected = bytes_expected.remove(0);
            assert_eq!(byte_expected, *byte);
        }

        let string: String = petscii.into();
        assert_eq!(string, "abcABCABC\u{FFFD}");
    }

    #[test]
    fn test_padding() {
        const LENGTH: usize = 8;
        const PAD: u8 = 0xa0;

        // Test basic padding scenario
        const BASIC_PADDED: [u8; LENGTH] = [0x41, 0x42, 0x43, PAD, PAD, PAD, PAD, PAD];
        const BASIC_UNPADDED: [u8; 3] = [0x41, 0x42, 0x43];
        let petscii = Petscii::from_bytes(&BASIC_PADDED);
        assert_eq!(petscii.len(), BASIC_PADDED.len());
        let petscii = Petscii::from_padded_bytes(&BASIC_PADDED, PAD);
        assert_eq!(petscii.len(), BASIC_UNPADDED.len());
        let mut bytes = [b'\0'; LENGTH];
        petscii.write_bytes_with_padding(&mut bytes, PAD).unwrap();
        assert_eq!(bytes, BASIC_PADDED);

        // Test full buffer (no padding)
        const FULL: [u8; LENGTH] = [0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48];
        let petscii = Petscii::from_padded_bytes(&FULL, PAD);
        assert_eq!(petscii.len(), FULL.len());
        let mut bytes = [b'\0'; LENGTH];
        petscii.write_bytes_with_padding(&mut bytes, PAD).unwrap();
        assert_eq!(bytes, FULL);

        // Test writing to undersized buffer
        let mut bytes = [b'\0'; LENGTH - 1];
        assert!(petscii.write_bytes_with_padding(&mut bytes, PAD).is_err());

        // Test zero-length buffer
        const ZERO_PADDED: [u8; LENGTH] = [PAD, PAD, PAD, PAD, PAD, PAD, PAD, PAD];
        const ZERO_UNPADDED: [u8; 0] = [];
        let petscii = Petscii::from_bytes(&ZERO_PADDED);
        assert_eq!(petscii.len(), ZERO_PADDED.len());
        let petscii = Petscii::from_padded_bytes(&ZERO_PADDED, PAD);
        assert_eq!(petscii.len(), ZERO_UNPADDED.len());
        let mut bytes = [b'\0'; LENGTH];
        petscii.write_bytes_with_padding(&mut bytes, PAD).unwrap();
        assert_eq!(bytes, ZERO_PADDED);
    }

    #[test]
    fn test_conversions() {
        fn process_move<P: Into<Petscii>>(petscii: P) -> String {
            let petscii: Petscii = petscii.into();
            petscii.into()
        }
        fn process_ref<P: AsRef<Petscii> + ?Sized>(petscii: &P) -> String {
            let petscii: &Petscii = petscii.as_ref();
            petscii.to_string()
        }

        let str_slice_storage: String = String::from("&str (string slice) test");
        let str_slice: &str = &str_slice_storage[..];
        let string: String = String::from("String test.");
        let string_ref: &String = &string;
        let petscii: Petscii = Petscii::from_str("Petscii test.");

        assert_eq!(process_move(str_slice), str_slice_storage);
        assert_eq!(process_move(string.clone()), string);
        assert_eq!(process_move(string_ref), string);
        assert_eq!(process_move(petscii.clone()), petscii.to_string());

        // Test AsRef implementations
        static INPUT_BUFFER: [u8; 3] = [b'a', b'b', b'c'];
        let petscii: Petscii = Petscii::from_bytes(&INPUT_BUFFER);
        process_ref(&petscii);
        let output_buffer: &[u8] = petscii.as_ref();
        assert_eq!(output_buffer, INPUT_BUFFER);
    }
}

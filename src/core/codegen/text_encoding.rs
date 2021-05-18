use crate::core::parser::TextEncoding;
use cbm::Petscii;

pub fn encode_text(str: &str, encoding: TextEncoding) -> Vec<u8> {
    match encoding {
        TextEncoding::Ascii | TextEncoding::Unspecified => str.as_bytes().to_vec(),
        TextEncoding::Petscii => Petscii::from_str(&str).as_bytes().to_vec(),
        TextEncoding::Petscreen => Petscii::from_str(&str)
            .as_bytes()
            .iter()
            .map(|c| match c {
                0x00..=0x1f => c + 128,
                0x20..=0x3f => *c,
                0x40..=0x5f => c - 64,
                0x60..=0x7f => c - 32,
                0x80..=0x9f => c + 64,
                0xa0..=0xbf => c - 64,
                0xc0..=0xfe => c - 128,
                0xff => 0x5e,
            })
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_encode_text() {
        assert_eq!(encode_text("abc", TextEncoding::Ascii), &[97, 98, 99]);
        assert_eq!(encode_text("abc", TextEncoding::Petscii), &[65, 66, 67]);
        assert_eq!(encode_text("abc", TextEncoding::Petscreen), &[1, 2, 3]);
    }
}

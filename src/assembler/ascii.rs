use std::fmt;

use regex::{Captures, Regex};

#[derive(Clone, PartialEq)]
pub struct AsciiStr {
    inner: Vec<u8>,
}

impl AsciiStr {
    pub unsafe fn from_bytes_unchecked(buf: &[u8]) -> Self {
        AsciiStr {
            inner: Vec::from(buf),
        }
    }
}

impl TryFrom<String> for AsciiStr {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            return Err(());
        }

        Ok(AsciiStr {
            inner: value.into_bytes(),
        })
    }
}

impl std::fmt::Debug for AsciiStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe {
            std::str::from_utf8_unchecked(&self.inner)
        })
    }
}

impl PartialEq<str> for AsciiStr {
    fn eq(&self, other: &str) -> bool {
        unsafe { std::str::from_utf8_unchecked(&self.inner) == other }
    }
}

impl PartialEq<&str> for AsciiStr {
    fn eq(&self, other: &&str) -> bool {
        unsafe { std::str::from_utf8_unchecked(&self.inner) == *other }
    }
}

impl fmt::Display for AsciiStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", unsafe {
            std::str::from_utf8_unchecked(&self.inner)
        })
    }
}

impl std::ops::Deref for AsciiStr {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl IntoIterator for AsciiStr {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnescapeError {
    UnmatchedBackslash(usize),
    InvalidAscii(char),
}

pub fn unescape_str<'a>(s: &'a str) -> Result<AsciiStr, UnescapeError> {
    let mut failure = None;

    let hex = Regex::new(r"\\x[0-9a-fA-F]{2}").unwrap();
    let mut numbered = hex.replace_all(s, |cap: &Captures<'_>| {
        // The RegEx expression guarantees a valid hex.
        let byte = u8::from_str_radix(&cap[0].strip_prefix("\\x").unwrap(), 16).unwrap();

        if byte > 0x7F {
            failure = Some(byte);
        }

        unsafe { String::from_utf8_unchecked(vec![byte]) }
    });

    if let Some(byte) = failure {
        return Err(UnescapeError::InvalidAscii(byte as char));
    }

    let owned = numbered.to_owned();
    numbered = {
        let octal = Regex::new(r"\\[0-7]{3}").unwrap();

        octal.replace_all(&owned, |cap: &Captures<'_>| {
            // The RegEx expression guarantees a valid octal.
            let byte = u8::from_str_radix(&cap[0].strip_prefix('\\').unwrap(), 8).unwrap();

            if byte > 0x7F {
                failure = Some(byte);
            }

            unsafe { String::from_utf8_unchecked(vec![byte]) }
        })
    };

    if let Some(byte) = failure {
        return Err(UnescapeError::InvalidAscii(byte as char));
    }

    let invalid = Regex::new(r"(\\[^nt0rabfv\\])|(\\\z)").unwrap();
    if let Some(index) = invalid.find(&numbered) {
        return Err(UnescapeError::UnmatchedBackslash(index.start()));
    }

    let simple = numbered
        .replace("\\n", "\n")
        .replace("\\\\", "\\")
        .replace("\\t", "\t")
        .replace("\\'", "'")
        .replace("\\\"", "\"")
        .replace("\\0", "\0")
        .replace("\\r", "\r")
        .replace("\\a", "\x07")
        .replace("\\b", "\x08")
        .replace("\\f", "\x0C")
        .replace("\\v", "\x0B");

    if let Some(byte) = simple.find(|c| c > '\x7F') {
        return Err(UnescapeError::InvalidAscii(
            simple.chars().nth(byte).unwrap(),
        ));
    }

    unsafe { Ok(AsciiStr::from_bytes_unchecked(simple.as_bytes())) }
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn unescape() {
        let test_str = "\\050 hello \\x29 \\t\\n\\0";
        let unescaped = unescape_str(test_str).unwrap();
        assert_eq!(unescaped, "\x28 hello \x29 \t\n\0");

        let failure = "\\050 \\";
        unescape_str(failure).unwrap_err();
    }
}

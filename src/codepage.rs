#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, string::String};

#[derive(Debug, Default, Clone, Copy)]
/// Windows codepage to use for encoding/decoding short filenames
///
/// Windows codepages are an extension of ASCII. They were in use by
/// Microsoft all the way back in the '80s and '90s. FAT uses them
/// only for the short file names and they don't play much of a big role
/// in it. They are used by this library for maximal backwards compatibility.
/// Virtually all FAT implementations use the 437 OEM codepage (OEM United States),
/// which is the default codepage.
pub enum Codepage {
    #[default]
    /// OEM United States
    CP437,
    /// Arabic (Transparent ASMO); Arabic (DOS)
    CP720,
    /// OEM Greek (formerly 437G); Greek (DOS)
    CP737,
    /// OEM Baltic; Baltic (DOS)
    CP775,
    /// OEM Multilingual Latin 1; Western European (DOS)
    CP850,
    /// OEM Latin 2; Central European (DOS)
    CP852,
    /// OEM Cyrillic (primarily Russian)
    CP855,
    /// OEM Turkish; Turkish (DOS)
    CP857,
    /// OEM Multilingual Latin 1 + Euro symbol
    CP858,
    /// OEM Portuguese; Portuguese (DOS)
    CP860,
    /// OEM Icelandic; Icelandic (DOS)
    CP861,
    /// OEM Hebrew; Hebrew (DOS)
    CP862,
    /// OEM French Canadian; French Canadian (DOS)
    CP863,
    /// OEM Arabic; Arabic (864)
    CP864,
    /// OEM Nordic; Nordic (DOS)
    CP865,
    /// OEM Russian; Cyrillic (DOS)
    CP866,
    /// OEM Modern Greek; Greek, Modern (DOS)
    CP869,
    /// ANSI/OEM Thai (ISO 8859-11); Thai (Windows)
    CP874,
}

impl Codepage {
    pub(crate) fn decode(&self, v: &[u8]) -> String {
        use oem_cp::{decode_string_complete_table, decode_string_incomplete_table_lossy};

        match self {
            Codepage::CP437 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP437)
            }
            Codepage::CP720 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP720)
            }
            Codepage::CP737 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP737)
            }
            Codepage::CP775 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP775)
            }
            Codepage::CP850 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP850)
            }
            Codepage::CP852 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP852)
            }
            Codepage::CP855 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP855)
            }
            Codepage::CP857 => {
                decode_string_incomplete_table_lossy(v, &oem_cp::code_table::DECODING_TABLE_CP857)
            }
            Codepage::CP858 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP858)
            }
            Codepage::CP860 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP860)
            }
            Codepage::CP861 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP861)
            }
            Codepage::CP862 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP862)
            }
            Codepage::CP863 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP863)
            }
            Codepage::CP864 => {
                decode_string_incomplete_table_lossy(v, &oem_cp::code_table::DECODING_TABLE_CP864)
            }
            Codepage::CP865 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP865)
            }
            Codepage::CP866 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP866)
            }
            Codepage::CP869 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP869)
            }
            Codepage::CP874 => {
                decode_string_incomplete_table_lossy(v, &oem_cp::code_table::DECODING_TABLE_CP874)
            }
        }
    }

    // this might come in handy in the future
    #[allow(unused)]
    pub(crate) fn encode(&self, s: &str) -> Box<[u8]> {
        use oem_cp::encode_string_lossy;

        match self {
            Codepage::CP437 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP437),
            Codepage::CP720 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP720),
            Codepage::CP737 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP737),
            Codepage::CP775 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP775),
            Codepage::CP850 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP850),
            Codepage::CP852 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP852),
            Codepage::CP855 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP855),
            Codepage::CP857 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP857),
            Codepage::CP858 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP858),
            Codepage::CP860 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP860),
            Codepage::CP861 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP861),
            Codepage::CP862 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP862),
            Codepage::CP863 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP863),
            Codepage::CP864 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP864),
            Codepage::CP865 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP865),
            Codepage::CP866 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP866),
            Codepage::CP869 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP869),
            Codepage::CP874 => encode_string_lossy(s, &oem_cp::code_table::ENCODING_TABLE_CP874),
        }
        .into_boxed_slice()
    }

    pub(crate) fn contains(&self, c: char) -> bool {
        if c.is_ascii() {
            return true;
        }

        match self {
            Codepage::CP437 => oem_cp::code_table::DECODING_TABLE_CP437.contains(&c),
            Codepage::CP720 => oem_cp::code_table::DECODING_TABLE_CP720.contains(&c),
            Codepage::CP737 => oem_cp::code_table::DECODING_TABLE_CP737.contains(&c),
            Codepage::CP775 => oem_cp::code_table::DECODING_TABLE_CP775.contains(&c),
            Codepage::CP850 => oem_cp::code_table::DECODING_TABLE_CP850.contains(&c),
            Codepage::CP852 => oem_cp::code_table::DECODING_TABLE_CP852.contains(&c),
            Codepage::CP855 => oem_cp::code_table::DECODING_TABLE_CP855.contains(&c),
            Codepage::CP857 => oem_cp::code_table::DECODING_TABLE_CP857.contains(&Some(c)),
            Codepage::CP858 => oem_cp::code_table::DECODING_TABLE_CP858.contains(&c),
            Codepage::CP860 => oem_cp::code_table::DECODING_TABLE_CP860.contains(&c),
            Codepage::CP861 => oem_cp::code_table::DECODING_TABLE_CP861.contains(&c),
            Codepage::CP862 => oem_cp::code_table::DECODING_TABLE_CP862.contains(&c),
            Codepage::CP863 => oem_cp::code_table::DECODING_TABLE_CP863.contains(&c),
            Codepage::CP864 => oem_cp::code_table::DECODING_TABLE_CP864.contains(&Some(c)),
            Codepage::CP865 => oem_cp::code_table::DECODING_TABLE_CP865.contains(&c),
            Codepage::CP866 => oem_cp::code_table::DECODING_TABLE_CP866.contains(&c),
            Codepage::CP869 => oem_cp::code_table::DECODING_TABLE_CP869.contains(&c),
            Codepage::CP874 => oem_cp::code_table::DECODING_TABLE_CP874.contains(&Some(c)),
        }
    }
}

impl TryFrom<u16> for Codepage {
    type Error = ();

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            437 => Ok(Codepage::CP437),
            720 => Ok(Codepage::CP720),
            737 => Ok(Codepage::CP737),
            775 => Ok(Codepage::CP775),
            850 => Ok(Codepage::CP850),
            852 => Ok(Codepage::CP852),
            855 => Ok(Codepage::CP855),
            857 => Ok(Codepage::CP857),
            858 => Ok(Codepage::CP858),
            860 => Ok(Codepage::CP860),
            861 => Ok(Codepage::CP861),
            862 => Ok(Codepage::CP862),
            863 => Ok(Codepage::CP863),
            864 => Ok(Codepage::CP864),
            865 => Ok(Codepage::CP865),
            866 => Ok(Codepage::CP866),
            869 => Ok(Codepage::CP869),
            874 => Ok(Codepage::CP874),
            _ => Err(()),
        }
    }
}

impl From<Codepage> for u16 {
    fn from(value: Codepage) -> Self {
        match value {
            Codepage::CP437 => 437,
            Codepage::CP720 => 720,
            Codepage::CP737 => 737,
            Codepage::CP775 => 775,
            Codepage::CP850 => 850,
            Codepage::CP852 => 852,
            Codepage::CP855 => 855,
            Codepage::CP857 => 857,
            Codepage::CP858 => 858,
            Codepage::CP860 => 860,
            Codepage::CP861 => 861,
            Codepage::CP862 => 862,
            Codepage::CP863 => 863,
            Codepage::CP864 => 864,
            Codepage::CP865 => 865,
            Codepage::CP866 => 866,
            Codepage::CP869 => 869,
            Codepage::CP874 => 874,
        }
    }
}

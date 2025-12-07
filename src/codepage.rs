#[cfg(not(feature = "std"))]
use alloc::string::String;

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
    #[cfg(any(feature = "codepage", feature = "cp720"))]
    /// Arabic (Transparent ASMO); Arabic (DOS)
    CP720,
    #[cfg(any(feature = "codepage", feature = "cp737"))]
    /// OEM Greek (formerly 437G); Greek (DOS)
    CP737,
    #[cfg(any(feature = "codepage", feature = "cp775"))]
    /// OEM Baltic; Baltic (DOS)
    CP775,
    #[cfg(any(feature = "codepage", feature = "cp850"))]
    /// OEM Multilingual Latin 1; Western European (DOS)
    CP850,
    #[cfg(any(feature = "codepage", feature = "cp852"))]
    /// OEM Latin 2; Central European (DOS)
    CP852,
    #[cfg(any(feature = "codepage", feature = "cp855"))]
    /// OEM Cyrillic (primarily Russian)
    CP855,
    #[cfg(any(feature = "codepage", feature = "cp857"))]
    /// OEM Turkish; Turkish (DOS)
    CP857,
    #[cfg(any(feature = "codepage", feature = "cp858"))]
    /// OEM Multilingual Latin 1 + Euro symbol
    CP858,
    #[cfg(any(feature = "codepage", feature = "cp860"))]
    /// OEM Portuguese; Portuguese (DOS)
    CP860,
    #[cfg(any(feature = "codepage", feature = "cp861"))]
    /// OEM Icelandic; Icelandic (DOS)
    CP861,
    #[cfg(any(feature = "codepage", feature = "cp862"))]
    /// OEM Hebrew; Hebrew (DOS)
    CP862,
    #[cfg(any(feature = "codepage", feature = "cp863"))]
    /// OEM French Canadian; French Canadian (DOS)
    CP863,
    #[cfg(any(feature = "codepage", feature = "cp864"))]
    /// OEM Arabic; Arabic (864)
    CP864,
    #[cfg(any(feature = "codepage", feature = "cp865"))]
    /// OEM Nordic; Nordic (DOS)
    CP865,
    #[cfg(any(feature = "codepage", feature = "cp866"))]
    /// OEM Russian; Cyrillic (DOS)
    CP866,
    #[cfg(any(feature = "codepage", feature = "cp869"))]
    /// OEM Modern Greek; Greek, Modern (DOS)
    CP869,
    #[cfg(any(feature = "codepage", feature = "cp874"))]
    /// ANSI/OEM Thai (ISO 8859-11); Thai (Windows)
    CP874,
}

impl Codepage {
    pub(crate) fn decode(&self, v: &[u8]) -> String {
        // we could use an expect attribute here, but it would be too hard to read
        #[allow(unused_imports)]
        use oem_cp::{decode_string_complete_table, decode_string_incomplete_table_lossy};

        match self {
            Codepage::CP437 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP437)
            }
            #[cfg(any(feature = "codepage", feature = "cp720"))]
            Codepage::CP720 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP720)
            }
            #[cfg(any(feature = "codepage", feature = "cp737"))]
            Codepage::CP737 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP737)
            }
            #[cfg(any(feature = "codepage", feature = "cp775"))]
            Codepage::CP775 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP775)
            }
            #[cfg(any(feature = "codepage", feature = "cp850"))]
            Codepage::CP850 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP850)
            }
            #[cfg(any(feature = "codepage", feature = "cp852"))]
            Codepage::CP852 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP852)
            }
            #[cfg(any(feature = "codepage", feature = "cp855"))]
            Codepage::CP855 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP855)
            }
            #[cfg(any(feature = "codepage", feature = "cp857"))]
            Codepage::CP857 => {
                decode_string_incomplete_table_lossy(v, &oem_cp::code_table::DECODING_TABLE_CP857)
            }
            #[cfg(any(feature = "codepage", feature = "cp858"))]
            Codepage::CP858 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP858)
            }
            #[cfg(any(feature = "codepage", feature = "cp860"))]
            Codepage::CP860 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP860)
            }
            #[cfg(any(feature = "codepage", feature = "cp861"))]
            Codepage::CP861 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP861)
            }
            #[cfg(any(feature = "codepage", feature = "cp862"))]
            Codepage::CP862 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP862)
            }
            #[cfg(any(feature = "codepage", feature = "cp863"))]
            Codepage::CP863 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP863)
            }
            #[cfg(any(feature = "codepage", feature = "cp864"))]
            Codepage::CP864 => {
                decode_string_incomplete_table_lossy(v, &oem_cp::code_table::DECODING_TABLE_CP864)
            }
            #[cfg(any(feature = "codepage", feature = "cp865"))]
            Codepage::CP865 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP865)
            }
            #[cfg(any(feature = "codepage", feature = "cp866"))]
            Codepage::CP866 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP866)
            }
            #[cfg(any(feature = "codepage", feature = "cp869"))]
            Codepage::CP869 => {
                decode_string_complete_table(v, &oem_cp::code_table::DECODING_TABLE_CP869)
            }
            #[cfg(any(feature = "codepage", feature = "cp874"))]
            Codepage::CP874 => {
                decode_string_incomplete_table_lossy(v, &oem_cp::code_table::DECODING_TABLE_CP874)
            }
        }
    }

    // this might come in handy in the future
    pub(crate) fn encode_char_checked(&self, c: char) -> Option<u8> {
        use oem_cp::encode_char_checked;

        match self {
            Codepage::CP437 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP437),
            #[cfg(any(feature = "codepage", feature = "cp720"))]
            Codepage::CP720 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP720),
            #[cfg(any(feature = "codepage", feature = "cp737"))]
            Codepage::CP737 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP737),
            #[cfg(any(feature = "codepage", feature = "cp775"))]
            Codepage::CP775 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP775),
            #[cfg(any(feature = "codepage", feature = "cp850"))]
            Codepage::CP850 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP850),
            #[cfg(any(feature = "codepage", feature = "cp852"))]
            Codepage::CP852 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP852),
            #[cfg(any(feature = "codepage", feature = "cp855"))]
            Codepage::CP855 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP855),
            #[cfg(any(feature = "codepage", feature = "cp857"))]
            Codepage::CP857 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP857),
            #[cfg(any(feature = "codepage", feature = "cp858"))]
            Codepage::CP858 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP858),
            #[cfg(any(feature = "codepage", feature = "cp860"))]
            Codepage::CP860 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP860),
            #[cfg(any(feature = "codepage", feature = "cp861"))]
            Codepage::CP861 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP861),
            #[cfg(any(feature = "codepage", feature = "cp862"))]
            Codepage::CP862 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP862),
            #[cfg(any(feature = "codepage", feature = "cp863"))]
            Codepage::CP863 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP863),
            #[cfg(any(feature = "codepage", feature = "cp864"))]
            Codepage::CP864 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP864),
            #[cfg(any(feature = "codepage", feature = "cp865"))]
            Codepage::CP865 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP865),
            #[cfg(any(feature = "codepage", feature = "cp866"))]
            Codepage::CP866 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP866),
            #[cfg(any(feature = "codepage", feature = "cp869"))]
            Codepage::CP869 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP869),
            #[cfg(any(feature = "codepage", feature = "cp874"))]
            Codepage::CP874 => encode_char_checked(c, &oem_cp::code_table::ENCODING_TABLE_CP874),
        }
    }
}

impl TryFrom<u16> for Codepage {
    type Error = ();

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            437 => Ok(Codepage::CP437),
            #[cfg(any(feature = "codepage", feature = "cp720"))]
            720 => Ok(Codepage::CP720),
            #[cfg(any(feature = "codepage", feature = "cp737"))]
            737 => Ok(Codepage::CP737),
            #[cfg(any(feature = "codepage", feature = "cp775"))]
            775 => Ok(Codepage::CP775),
            #[cfg(any(feature = "codepage", feature = "cp850"))]
            850 => Ok(Codepage::CP850),
            #[cfg(any(feature = "codepage", feature = "cp852"))]
            852 => Ok(Codepage::CP852),
            #[cfg(any(feature = "codepage", feature = "cp855"))]
            855 => Ok(Codepage::CP855),
            #[cfg(any(feature = "codepage", feature = "cp857"))]
            857 => Ok(Codepage::CP857),
            #[cfg(any(feature = "codepage", feature = "cp858"))]
            858 => Ok(Codepage::CP858),
            #[cfg(any(feature = "codepage", feature = "cp860"))]
            860 => Ok(Codepage::CP860),
            #[cfg(any(feature = "codepage", feature = "cp861"))]
            861 => Ok(Codepage::CP861),
            #[cfg(any(feature = "codepage", feature = "cp862"))]
            862 => Ok(Codepage::CP862),
            #[cfg(any(feature = "codepage", feature = "cp863"))]
            863 => Ok(Codepage::CP863),
            #[cfg(any(feature = "codepage", feature = "cp864"))]
            864 => Ok(Codepage::CP864),
            #[cfg(any(feature = "codepage", feature = "cp865"))]
            865 => Ok(Codepage::CP865),
            #[cfg(any(feature = "codepage", feature = "cp866"))]
            866 => Ok(Codepage::CP866),
            #[cfg(any(feature = "codepage", feature = "cp869"))]
            869 => Ok(Codepage::CP869),
            #[cfg(any(feature = "codepage", feature = "cp874"))]
            874 => Ok(Codepage::CP874),
            _ => Err(()),
        }
    }
}

impl From<Codepage> for u16 {
    fn from(value: Codepage) -> Self {
        match value {
            Codepage::CP437 => 437,
            #[cfg(any(feature = "codepage", feature = "cp720"))]
            Codepage::CP720 => 720,
            #[cfg(any(feature = "codepage", feature = "cp737"))]
            Codepage::CP737 => 737,
            #[cfg(any(feature = "codepage", feature = "cp775"))]
            Codepage::CP775 => 775,
            #[cfg(any(feature = "codepage", feature = "cp850"))]
            Codepage::CP850 => 850,
            #[cfg(any(feature = "codepage", feature = "cp852"))]
            Codepage::CP852 => 852,
            #[cfg(any(feature = "codepage", feature = "cp855"))]
            Codepage::CP855 => 855,
            #[cfg(any(feature = "codepage", feature = "cp857"))]
            Codepage::CP857 => 857,
            #[cfg(any(feature = "codepage", feature = "cp858"))]
            Codepage::CP858 => 858,
            #[cfg(any(feature = "codepage", feature = "cp860"))]
            Codepage::CP860 => 860,
            #[cfg(any(feature = "codepage", feature = "cp861"))]
            Codepage::CP861 => 861,
            #[cfg(any(feature = "codepage", feature = "cp862"))]
            Codepage::CP862 => 862,
            #[cfg(any(feature = "codepage", feature = "cp863"))]
            Codepage::CP863 => 863,
            #[cfg(any(feature = "codepage", feature = "cp864"))]
            Codepage::CP864 => 864,
            #[cfg(any(feature = "codepage", feature = "cp865"))]
            Codepage::CP865 => 865,
            #[cfg(any(feature = "codepage", feature = "cp866"))]
            Codepage::CP866 => 866,
            #[cfg(any(feature = "codepage", feature = "cp869"))]
            Codepage::CP869 => 869,
            #[cfg(any(feature = "codepage", feature = "cp874"))]
            Codepage::CP874 => 874,
        }
    }
}

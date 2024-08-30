package io.kaitai.struct

/**
 * Contains list of encodings supported by Kaitai Struct.
 */
object EncodingList {
  /**
   * Map of encodings supported by Kaitai Struct. Key is the canonical encoding name,
   * and value is a Set of Strings which comprise all possible aliases of such encoding.
   * Canonical name itself doesn't need to appear in the list of aliases, so if there's
   * only one popular spelling of a specific encoding, Set can be empty.
   *
   * Canonical names are case-sensitive. Aliases are matched case insensitively and
   * generate a KSC warning no matter which form was used.
   */
  val canonicalToAlias = Map(
    "ASCII" -> Set("US-ASCII", "US_ASCII", "IBM367", "cp367", "csASCII", "iso-ir-6"),
    "UTF-8" -> Set("UTF8", "UTF_8", "ISO-10646/UTF-8", "ISO-10646/UTF8", "cp65001", "csUTF8", "unicode-1-1-utf-8", "unicode-2-0-utf-8"),
    "UTF-16BE" -> Set("UTF16BE", "UTF16-BE", "UTF-16-BE", "UTF_16BE", "UTF16_BE", "UTF_16_BE", "csUTF16BE"),
    "UTF-16LE" -> Set("UTF16LE", "UTF16-LE", "UTF-16-LE", "UTF_16LE", "UTF16_LE", "UTF_16_LE", "csUTF16LE"),
    "UTF-32BE" -> Set("UTF32BE", "UTF32-BE", "UTF-32-BE", "UTF_32BE", "UTF32_BE", "UTF_32_BE", "csUTF32BE"),
    "UTF-32LE" -> Set("UTF32LE", "UTF32-LE", "UTF-32-LE", "UTF_32LE", "UTF32_LE", "UTF_32_LE", "csUTF32LE"),
    "ISO-8859-1"  -> Set("ISO8859-1",  "ISO_8859-1",  "ISO88591",  "ISO_8859_1",  "ISO8859_1",  "8859_1",  "8859-1",  "88591",  "latin1", "L1", "csISOLatin1", "iso-ir-100", "IBM819", "cp819", "windows-28591"),
    "ISO-8859-2"  -> Set("ISO8859-2",  "ISO_8859-2",  "ISO88592",  "ISO_8859_2",  "ISO8859_2",  "8859_2",  "8859-2",  "88592",  "latin2", "L2", "csISOLatin2", "iso-ir-101", "IBM1111", "windows-28592"),
    "ISO-8859-3"  -> Set("ISO8859-3",  "ISO_8859-3",  "ISO88593",  "ISO_8859_3",  "ISO8859_3",  "8859_3",  "8859-3",  "88593",  "latin3", "L3", "csISOLatin3", "iso-ir-109", "windows-28593"),
    "ISO-8859-4"  -> Set("ISO8859-4",  "ISO_8859-4",  "ISO88594",  "ISO_8859_4",  "ISO8859_4",  "8859_4",  "8859-4",  "88594",  "latin4", "L4", "csISOLatin4", "iso-ir-110", "windows-28594"),
    "ISO-8859-5"  -> Set("ISO8859-5",  "ISO_8859-5",  "ISO88595",  "ISO_8859_5",  "ISO8859_5",  "8859_5",  "8859-5",  "88595",  "cyrillic", "csISOLatinCyrillic", "iso-ir-144", "windows-28595"),
    "ISO-8859-6"  -> Set("ISO8859-6",  "ISO_8859-6",  "ISO88596",  "ISO_8859_6",  "ISO8859_6",  "8859_6",  "8859-6",  "88596",  "arabic", "csISOLatinArabic", "iso-ir-127", "windows-28596", "ECMA-114", "ASMO-708"),
    "ISO-8859-7"  -> Set("ISO8859-7",  "ISO_8859-7",  "ISO88597",  "ISO_8859_7",  "ISO8859_7",  "8859_7",  "8859-7",  "88597",  "greek", "greek8", "csISOLatinGreek", "iso-ir-126", "windows-28597", "ECMA-118", "ELOT_928"),
    "ISO-8859-8"  -> Set("ISO8859-8",  "ISO_8859-8",  "ISO88598",  "ISO_8859_8",  "ISO8859_8",  "8859_8",  "8859-8",  "88598",  "hebrew", "csISOLatinHebrew", "iso-ir-138", "windows-28598"),
    "ISO-8859-9"  -> Set("ISO8859-9",  "ISO_8859-9",  "ISO88599",  "ISO_8859_9",  "ISO8859_9",  "8859_9",  "8859-9",  "88599",  "latin5", "L5", "csISOLatin5", "iso-ir-148", "windows-28599"),
    "ISO-8859-10" -> Set("ISO8859-10", "ISO_8859-10", "ISO885910", "ISO_8859_10", "ISO8859_10", "8859_10", "8859-10", "885910", "latin6", "L6", "csISOLatin6", "iso-ir-157"),
    "ISO-8859-11" -> Set("ISO8859-11", "ISO_8859-11", "ISO885911", "ISO_8859_11", "ISO8859_11", "8859_11", "8859-11", "885911", "thai", "csTIS620", "TIS-620"),
    "ISO-8859-13" -> Set("ISO8859-13", "ISO_8859-13", "ISO885913", "ISO_8859_13", "ISO8859_13", "8859_13", "8859-13", "885913", "latin7", "L7", "csISO885913", "windows-28603"),
    "ISO-8859-14" -> Set("ISO8859-14", "ISO_8859-14", "ISO885914", "ISO_8859_14", "ISO8859_14", "8859_14", "8859-14", "885914", "latin8", "L8", "csISO885914", "iso-ir-199", "iso-celtic"),
    "ISO-8859-15" -> Set("ISO8859-15", "ISO_8859-15", "ISO885915", "ISO_8859_15", "ISO8859_15", "8859_15", "8859-15", "885915", "latin9", "L9", "csISO885915", "windows-28605"),
    "ISO-8859-16" -> Set("ISO8859-16", "ISO_8859-16", "ISO885916", "ISO_8859_16", "ISO8859_16", "8859_16", "8859-16", "885916", "latin10", "L10", "csISO885916"),
    "windows-1250" -> Set("cp1250", "cswindows1250"),
    "windows-1251" -> Set("cp1251", "cswindows1251"),
    "windows-1252" -> Set("cp1252", "cswindows1252"),
    "windows-1253" -> Set("cp1253", "cswindows1253"),
    "windows-1254" -> Set("cp1254", "cswindows1254"),
    "windows-1255" -> Set("cp1255", "cswindows1255"),
    "windows-1256" -> Set("cp1256", "cswindows1256"),
    "windows-1257" -> Set("cp1257", "cswindows1257"),
    "windows-1258" -> Set("cp1258", "cswindows1258"),
    "IBM437" -> Set("cp437", "437", "csPC8CodePage437"),
    "IBM866" -> Set("cp866", "866", "csIBM866"),
    "Shift_JIS" -> Set("Shift-JIS", "ShiftJIS", "S-JIS", "S_JIS", "SJIS", "PCK", "csShiftJIS"),
    "Big5" -> Set("csBig5"),
    "EUC-KR" -> Set("EUCKR", "EUC_KR", "csEUCKR", "korean", "iso-ir-149"),
  )
}

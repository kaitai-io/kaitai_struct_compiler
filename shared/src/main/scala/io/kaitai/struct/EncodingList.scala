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
    "ASCII" -> Set("US-ASCII"),
    "UTF-8" -> Set("UTF8", "ISO-10646/UTF-8", "ISO-10646/UTF8"),
    "UTF-16LE" -> Set("UTF16LE"),
    "UTF-16BE" -> Set("UTF16BE"),
    "UTF-32LE" -> Set("UTF32LE"),
    "UTF-32BE" -> Set("UTF32BE"),
    "ISO-8859-1" -> Set("ISO8859-1", "ISO_8859-1", "ISO88591", "8859_1", "8859-1", "88591", "LATIN1", "IBM819", "CP819", "csISOLatin1", "iso-ir-100", "windows-28591", "WE8ISO8859P1"),
    "ISO-8859-2" -> Set("ISO8859-2", "ISO_8859-2", "ISO88592", "8859_2", "8859-2", "88592", "LATIN2", "IBM1111", "CP1111", "csISOLatin2", "iso-ir-101", "windows-28592"),
    "ISO-8859-3" -> Set("ISO8859-3", "ISO_8859-3", "ISO88593", "8859_3", "8859-3", "88593", "LATIN3", "csISOLatin3", "iso-ir-109", "windows-28593"),
    "ISO-8859-4" -> Set("ISO8859-4", "ISO_8859-4", "ISO88594", "8859_4", "8859-4", "88594", "LATIN4", "csISOLatin4", "iso-ir-110", "windows-28594"),
    "ISO-8859-5" -> Set("ISO8859-5", "ISO_8859-5", "ISO88595", "8859_5", "8859-5", "88595", "csISOLatinCyrillic", "ISO-IR-144", "windows-28595"),
    "ISO-8859-6" -> Set("ISO8859-6", "ISO_8859-6", "ISO88596", "8859_6", "8859-6", "88596", "iso-ir-127", "ECMA-114", "ASMO-708", "csISOLatinArabic", "windows-28596"),
  )
}

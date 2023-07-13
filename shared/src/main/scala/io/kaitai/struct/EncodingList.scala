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
    "US-ASCII" -> Set("ASCII"),
    "UTF-8" -> Set("UTF8", "ISO-10646/UTF-8", "ISO-10646/UTF8"),
    "ISO-8859-1" -> Set("ISO8859-1", "ISO_8859-1", "ISO88591", "8859_1", "8859-1", "88591", "LATIN1"),
    "ISO-8859-2" -> Set("ISO8859-2", "ISO_8859-2", "ISO88592", "8859_2", "8859-2", "88592"),
    "ISO-8859-3" -> Set("ISO8859-3", "ISO_8859-3", "ISO88593", "8859_3", "8859-3", "88593"),
  )
}

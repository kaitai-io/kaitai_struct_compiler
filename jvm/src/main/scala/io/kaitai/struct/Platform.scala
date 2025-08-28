package io.kaitai.struct

import java.util.Locale

object Platform {
  def toUpperLocaleInsensitive(s: String) = s.toUpperCase(Locale.ROOT)
}

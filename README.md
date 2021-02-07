A personal collection of utilities plucked from all over the place.
SBCL-specific.

std.sb.cl.text - Unicode-aware string processing utilities.

Constant `+whitespace-char+` is a vector of all whitespace characters, including those from Unicode.

* `whitespacep (c)` - Determine whether `c` is a whitespace character. Return `T` or `NIL`.
* `ascii-char-p (c)` - Determine whether `c` is an ASCII character.
* `upper-case-string-p (s &key (letters-only nil))` - Determine whether string `s` is all-uppercase. `T` for a zero-length `s`. If `letters-only` is `T`, ignore non-alphabetic characters.


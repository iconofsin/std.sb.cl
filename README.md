A personal collection of utilities plucked from all over the place.
SBCL-specific.

std.sb.cl.text - Unicode-aware string processing utilities.

Constant `+whitespace-char+` is a vector of all whitespace characters, including those from Unicode.

* `whitespacep (c)` - Determine whether `c` is a whitespace character. Return `T` or `NIL`.
* `ascii-char-p (c)` - Determine whether `c` is an ASCII character.
* `upper-case-string-p (s &key (letters-only nil))` - Determine whether string `s` is all-uppercase. `T` for a zero-length `s`. If `letters-only` is `T`, ignore non-alphabetic characters.
* `lower-case-string-p (s &key (letters-only nil))` - Determine whether string `s` is all-lowercase. `T` for a zero-length `s`. If `letters-only` is `T`, ignore non-alphabetic characters.
* `consistent-case-p (s &key (letters-only t))` - Determine whether string `s` has consistent case. Return `:lower` if all-lowercase, `:upper` if all-uppercase, `NIL` if mixed-case, `T` for a zero-length `s`. By default, ignores non-alphabetic characters.
* `downcase (s &key (start 0) end)` - Like `cl:string-downcase`, but return unquoted `NIL` when `s` is `NIL`.
* `upcase (s &key (start 0) end)` - Like `cl:string-upcase`, but return unquoted `NIL` when `s` is `NIL`.
* `capitalize (s &key (start 0) end)` - Like `cl:string-capitalize`, but return unquoted `NIL` when `s` is `NIL`.
* `numeric-p (s)` - Determine whether string `s` only contains digits. Return `NIL` for a zero-length `s`.
* `alpha-p (s)` - Determine whether string `s` only contains alphabetic characters. Depends on SB-UNICODE. Return NIL for a zero-length `s`.
* `alpha-or-numeric-p (c)` - Determine whether character `c` is a digit or an alphabetic character. Depends on SB-UNICODE.
* `alphanumeric-p (s)` - Determine whether string `s` only consists of alphanumeric characters. Return `NIL` for a zero-length `s`. Depends on SB-UNICODE.
* `string-empty-p (s)` - (macro) Determine whether string `s` is `NIL` or zero-length.
* `string-blank-p (s)` - (macro) Determine whether string `s` is `NIL` or only contains whitespace characters.
* `trim-space (s)` - Remove all whitespace on either side of string `s`. Return a new string.
* `trim-space-left (s)` - Remove all leading whitespace in string `s`. Return a new string.
* `trim-space-right (s)` - Remove all trailing whitespace in string `s`. Return a new string.
* `trim* (char-bag s &key (left t) (right t))` - Return a substring of string `s`, with all characters in `char-bag` stripped off the beginning and the end, or either. If both `:left` and `:right` are `NIL`, returns `s` as is. If `char-bag` is `NIL`, returns `NIL`.
* `in-char-bag (char-bag c &key (ignore-case t))` - Determine whether a character `c` is in `char-bag`. `char-bag` is a sequence containing characters. Set `:ignore-case` to `NIL` for a case-sensitive check."
* `collapse-all (s &key (ignore-case t))` - Remove all adjacent duplicate characters in string `s` and return as a new string. This automatically takes care of any non-singular  whitespace, converting every span of whitespace into a single `#\Space`. Set `:ignore-case` to `NIL` for a case-sensitive compression.
* `collapse (char-bag s &key (ignore-case t))` - Remove adjacent duplicate characters in string `s` and return as a new  string. `char-bag` specifies characters to remove. If `NIL`, collapse all duplicates by calling `collapse-all`. Set `:ignore-case` to `NIL` for a case-sensitive compression.
* `collapse-whitespace (s)` - Replace spans of whitespace in string `s` with `#\Space`. See also `+whitespace-chars+`. `collapse-whitespace` is about 20% faster than `cl-ppcre:regex-replace-all` and saves a ton of CONSing as well."
* `concat (&rest strings)` - Shortcut for `(concatenate 'string ...)`
* `toggle-char-case (s)` - Toggle cases of individual characters in string `s` and return as a new string.
* `toggle-case (s &key (process-mixed-case nil))` - Toggle case of string `s`. By default, if `s` is mixed-case, return `s`. Set `:process-mixed-case` to `T` to override this behavior.
* `tokenize (s &key (delimiterp #'whitespacep))` - Find all non-whitespace spans of characters. Return as a list of 'tokens'. Call with a predicate such as notalphanumericp to find 'words'.
* `replace-all (s part replacement &key (test #'char=))` - Replace all occurrences of `part` in string `s` with `replacement`. Return a new string."
* `double-quote (s)` - (macro) Insert double quotes around the contents of `s`. Return as a new string.

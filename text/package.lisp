(defpackage #:std.sb.cl.text
  (:use :cl :std.sb.cl)
  (:export #:+whitespace-chars+
           #:+ellipsis+
           #:whitespacep
           #:ascii-char-p
           #:upper-case-string-p
           #:lower-case-string-p
           #:consistent-case-p
           #:downcase
           #:upcase
           #:capitalize
           #:numeric-p
           #:alpha-p
           #:alpha-or-numeric-p
           #:alphanumeric-p
           #:string-empty-p
           #:string-blank-p
           #:trim-space
           #:trim-space-left
           #:trim-space-right
           #:trim*
           #:in-char-bag
           #:collapse-all
           #:collapse
           #:collapse-whitespace
           #:concat
           #:toggle-case
           #:tokenize
           #:replace-all
           #:double-quote))

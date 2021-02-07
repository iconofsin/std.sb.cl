(in-package :std.sb.cl.text)

(define-constant +whitespace-chars+
  #.(remove-duplicates 
    #(#\Tab #\Newline #\Vt #\Page #\Return #\Space #\Backspace
      #\Next-Line #\Linefeed #\Rubout #\No-Break_Space #\Ogham_Space_Mark
      #\En_Quad #\Em_Quad #\En_Space #\Em_Space #\Three-Per-Em_Space
      #\Four-Per-Em_Space #\Six-Per-Em_Space #\Figure_Space
      #\Punctuation_Space #\Thin_Space #\Hair_Space #\Line_Separator
      #\Paragraph_Separator #\Narrow_No-Break_Space #\Medium_Mathematical_Space
      #\Ideographic_Space #\Mongolian_Vowel_Separator #\Zero_Width_Space
      #\Zero_Width_Non-Joiner #\Zero_Width_Joiner #\Word_Joiner
      #\Zero_Width_No-Break_Space))
  "The definitive whitespace album.")

(define-constant +ellipsis+ "..."
  "The default suffix for `shorten'.")

(defun whitespacep (c)
  "Determine whether `c' is a whitespace character. Return T or NIL."
  (check-type c character)
  (not (not (find c +whitespace-chars+ :test #'char=))))

(defun whitespace-to-space (c)
  "Convert any whitespace character to #\Space. Return `c' unchanged 
if a non-whitespace character."
  (check-type c character)
  (if (whitespacep c)
      #\Space
      c))

(defun ascii-char-p (c)
  "Determine whether `c' is an ASCII character."
  (check-type c character)
  (< (char-code c) 128))

(defun upper-case-string-p (s &key (letters-only nil))
  "Determine whether string `s' is all-uppercase. T for a zero-length `s'. If
letters-only is T, ignore non-alphabetic characters."
  (check-type s string)
  (if letters-only
      (every #'upper-case-p (remove-if-not #'sb-unicode:alphabetic-p s))
      (every #'upper-case-p s)))

(defun lower-case-string-p (s &key (letters-only nil))
  "Determine whether string `s' is all-lowercase. T for a zero-length `s'. If
letters-only is T, ignore non-alphabetic characters."
  (check-type s string)
  (if letters-only
      (every #'lower-case-p (remove-if-not #'sb-unicode:alphabetic-p s))
      (every #'lower-case-p s)))

(defun consistent-case-p (s &key (letters-only t))
  "Determine whether string `s' has consistent case. Return :lower 
if all-lowercase, :upper if all-uppercase, NIL if mixed-case, T for
a zero-length `s'. By default, ignores non-alphabetic characters."
  (cond
    ((lower-case-string-p s :letters-only letters-only) :lower)
    ((upper-case-string-p s :letters-only letters-only) :upper)))

(defun downcase (s &key (start 0) end)
  "Like cl:string-downcase, but return unquoted NIL when `s' is NIL."
  (when s
    (string-downcase s :start start :end end)))

(defun upcase (s &key (start 0) end)
  "Like cl:string-upcase, but return unquoted NIL when `s' is NIL."
  (when s
    (string-upcase s :start start :end end)))

(defun capitalize (s &key (start 0) end)
  "Like cl:string-capitalize, but return unquoted NIL when `s' is NIL."
  (when s
    (string-capitalize s :start start :end end)))

(defun numeric-p (s)
  "Determine whether string `s' only contains digits. Return NIL for a
zero-length `s'."
  (when s
    (if (string-equal "" s)
        nil
        (every #'digit-char-p s))))

(defun alpha-p (s)
  "Determine whether string `s' only contains alphabetic characters.
Depends on SB-UNICODE. Return NIL for a zero-length `s'."
  (when s
    (if (string-equal "" s)
        nil
        (every #'sb-unicode:alphabetic-p s))))

(defun alpha-or-numeric-p (c)
  "Determine whether character `c' is a digit or an alphabetic character.
Depends on SB-UNICODE."
  (check-type c character)
  (or (sb-unicode:alphabetic-p c)
      (digit-char-p c)))

(defun alphanumeric-p (s)
  "Determine whether string `s' only consists of alphanumeric characters.
Return NIL for a zero-length `s'. Depends on SB-UNICODE."
  (when s
    (if (string-equal "" s)
        nil
        (every #'alpha-or-numeric-p s))))

(defmacro string-empty-p (s)
  "Determine whether string `s' is NIL or zero-length."
  `(or (null ,s) (string-equal "" ,s)))

(defmacro string-blank-p (s)
  "Determine whether string `s' is NIL or only contains whitespace characters."
  `(or (null ,s) (every #'whitespacep ,s)))

(defun trim-space (s)
  "Remove all whitespace on either side of string `s'. Return a new string."
  (check-type s string)
  (string-trim +whitespace-chars+ s))

(defun trim-space-left (s)
  "Remove all leading whitespace in string `s'. Return a new string."
  (check-type s string)
  (string-left-trim +whitespace-chars+ s))

(defun trim-space-right (s)
  "Remove all trailing whitespace in string `s'. Return a new string."
  (check-type s string)
  (string-right-trim +whitespace-chars+ s))

(defun trim* (char-bag s &key (left t) (right t))
  "Return a substring of string `s', with all characters in char-bag
stripped off the beginning and the end, or either. If both :left and 
:right are nil, returns s as is. If char-bag is NIL, returns NIL."
  (check-type s string)
  (when (and s char-bag)
    (cond ((and left right)
           (string-trim char-bag s))
          (left
           (string-left-trim char-bag s))
          (right
           (string-right-trim char-bag s))
          (t
           s))))

(defun in-char-bag (char-bag c &key (ignore-case t))
  "Determine whether a character `c' is in `char-bag'. `char-bag' is a 
sequence containing characters. Set :ignore-case to NIL for a case-sensitive 
check."
  (check-type c character)
  (let ((compfn (if ignore-case #'char-equal #'char=)))
    (typecase char-bag
      (list
       (loop :for bag-char :in char-bag
             :thereis (funcall compfn bag-char c)))
      (simple-string
       (loop :for bag-char :across char-bag
             :thereis (funcall compfn bag-char c)))
      (simple-vector
       (loop :for bag-char :across char-bag
             :thereis (funcall compfn bag-char c)))
      (vector
       (loop :for bag-char :across char-bag
             :thereis (funcall compfn bag-char c)))
      (t
       (find c char-bag :test compfn)))))

(defun collapse-all (s &key (ignore-case t))
  "Remove all adjacent duplicate characters in string `s' and return 
as a new string. This automatically takes care of any non-singular 
whitespace, converting every span of whitespace into a single #\Space.
Set :ignore-case to NIL for a case-sensitive compression."
  (check-type s string)
  (let ((compfn (if ignore-case #'char-equal #'char=)))
    (with-output-to-string (stream)
      (let ((last-char (code-char 0)))
        (loop :for c across s :do
              (unless (funcall compfn last-char (whitespace-to-space c))
                (setf last-char (whitespace-to-space c))
                (princ last-char stream)))))))

(defun collapse (char-bag s &key (ignore-case t))
  "Remove adjacent duplicate characters in string `s' and return as a new 
string. `char-bag' specifies characters to remove. If NIL, collapse all
duplicates by calling collapse-all. Set :ignore-case to NIL for a case-
sensitive compression."
  (check-type s string)
  (if (null char-bag)
      (collapse-all s :ignore-case ignore-case)
      (let ((compfn (if ignore-case #'char-equal #'char=)))
        (with-output-to-string (stream)
          (let ((last-char (code-char 0)))
            (loop :for c across s :do
                  (if (in-char-bag char-bag c)
                      (unless (funcall compfn last-char (whitespace-to-space c))
                        (setf last-char (whitespace-to-space c))
                        (princ last-char stream))
                      (progn
                        (setf last-char (whitespace-to-space c))
                        (princ last-char stream)))))))))

(defun collapse-whitespace (s)
  "Replace spans of whitespace in string `s' with #\Space. See also 
+whitespace-chars+. collapse-whitespace is about 20% faster than 
cl-ppcre:regex-replace-all and saves a ton of CONSing as well."
  (check-type s string)
  (collapse +whitespace-chars+ s))

(defun concat (&rest strings)
  "Shortcut for (concatenate 'string ...)"
  (apply #'concatenate 'string strings))

(defun toggle-char-case (s)
  "Toggle cases of individual characters in string `s' and return 
as a new string."
  (check-type s string)
  (with-output-to-string (stream)
    (loop :for c across s :do
          (if (upper-case-p c)
              (princ (char-downcase c) stream)
              (princ (char-upcase c) stream)))))

(defun toggle-case (s &key (process-mixed-case nil))
  "Toggle case of string `s'. By default, if `s' is mixed-case, 
return s. Set process-mixed-case to T to override this behavior."
  (check-type s string)
  (case (consistent-case-p s)
    (:upper (downcase s))
    (:lower (upcase s))
    (t
     (if process-mixed-case
         (toggle-char-case s)
         s))))

(defun tokenize (s &key (delimiterp #'whitespacep))
  "Find all non-whitespace spans of characters. Return as a list of 'tokens'.
Call with a predicate such as notalphanumericp to find 'words'."
  (check-type s string)
  (check-type delimiterp function)
  (loop :for beg = (position-if-not delimiterp s)
        :then (position-if-not delimiterp s :start (1+ end))
        :for end = (and beg (position-if delimiterp s :start beg))
        :when beg :collect (subseq s beg end)
        :while end))

(defun replace-all (s part replacement &key (test #'char=))
  "Replace all occurrences of `part' in string `s' with `replacement'.
Return a new string."
  (check-type s string)
  (check-type part string)
  (check-type replacement string)
  (assert (> (length part) 0))
  (with-output-to-string (stream)
    (loop :with part-length = (length part)
          :for old-pos = 0 then (+ pos part-length)
          :for pos = (search part s
                            :start2 old-pos
                            :test test)
          :do (write-string s stream
                           :start old-pos
                           :end (or pos (length s)))
          :when pos do (write-string replacement stream)
          :while pos)))

(defmacro double-quote (s)
  "Insert double quotes around the contents of `s'. Return as a new string."
  `(concat "\"" ,s "\""))

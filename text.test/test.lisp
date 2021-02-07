(in-package :std.sb.cl.text.test)

(fiveam:def-suite text-whitespace)
(fiveam:def-suite text-ascii-char-p)
(fiveam:def-suite text-upper-case-string-p)
(fiveam:def-suite text-lower-case-string-p)
(fiveam:def-suite text-consistent-case-p)
(fiveam:def-suite text-downcase)
(fiveam:def-suite text-upcase)
(fiveam:def-suite text-capitalize)
(fiveam:def-suite text-numeric-p)
(fiveam:def-suite text-alpha-p)
(fiveam:def-suite text-alpha-or-numeric-p)
(fiveam:def-suite text-alphanumeric-p)
(fiveam:def-suite text-string-empty-p)
(fiveam:def-suite text-string-blank-p)
(fiveam:def-suite text-trim-space)
(fiveam:def-suite text-trim-space-left)
(fiveam:def-suite text-trim-space-right)
(fiveam:def-suite text-trim*)
(fiveam:def-suite text-in-char-bag)
(fiveam:def-suite text-collapse-all)
(fiveam:def-suite text-collapse)
(fiveam:def-suite text-whitespace)
(fiveam:def-suite text-concat)
(fiveam:def-suite text-shorten)
(fiveam:def-suite text-toggle-case)
(fiveam:def-suite text-tokenize)
(fiveam:def-suite text-replace-all)
(fiveam:def-suite text-double-quote)
(fiveam:def-suite text-dq)


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-whitespace)

;; any whitespace must convert to #\Space
(test conversion
  (is
   (every #'(lambda (c)
                (char= c #\Space))
          (map 'list
               #'std.sb.cl.text::whitespace-to-space
               +whitespace-chars+))))

;; any other character converts to itself
(test ignoring-1
  (is
   (char= #\A (std.sb.cl.text::whitespace-to-space #\A))))

(test ignoring-2
  (is (char= #\Circled_Hangul_Ieung
             (std.sb.cl.text::whitespace-to-space #\Circled_Hangul_Ieung))))

;; a non-character or nil signals type error
(test type-error-1
  (signals simple-type-error
    (std.sb.cl.text::whitespace-to-space "string")))

(test type-error-2
  (signals simple-type-error
    (std.sb.cl.text::whitespace-to-space nil)))


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-ascii-char-p)

(test positives
  (is
   (every #'identity
          (loop :for i :from 0 :to 127
                :collect (ascii-char-p (code-char i))))))

(test negatives
  (is
   (notany #'identity
           (loop :for i :from 128 :to 22250
                 :collect (ascii-char-p (code-char i))))))

;; a non-character or nil signals type error
(test type-error-1
  (signals simple-type-error
    (ascii-char-p "")))

(test type-error-2
  (signals simple-type-error
    (ascii-char-p nil)))


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-upper-case-string-p)

(test upper
  (is (upper-case-string-p "КАДАБРА")))

(test notupper
  (is-false (upper-case-string-p "кадабра")))

(test mixed
  (is-false (upper-case-string-p "АБРа")))

(test empty
  (is (upper-case-string-p "")))

(test letters-only
  (is (upper-case-string-p "54311 ДАДА" :letters-only t)))

;; a non-character or nil signals type error
(test type-error-1
  (signals simple-type-error
    (upper-case-string-p #\Space)))

(test type-error-2
  (signals simple-type-error
    (upper-case-string-p nil)))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-lower-case-string-p)

(test lower
  (is (lower-case-string-p "кадабра")))

(test notlower
  (is-false (lower-case-string-p "КАДАБРА")))

(test mixed
  (is-false (lower-case-string-p "АБРа")))

(test empty
  (is (lower-case-string-p "")))

(test letters-only
  (is (lower-case-string-p "кадабра 54311" :letters-only t)))

;; a non-character or nil signals type error
(test type-error-1
  (signals simple-type-error
    (lower-case-string-p #\Space)))

(test type-error-2
  (signals simple-type-error
    (lower-case-string-p nil)))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-consistent-case-p)

(test consistent-upper-1
  (is (consistent-case-p "STRING")))

(test consistent-upper-2
  (is (eq :upper (consistent-case-p "STRING"))))

(test consistent-lower-1
  (is (consistent-case-p "string")))

(test consistent-lower-2
  (is (eq :lower (consistent-case-p "string"))))

(test inconsistent
  (is-false (consistent-case-p "СТрОКА")))

(test empty
  (is-true (consistent-case-p "")))

(test letters-only-1
  (is (consistent-case-p "абрамс танк это вам не т34")))

(test letters-only-2
  (is-false (consistent-case-p "абрамс танк это вам не т34" :letters-only nil)))

(test letters-only-3
  (is (eq :upper
          (consistent-case-p "ВЫШАК 3093$"))))


;; a non-character or nil signals type error
(test type-error-1
  (signals simple-type-error
    (lower-case-string-p #\Space)))

(test type-error-2
  (signals simple-type-error
    (lower-case-string-p nil)))


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-downcase)

(test regular
  (is (string= "abracadabra" (downcase "aBrAcADABra"))))

(test empty-string
  (is (string= "" (downcase ""))))

(test nil-string
  (is (null (downcase nil))))

(test regular-bound-1
  (is (string= "abracadaBra"
               (downcase "aBrAcADABra" :start 0 :end 8))))

(test regular-bound-2
  (is (string= "aBracadabra"
               (downcase "aBrAcADABra" :start 3))))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-upcase)

(test regular
  (is (string= "АБРАКАДАБРА" (upcase "абраКАДаБрА"))))

(test empty-string
  (is (string= "" (upcase ""))))

(test nil-string
  (is (null (upcase nil))))


(test regular-bound-1
  (is (string= "АБРАКАДАБрА"
               (upcase "абраКАДаБрА" :start 0 :end 8))))

(test regular-bound-2
  (is (string= "абрАКАДАБРА"
               (upcase "абраКаДаБрА" :start 3))))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-capitalize)

(test regular
  (is (string= "Абра Кадабра" (capitalize "абра КАДАБРА"))))

(test empty-string
  (is (string= "" (capitalize ""))))

(test nil-string
  (is (null (capitalize nil))))

(test regular-bound-1
  (is (string= "абра Кадабра" (capitalize "абра КАДАБРА" :start 5))))

(test regular-bound-1
  (is (string= "Абра КАДАБРА" (capitalize "абра КАДАБРА" :end 5))))


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-numeric-p)

(test empty-string
  (is-false (numeric-p "")))

(test positive
  (is-true (numeric-p "123455")))

(test negative
  (is-false (numeric-p "1234a33")))

(test nil-string
  (is (null (numeric-p nil))))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-alpha-p)

(test empty-string
  (is-false (alpha-p "")))

(test negative
  (is-false (alpha-p "abragкудабра1")))

(test positive
  (is-true (alpha-p "сюдабра")))

(test nil-string
  (is (null (alpha-p nil))))


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-alpha-or-numeric-p)

(test nil-character
  (signals simple-type-error (alpha-or-numeric-p nil)))

(test positive-1
  (is-true (alpha-or-numeric-p #\A)))

(test positive-2
  (is-true (alpha-or-numeric-p #\4)))

(test negative
  (is-false (alpha-or-numeric-p #\,)))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-alphanumeric-p)

(test negative
  (is-false (alphanumeric-p "-абрау")))

(test nil-string
  (is (null (alphanumeric-p nil))))

(test empty-string
  (is (null (alphanumeric-p ""))))


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-string-empty-p)

(test positive-1
  (is-true (string-empty-p nil)))

(test positive-2
  (is-true (string-empty-p "")))

(test negative
  (is-false (string-empty-p " ")))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-string-blank-p)

(test positive-1
  (is-true (string-blank-p nil)))

(test positive-2
  (is-true (string-blank-p "    ")))

(test negative
  (is-false (string-blank-p "f")))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-trim-space)

(test normal-execution-1
  (is (string= "" (trim-space ""))))

(test normal-execution-2
  (is (string= "abc" (trim-space "abc   "))))

(test normal-execution-3
  (is (string= "abc" (trim-space "   abc"))))

(test normal-execution-4
  (is (string= "abc" (trim-space "   abc   "))))

(test normal-execution-5
  (is (string= "abc" (trim-space "abc"))))

(test signal-type-error
  (signals simple-type-error
    (trim-space nil)))


;;;
;; ---------------------------------------------------------
;;;
(in-suite text-trim-space-left)

(test normal-execution-1
  (is (string= "" (trim-space-left ""))))

(test normal-execution-2
  (is (string= "abc   " (trim-space-left "abc   "))))

(test normal-execution-3
  (is (string= "abc" (trim-space-left "   abc"))))

(test normal-execution-4
  (is (string= "abc   " (trim-space-left "   abc   "))))

(test normal-execution-5
  (is (string= "abc" (trim-space-left "abc"))))

(test signal-type-error
  (signals simple-type-error
    (trim-space-left nil)))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-trim-space-right)

(test normal-execution-1
  (is (string= "" (trim-space-right ""))))

(test normal-execution-2
  (is (string= "abc" (trim-space-right "abc   "))))

(test normal-execution-3
  (is (string= "   abc" (trim-space-right "   abc"))))

(test normal-execution-4
  (is (string= "   abc" (trim-space-right "   abc   "))))

(test normal-execution-5
  (is (string= "abc" (trim-space-right "abc"))))

(test signal-type-error
  (signals simple-type-error
    (trim-space-right nil)))



;;;
;; ---------------------------------------------------------
;;;
(in-suite text-trim*)

(test as-is
  (is (string= "  StRiNgS  "
               (trim* +whitespace-chars+
                      "  StRiNgS  " :left nil :right nil))))

(test right-side-trim*
  (is (string= "  StRiNg"
               (trim* "S "
                      "  StRiNgS  " :left nil))))

(test left-side-trim*
  (is (string= "tRiNgS  "
               (trim* " S"
                      "  StRiNgS  " :right nil))))

(test trim*
  (is (string= "tRiNg"
               (trim* "S "
                      "  StRiNgS  "))))

(test nil-ret
  (is (null (trim* nil "  StR  "))))

(test signal-type-error
  (signals simple-type-error
    (trim* "trim" nil)))



;;;
;; ---------------------------------------------------------
;;;
(fiveam:def-suite text-in-char-bag)

(test as-string-1
  (is (in-char-bag "ABC " #\A)))

(test as-string-2
  (is-false (in-char-bag "ABC " #\b :ignore-case nil)))

(test as-string-3
  (is (in-char-bag "ABC " #\b)))

(test as-string-empty
  (is-false (in-char-bag "" #\Space)))

(test as-nil-string
  (is-false (in-char-bag nil #\Space)))

(test as-list-1
  (is (in-char-bag (list #\Space #\D #\m) #\M)))

(test as-list-empty
  (is-false (in-char-bag (list) #\Space)))

(test as-vector
  (is (in-char-bag #(#\f #\g) #\f)))

(test as-vector-sensitive
  (is-false (in-char-bag #(#\f #\g) #\F :ignore-case nil)))

(test as-nil-character
  (signals simple-type-error (in-char-bag (list nil) nil)))

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-collapse-all)

(test regular
  (is (string= " "
               (collapse-all (concatenate 'string "" (list #\Space #\Vt))))))

(test regular-1
  (is (string= "ab cDM"
               (collapse-all "aaaAAbb ccDdMmm"))))

(test regular-2
  (is (string= "aAb cDdMm"
               (collapse-all "aaaAAbb ccDdMmm" :ignore-case nil))))

(test regular-empty
  (is (string= "" (collapse-all ""))))

(test nil-string
  (signals simple-type-error
    (collapse-all nil)))

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-collapse)

(test regular-1
  (is (string= "ab BcCd"
               (collapse "ab " "aaaaabbbbbB   BBcCd"))))

(test regular-2
  (is (string= "aaaaabbbbbB   BBcCd"
               (collapse "" "aaaaabbbbbB   BBcCd"))))

(test regular-3
  (is (string= "abB BcCd"
               (collapse "ab " "aaaaabbbbbB   BBcCd" :ignore-case nil))))

(test nil-all
  (signals simple-type-error
    (collapse nil nil)))

(test nil-string
  (signals simple-type-error
    (collapse "anbc " nil)))

(test nil-bag-regular
  (is (string= " "
               (collapse nil (concatenate 'string "" (list #\Space #\Vt))))))

(test nil-bag-regular-1
  (is (string= "ab cDM"
               (collapse nil "aaaAAbb ccDdMmm"))))

(test nil-bag-regular-2
  (is (string= "aAb cDdMm"
               (collapse nil "aaaAAbb ccDdMmm" :ignore-case nil))))

(test nil-bag-regular-empty
  (is (string= "" (collapse nil ""))))

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-whitespace)

(test regular-1
  (is (string= ""
               (collapse-whitespace ""))))

(test regular-2
  (is (string= " ab nor mal "
               (collapse-whitespace
                (concatenate 'string (list #\Page #\Space)
                             " " "ab     nor     mal          ")))))

(test nil-string
  (signals simple-type-error
    (collapse-whitespace nil)))

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-concat)

;; - ignore for now

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-toggle-case)

(test uppercase-string
  (is (string= "абракадабра "
               (toggle-case "АБРАКАДАБРА "))))

(test lowecase-string
  (is (string= "SPLINTER CELLS "
               (toggle-case "splinter cells "))))

(test mixed-string-default
  (is (string= "иЗвИниТи за НиРоВнЫй ПоЧеРк"
               (toggle-case "иЗвИниТи за НиРоВнЫй ПоЧеРк"))))

(test mixed-string-toggle
  (is (string= "ИзВиНИтИ ЗА нИрОвНыЙ пОчЕрК"
               (toggle-case
                "иЗвИниТи за НиРоВнЫй ПоЧеРк" :process-mixed-case t))))

(test nil-string
  (signals simple-type-error
    (toggle-case nil)))

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-tokenize)

(test empty-string
  (is (null (tokenize ""))))

(test regular
  (is 
   (every #'identity
    (mapcar #'(lambda (a b)
                (string= a b))
            (list "раз" "два" "три" "четыре" "5ть")
            (tokenize "раз два    три четыре   5ть")))))

(test nil-string
  (signals simple-type-error
    (tokenize nil)))

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-replace-all)

(test regular-1
  (is (string= ""
               (replace-all "" "a" "b"))))

(test regular-2
  (is (string= "repped"
               (replace-all "relled" "l" "p"))))

(test regular-3
  (is (string= "reed"
               (replace-all "relled" "l" ""))))

(test regular-empty-part
  (signals simple-error
    (replace-all "repped" "" "p")))


(test nil-string-s
  (signals simple-type-error
    (replace-all nil "a" "b")))

(test nil-string-part
  (signals simple-type-error
    (replace-all "string" nil "b")))

(test nil-string-replacement
  (signals simple-type-error
    (replace-all "string" "a" nil)))

;;;
;; ---------------------------------------------------------
;;;
(in-suite text-double-quote)

(test regular-1
  (is (string= "\"\""
               (double-quote nil))))

(test regular-2
  (is (string= "\"text\""
               (double-quote "text"))))



;;
;; RUN ALL
;; 

;(run-all-tests)


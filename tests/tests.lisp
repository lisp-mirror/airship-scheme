;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme/tests)

(defun read-scheme* (string)
  (with-input-from-string (stream string)
    (scheme::read-scheme stream)))

(5am:def-suite airship-scheme/tests)

(5am:def-suite airship-scheme/scheme-read
  :in airship-scheme/tests)

(5am:in-suite airship-scheme/scheme-read)

(5am:test boolean-syntax
  "Are true and false read correctly?"
  (is (eq (read-scheme* "#t") t))
  (is (eq (read-scheme* "#true") t))
  (is (eq (read-scheme* "#TrUe") t))
  (is (eq (read-scheme* "#tRuE") t))
  (is (eq (read-scheme* "#TRUE") t))
  (is (eq (car (read-scheme* "(#t)")) t))
  (is (eq (car (read-scheme* "(#true)")) t))
  (is (eq (read-scheme* "#f") %scheme-boolean:f))
  (is (eq (read-scheme* "#false") %scheme-boolean:f))
  (is (eq (read-scheme* "#FaLsE") %scheme-boolean:f))
  (is (eq (read-scheme* "#fAlSe") %scheme-boolean:f))
  (is (eq (read-scheme* "#FALSE") %scheme-boolean:f))
  (is (eq (car (read-scheme* "(#f)")) %scheme-boolean:f))
  (is (eq (car (read-scheme* "(#false)")) %scheme-boolean:f))
  (is (eq '%scheme-boolean:f %scheme-boolean:f)))

;;; Note: This should match the list of numbers in examples/syntax.scm
(5am:test numeric-syntax
  "Are numbers read correctly?"
  (is (eql (read-scheme* "1")
           1))
  (is (eql (read-scheme* "+1")
           1))
  (is (eql (read-scheme* "-1")
           -1))
  (is (eql (read-scheme* "1.0")
           1.0d0))
  (is (eql (read-scheme* "4.")
           4.0d0))
  (is (eql (read-scheme* ".4")
           0.4d0))
  (is (eql (read-scheme* "-.4")
           -0.4d0))
  (is (eql (read-scheme* "+.4")
           0.4d0))
  (is (eql (read-scheme* "1.0f0")
           1.0f0))
  (is (eql (read-scheme* "-1.2s3")
           -1.2s3))
  (is (eql (read-scheme* "1.0l-1")
           1.0l-1))
  (is (eql (read-scheme* "3d1")
           3d1))
  (is (eql (read-scheme* "87e2")
           87d2))
  (is (eql (read-scheme* "4/3")
           4/3))
  (is (eql (read-scheme* "-123/456")
           -123/456))
  (is (eql (read-scheme* "-3/2")
           -3/2))
  (is (eql (read-scheme* "-4i")
           #C(0 -4)))
  (is (eql (read-scheme* "+3i")
           #C(0 3)))
  (is (eql (read-scheme* "7i")
           #C(0 7)))
  (is (eql (read-scheme* "4-i")
           #C(4 -1)))
  (is (eql (read-scheme* "9+i")
           #C(9 1)))
  (is (eql (read-scheme* "4/3-3/4i")
           #C(4/3 -3/4)))
  (is (eql (read-scheme* "3e4-4e4i")
           #C(3d4 -4d4)))
  (is (eql (read-scheme* "-4.0f30+3.0f20i")
           #C(-4.0f30 3.0f20)))
  (is (eql (read-scheme* "-1+4i")
           #C(-1 4)))
  (is (eql (read-scheme* "+4-3i")
           #C(4 -3)))
  (is (eql (read-scheme* "3+2i")
           #C(3 2)))
  (let* ((z (read-scheme* "4@5"))
         (z* (* 4d0 (cis 5d0)))
         (z-z* (- z z*)))
    (is (and (typep z '(complex double-float))
             (< (abs (realpart z-z*)) double-float-epsilon)
             (< (abs (imagpart z-z*)) double-float-epsilon))))
  (let* ((z (read-scheme* "-3.0@+4e3"))
         (z* (* -3d0 (cis 4d3)))
         (z-z* (- z z*)))
    (is (and (typep z '(complex double-float))
             (< (abs (realpart z-z*)) double-float-epsilon)
             (< (abs (imagpart z-z*)) double-float-epsilon))))
  (let* ((z (read-scheme* "-321.0f-3@+432f12"))
         (z* (* -321.0f-3 (cis 432f12)))
         (z-z* (- z z*)))
    (is (and (typep z '(complex single-float))
             (< (abs (realpart z-z*)) single-float-epsilon)
             (< (abs (imagpart z-z*)) single-float-epsilon)))))

(5am:test infnan-syntax
  "Are the 'infnan' numbers read correctly?"
  (is (eql (read-scheme* "+inf.0")
           f:double-float-positive-infinity))
  (is (eql (read-scheme* "-inf.0")
           f:double-float-negative-infinity))
  (let ((nan (read-scheme* "+nan.0")))
    (is (and (scheme::nanp nan)
             (typep nan 'double-float)
             (not (scheme::sign-bit? nan)))))
  (let ((nan (read-scheme* "-nan.0")))
    (is (and (scheme::nanp nan)
             (typep nan 'double-float)
             (scheme::sign-bit? nan))))
  (is (eql (read-scheme* "+inf.0f0")
           f:single-float-positive-infinity))
  (is (eql (read-scheme* "-inf.0f0")
           f:single-float-negative-infinity))
  (let ((nan (read-scheme* "+nan.0f0")))
    (is (and (scheme::nanp nan)
             (typep nan 'single-float)
             (not (scheme::sign-bit? nan)))))
  (let ((nan (read-scheme* "-nan.0f0")))
    (is (and (scheme::nanp nan)
             (typep nan 'single-float)
             (scheme::sign-bit? nan))))
  (is (eql (read-scheme* "+inf.0+inf.0i")
           (complex f:double-float-positive-infinity
                    f:double-float-positive-infinity)))
  (let ((z (read-scheme* "+inf.0-nan.0i")))
    (is (eql (realpart z)
             f:double-float-positive-infinity))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'double-float)
               (scheme::sign-bit? i)))))
  (let ((z (read-scheme* "+nan.0+inf.0i")))
    (is (eql (imagpart z)
             f:double-float-positive-infinity))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'double-float)
               (not (scheme::sign-bit? x))))))
  (is (eql (read-scheme* "4-inf.0i")
           (complex 4.0d0
                    f:double-float-negative-infinity)))
  (let ((z (read-scheme* "-12+nan.0f0i")))
    (is (eql (realpart z)
             -12.0f0))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'single-float)
               (not (scheme::sign-bit? i))))))
  (is (eql (read-scheme* "+inf.0-3i")
           (complex f:double-float-positive-infinity
                    -3.0d0)))
  (let ((z (read-scheme* "-nan.0+42i")))
    (is (eql (imagpart z)
             42.0d0))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'double-float)
               (scheme::sign-bit? x)))))
  (let ((z (read-scheme* "-nan.0f0-333i")))
    (is (eql (imagpart z)
             -333.0f0))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'single-float)
               (scheme::sign-bit? x)))))
  (let ((z (read-scheme* "-nan.0f0+inf.0f0i")))
    (is (eql (imagpart z)
             f:single-float-positive-infinity))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'single-float)
               (scheme::sign-bit? x)))))
  (is (eql (read-scheme* "+inf.0f0+22i")
           (complex f:single-float-positive-infinity
                    22f0)))
  (let ((z (read-scheme* "+inf.0f0-nan.0f0i")))
    (is (eql (realpart z)
             f:single-float-positive-infinity))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'single-float)
               (scheme::sign-bit? i)))))
  (let ((z (read-scheme* "333+nan.0f0i")))
    (is (eql (realpart z)
             333.0f0))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'single-float)
               (not (scheme::sign-bit? i))))))
  (is (eql (read-scheme* "+inf.0i")
           (complex 0d0
                    f:double-float-positive-infinity)))
  (is (eql (read-scheme* "-inf.0f0i")
           (complex 0f0
                    f:single-float-negative-infinity)))
  (let ((z (read-scheme* "-nan.0i")))
    (is (eql (realpart z) 0d0))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'double-float)
               (scheme::sign-bit? i)))))
  (let ((z (read-scheme* "+nan.0d0i")))
    (is (eql (realpart z) 0d0)
        (let ((i (imagpart z)))
          (is (and (scheme::nanp i)
                   (typep i 'double-float)
                   (not (scheme::sign-bit? i)))))))
  (let ((z (read-scheme* "4@+inf.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "-7@-nan.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (eql (read-scheme* "+inf.0f0@111")
       (complex f:single-float-negative-infinity
                f:single-float-negative-infinity))
  (eql (read-scheme* "+inf.0@-3")
       (complex f:double-float-negative-infinity
                f:double-float-negative-infinity))
  (let ((z (read-scheme* "+nan.0@42")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "+inf.0@-inf.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "-nan.0@+nan.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "+nan.0f0@-nan.0f0")))
    (is (and (typep z '(complex single-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z))))))

(5am:test read-bases-and-exactness
  "Are numbers in different bases and exactness read correctly?"
  (is (eql (read-scheme* "#x42") 66))
  (is (eql (read-scheme* "#xFFF") 4095))
  (is (eql (read-scheme* "#Xfff") 4095))
  (is (eql (read-scheme* "#XffF") 4095))
  (is (eql (read-scheme* "#b101") 5))
  (is (eql (read-scheme* "#o777") 511))
  (is (eql (read-scheme* "#d1999") 1999))
  (is (eql (read-scheme* "#xabcdef") 11259375))
  (is (eql (read-scheme* "#i33") 33.0d0))
  (is (eql (read-scheme* "#e876") 876))
  (is (eql (read-scheme* "#e876.0") 876))
  (is (eql (read-scheme* "#e32.1") 321/10))
  (is (eql (read-scheme* "#x#iee") 238.0d0))
  (is (eql (read-scheme* "#i#xcd") 205.0d0))
  (is (eql (read-scheme* "#e#x1a") 26))
  (is (eql (read-scheme* "#x#e93fc3a") 9698362))
  (is (eql (read-scheme* "#i#o4321") 2257.0d0))
  (is (eql (read-scheme* "#b#i1110101010100001") 60065.0d0))
  (is (eql (read-scheme* "#e1e500") (expt 10 500)))
  (is (eql (read-scheme* "#e#d1e500") (expt 10 500)))
  (is (eql (read-scheme* "#d#e1e500") (expt 10 500))))

(5am:test read-symbols
  "Are symbols read correctly?"
  (is (string= (symbol-name (read-scheme* "hello"))
               "HELLO"))
  (is (string= (symbol-name (read-scheme* "WORLD"))
               "world"))
  (is (string= (symbol-name (read-scheme* "TeSt"))
               "tEsT"))
  (is (string= (symbol-name (read-scheme* "|hello|"))
               "HELLO"))
  (is (string= (symbol-name (read-scheme* "|HELLO|"))
               "hello"))
  (is (string= (symbol-name (read-scheme* "|This is a sentence.|"))
               "tHIS IS A SENTENCE."))
  (is (string= (symbol-name (read-scheme* "||"))
               ""))
  (is (string= (symbol-name (read-scheme* "|foo\\nbar|"))
               (format nil "FOO~%BAR")))
  (is (string= (symbol-name (read-scheme* "|foo\\tbar|"))
               "FOO	BAR"))
  (is (string= (symbol-name (read-scheme* (format nil "|foo \\~%bar|")))
               "FOO BAR"))
  (is (string= (symbol-name (read-scheme* "|escaped bar: \\||"))
               "ESCAPED BAR: \|")))

(defun quoted? (form)
  (and (listp form) (eql (car form) 'quote) (cdr form) (endp (cddr form))))

(defun quote-contents (quoted-form)
  (cadr quoted-form))

(5am:test read-quoted
  "Are quoted things read correctly?"
  (let ((quoted-a (read-scheme* "'a")))
    (is (and (quoted? quoted-a)
             (eql (quote-contents quoted-a) 'a))))
  (let* ((double-quoted-a (read-scheme* "''a"))
         (quoted-a (quote-contents double-quoted-a)))
    (is (and (quoted? double-quoted-a)
             (quoted? quoted-a)
             (eql (quote-contents quoted-a) 'a))))
  (is (equalp (read-scheme* "'(0 1 1 2 3 5)") ''(0 1 1 2 3 5))))

(5am:test string-escaped-characters
  "Do literal strings correctly handle escaped characters?"
  (is (char= (char (read-scheme* "\"\\n\"") 0) (code-char #x000a)))
  (is (char= (char (read-scheme* "\"\\t\"") 0) (code-char #x0009)))
  (is (char= (char (read-scheme* "\"\\a\"") 0) (code-char #x0007)))
  (is (char= (char (read-scheme* "\"\\b\"") 0) (code-char #x0008)))
  (is (char= (char (read-scheme* "\"\\r\"") 0) (code-char #x000d)))
  (let ((backslash (read-scheme* "\"\\\\\"")))
    (is (and (= 1 (length backslash))
             (char= (char backslash 0) #\\))))
  (is (string= (read-scheme* "\"escaped double quote: \\\"\"")
               "escaped double quote: \""))
  (is (char= (char (read-scheme* "\"\\x42;\"") 0) #\B))
  (is (string= (read-scheme* (format nil "\"foo \\~%bar\""))
               "foo bar"))
  (is (string= (read-scheme* (format nil "\"foo \\   ~%bar\""))
               "foo bar"))
  (is (string= (read-scheme* (format nil "\"foo \\	~%bar\""))
               "foo bar"))
  (is (string= (read-scheme* (format nil "\"foo \\ 	 ~%bar\""))
               "foo bar")))

(5am:test literal-characters
  "Are the literal characters read properly?"
  (is (char= (read-scheme* "#\\alarm")     (code-char #x0007)))
  (is (char= (read-scheme* "#\\backspace") (code-char #x0008)))
  (is (char= (read-scheme* "#\\delete")    (code-char #x007f)))
  (is (char= (read-scheme* "#\\escape")    (code-char #x001b)))
  (is (char= (read-scheme* "#\\newline")   (code-char #x000a)))
  (is (char= (read-scheme* "#\\null")      (code-char #x0000)))
  (is (char= (read-scheme* "#\\return")    (code-char #x000d)))
  (is (char= (read-scheme* "#\\space")     (char " " 0)))
  (is (char= (read-scheme* "#\\tab")       (char "	" 0)))
  (is (char= (read-scheme* "#\\x53")       #\S))
  (is (char= (read-scheme* "#\\X79")       #\y))
  (is (char= (read-scheme* "#\\x221E")     #\∞))
  (is (char= (read-scheme* "#\\xe9")       #\é)))

(5am:test read-sequences
  "Does Airship Scheme correctly read sequences?"
  (let ((scheme-sequence (read-scheme* "(a b c d e f g 1 2 3)"))
        (lisp-sequence '(a b c d e f g 1 2 3)))
    (is (and (typep scheme-sequence 'list)
             (= (length scheme-sequence) (length lisp-sequence))
             (every #'eql scheme-sequence lisp-sequence))))
  (is (equalp (read-scheme* "(a . b)") '(a . b)))
  (is (equalp (read-scheme* "(1 2 . 3)") '(1 2 . 3)))
  (let ((scheme-sequence (read-scheme* "\"z y x Z Y X\""))
        (lisp-sequence "z y x Z Y X"))
    (is (and (typep scheme-sequence 'simple-string)
             (string= scheme-sequence lisp-sequence))))
  (let ((scheme-sequence (read-scheme* "#(9 8 7 a b c)"))
        (lisp-sequence #(9 8 7 a b c)))
    (is (and (typep scheme-sequence 'simple-vector))
        (= (length scheme-sequence) (length lisp-sequence))
        (every #'eql scheme-sequence lisp-sequence)))
  (let ((scheme-sequence (read-scheme* "#u8(99 98 97)"))
        (lisp-sequence (make-array 3
                                   :element-type '(unsigned-byte 8)
                                   :initial-contents '(99 98 97))))
    (is (and (typep scheme-sequence '(simple-array (unsigned-byte 8) (*))))
        (= (length scheme-sequence) (length lisp-sequence))
        (every #'= scheme-sequence lisp-sequence)))
  (let ((scheme-sequence (read-scheme* "#u8(#x0f #xfe #xed #xdc #xcb)"))
        (lisp-sequence (make-array 5
                                   :element-type '(unsigned-byte 8)
                                   :initial-contents '(#x0f #xfe #xed #xdc #xcb))))
    (is (and (typep scheme-sequence '(simple-array (unsigned-byte 8) (*))))
        (= (length scheme-sequence) (length lisp-sequence))
        (every #'= scheme-sequence lisp-sequence))))

(defmacro scheme (expression)
  (destructuring-bind (symbol &rest rest) expression
    `(,(intern (symbol-name symbol) 'r7rs) #'identity ,@rest)))

(defmacro scheme* (expression)
  (destructuring-bind (symbol &rest rest) expression
    `(,(intern (symbol-name symbol) 'r7rs) #'values ,@rest)))

;;; TODO: equivalence predicates

(5am:test arithmetic
  "Are the arithmetic procedures correct?"
  (is (eql (scheme (+ 1)) 1))
  (is (eql (scheme (+ 8 3)) 11))
  (is (eql (scheme (+ 5 4 -3)) 6))
  (is (eql (scheme (+)) 0))
  (is (eql (scheme (- 42)) -42))
  (is (eql (scheme (- 4321 1234)) 3087))
  (is (eql (scheme (*)) 1))
  (is (eql (scheme (* 32)) 32))
  (is (eql (scheme (* 28 48)) 1344))
  (is (eql (scheme (/ 4)) 1/4))
  (is (eql (scheme (/ 3 7)) 3/7))
  (is (eql (scheme (/ 46 2)) 23))
  (is (eql (scheme (/ 46d0 2d0)) 23d0))
  (is (eql (scheme (abs -479678)) 479678))
  (is (eql (scheme (abs 742)) 742))
  (is (eql (scheme (abs -674.578d0)) 674.578d0))
  (is (eql (scheme (abs 976.798d0)) 976.798d0))
  (is (equal (multiple-value-list (scheme* (floor/ 17 8)))
             (list 2 1))))

(defun boolean-true? (expression)
  "
Returns T if the expression is both true and a boolean. This is
necessary because all values are true other than NIL. This ensures
that all Common Lisp implementations return the same result for
Airship Scheme, which isn't guaranteed without this predicate.
"
  (eql expression t))

(defun scheme-not (expression)
  "Returns T if the expression is false in Scheme."
  (eql expression %scheme-boolean:f))

(5am:test numerical-predicates
  "Are the numerical predicates correct?"
  (is (boolean-true? (scheme (number? 42))))
  (is (scheme-not (scheme (number? "hello"))))
  (is (boolean-true? (scheme (complex? 58d0))))
  (is (boolean-true? (scheme (complex? #C(3.0f0 2.0f0)))))
  (is (boolean-true? (scheme (real? 3))))
  (is (scheme-not (scheme (real? #C(842 546)))))
  (is (boolean-true? (scheme (rational? 8))))
  (is (boolean-true? (scheme (rational? 73/2))))
  (is (boolean-true? (scheme (integer? 259361371606))))
  (is (boolean-true? (scheme (integer? 259361371606.0f0))))
  (is (boolean-true? (scheme (integer? 259361371606.0d0))))
  (is (boolean-true? (scheme (exact-integer? 259361371606))))
  (is (scheme-not (scheme (exact-integer? 259361371606.0f0))))
  (is (scheme-not (scheme (exact-integer? 259361371606.0d0))))
  (is (and (boolean-true? (scheme (exact? 22/7)))
           (scheme-not (scheme (inexact? 22/7)))))
  (is (and (scheme-not (scheme (exact? 3.14f0)))
           (boolean-true? (scheme (inexact? 3.14f0)))))
  (is (and (scheme-not (scheme (exact? 3.14d0)))
           (boolean-true? (scheme (inexact? 3.14d0)))))
  (is (and (boolean-true? (scheme (finite? 749856)))
           (scheme-not (scheme (infinite? 749856)))
           (scheme-not (scheme (nan? 749856)))))
  (is (and (scheme-not (scheme (finite? (read-scheme* "+inf.0"))))
           (boolean-true? (scheme (infinite? (read-scheme* "+inf.0"))))
           (scheme-not (scheme (nan? (read-scheme* "+inf.0"))))))
  (is (and (scheme-not (scheme (finite? (read-scheme* "+nan.0"))))
           (scheme-not (scheme (infinite? (read-scheme* "+nan.0"))))
           (boolean-true? (scheme (nan? (read-scheme* "+nan.0"))))))
  (is (and (boolean-true? (scheme (zero? 0)))
           (boolean-true? (scheme (zero? 0.0f0)))
           (boolean-true? (scheme (zero? 0.0d0)))
           (boolean-true? (scheme (zero? (read-scheme* "0.0+0.0i"))))
           (boolean-true? (scheme (zero? (read-scheme* "0.0f0+0.0f0i"))))))
  (is (and (scheme-not (scheme (positive? 0)))
           (scheme-not (scheme (negative? 0)))))
  (is (and (scheme-not (scheme (zero? 42)))
           (scheme-not (scheme (zero? -1d0)))))
  (is (and (boolean-true? (scheme (positive? 42.0f0)))
           (scheme-not (scheme (positive? -42.0f0)))))
  (is (and (boolean-true? (scheme (negative? -42.0f0)))
           (scheme-not (scheme (negative? 42.0f0)))))
  (is (and (boolean-true? (scheme (even? 0)))
           (scheme-not (scheme (odd? 0)))))
  (is (and (boolean-true? (scheme (even? 756)))
           (scheme-not (scheme (odd? 756)))))
  (is (and (boolean-true? (scheme (odd? -99)))
           (scheme-not (scheme (even? -99))))))

;;; TODO: the rest of the number procedures

;;; TODO: number->string string->number

;;; TODO: booleans
;;; TODO: pairs
;;; TODO: lists
;;; TODO: symbols
;;; TODO: characters

;;; TODO: string<? string-ci<? string>? string-ci>? string<=?
;;; string-ci<=? string>=? string-ci>=?
;;;
;;; TODO: string-upcase string-downcase string-foldcase substring
;;; string-append string->list list->string string-copy string-copy!
(5am:test strings
  "Are the string procedures correct?"
  (is (boolean-true? (scheme (string? "Hello"))))
  (is (scheme-not (scheme (string? #(0 1 2)))))
  (is (scheme-not (scheme (string? 42))))
  (is (scheme-not (scheme (string? '(1 2 3)))))
  (is (scheme-not (scheme (string? #(#\A #\B #\C)))))
  (is (scheme-not (scheme (string? '(#\A #\B #\C)))))
  (let ((s (scheme (make-string 5))))
    (is (boolean-true? (scheme (string? s))))
    (is (= 5 (length s) (scheme (string-length s)))))
  (let ((s (scheme (make-string 5 #\Z))))
    (is (and (boolean-true? (scheme (string=? s "ZZZZZ")))
             (boolean-true? (scheme (string=? "ZZZZZ" s)))))
    (is (and (scheme-not (scheme (string=? s "zzzzz")))
             (scheme-not (scheme (string=? "zzzzz" s)))))
    (is (scheme-not (scheme (string=? s "ZZZ"))))
    (is (and (boolean-true? (scheme (string-ci=? "ZZZZZ" s)))
             (boolean-true? (scheme (string-ci=? s "ZZZZZ")))))
    (is (and (boolean-true? (scheme (string-ci=? "zzzzz" s)))
             (boolean-true? (scheme (string-ci=? s "zzzzz"))))))
  (let ((s (scheme (string #\a #\b #\c))))
    (is (and (string= s "abc")
             (scheme (string=? s "abc"))))
    (is (and (eql #\a (scheme (string-ref s 0)))
             (eql #\b (scheme (string-ref s 1)))
             (eql #\c (scheme (string-ref s 2)))))
    (progn
      (scheme (string-set! s 1 #\B))
      (is (and (string= s "aBc")
               (scheme (string=? s "aBc"))
               (eql #\B (scheme (string-ref s 1)))
               (eql #\B (aref s 1))))
      (scheme (string-fill! s #\z))
      (is (and (string= s "zzz")
               (scheme (string=? s "zzz"))
               (eql #\z (scheme (string-ref s 2)))
               (eql #\z (aref s 2))))
      (scheme (string-fill! s #\x 1))
      (is (and (string= s "zxx")
               (scheme (string=? s "zxx"))
               (eql #\x (scheme (string-ref s 2)))
               (eql #\x (aref s 2))))
      (scheme (string-fill! s #\y 1 2))
      (is (and (string= s "zyx")
               (scheme (string=? s "zyx"))
               (eql #\y (scheme (string-ref s 1)))
               (eql #\x (aref s 2)))))))

;;; TODO: vectors, bytevectors

;;; TODO: 6.10 6.11 6.12 6.13 6.14

;;; TODO: cl-environment and SRFI 112

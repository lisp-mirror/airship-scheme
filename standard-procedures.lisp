(in-package #:airship-scheme)

;;;; Standard procedures
;;;;
;;;; These are the standard procedures built into r7rs-small Scheme,
;;;; as described in chapter 6 of Revised^7 Report on the Algorithmic
;;;; Language Scheme, as found in r7rs.pdf. These are implemented in
;;;; Common Lisp macros, intended to be called from within the Scheme
;;;; runtime.
;;;;
;;;; Don't compose Scheme procedures here. This file is for the
;;;; built-in standard procedures that are implemented directly
;;;; through Common Lisp. It's possible that some of these might be
;;;; moved to a Scheme file if they are best implemented directly in
;;;; Scheme.
;;;;
;;;; Try to keep these as simple as possible. If a lot of complexity
;;;; is needed, consider writing a helper CL function that implements
;;;; the Scheme semantics. This will make things easier to test.

;;;; todo: add type checks where type checks are needed
;;;; todo: add errors when errors are required
;;;; todo: any entry that is commented out is incomplete

;;;; 6.1 - Equivalence predicates
;;;;
;;;; Note: These have to be implemented last to make sure that they
;;;; follow Scheme equivalence rules.

;;; (eqv? obj1 obj2)
;;; (eq? obj1 obj2)
;;; (equal? obj1 obj2)

;;;; 6.2 - Numbers

;;; Numerical predicates

(define-scheme-predicate (number? obj)
  (numberp obj))

(define-scheme-predicate (complex? obj)
  (numberp obj))

(define-scheme-predicate (real? obj)
  (realp obj))

(define-scheme-predicate (rational? obj)
  (or (rationalp obj) (floatp obj)))

(define-scheme-predicate (integer? obj)
  (or (integerp obj)
      (and (numberp obj) (zerop (second-value (round obj))))))

(define-scheme-predicate (exact? z)
  (exactp z))

(define-scheme-predicate (inexact? z)
  (not (exactp z)))

(define-scheme-predicate (exact-integer? z)
  (integerp z))

(define-scheme-predicate (finite? z)
  (finitep z))

(define-scheme-predicate (infinite? z)
  (infinitep z))

(define-scheme-predicate (nan? z)
  (nanp z))

;;; Basic numerical procedures

(define-scheme-predicate (= z . more-numbers)
  (apply #'= z more-numbers))

(define-scheme-predicate (< x . more-numbers)
  (apply #'< x more-numbers))

(define-scheme-predicate (> x . more-numbers)
  (apply #'> x more-numbers))

(define-scheme-predicate (<= x . more-numbers)
  (apply #'<= x more-numbers))

(define-scheme-predicate (>= x . more-numbers)
  (apply #'>= x more-numbers))

(define-scheme-predicate (zero? z)
  (zerop z))

(define-scheme-predicate (positive? x)
  (plusp x))

(define-scheme-predicate (negative? x)
  (minusp x))

(define-scheme-predicate (odd? n)
  (oddp n))

(define-scheme-predicate (even? n)
  (evenp n))

(define-scheme-procedure (max x . more-numbers)
  (apply #'max x more-numbers))

(define-scheme-procedure (min x . more-numbers)
  (apply #'min x more-numbers))

(define-scheme-procedure (+ . numbers)
  (apply #'+ numbers))

(define-scheme-procedure (* . numbers)
  (apply #'* numbers))

(define-scheme-procedure (- z . more-numbers)
  (apply #'- z more-numbers))

(define-scheme-procedure (/ z . more-numbers)
  (apply #'/ z more-numbers))

(define-scheme-procedure (abs x)
  (abs x))

;;; More numerical procedures

(define-scheme-procedure (floor/ n1 n2)
  (floor n1 n2))

(define-scheme-procedure (floor-quotient n1 n2)
  (first-value (floor n1 n2)))

(define-scheme-procedure (floor-remainder n1 n2)
  (mod n1 n2))

(define-scheme-procedure (truncate/ n1 n2)
  (truncate n1 n2))

(define-scheme-procedure (truncate-quotient n1 n2)
  (first-value (truncate n1 n2)))

(define-scheme-procedure (truncate-remainder n1 n2)
  (rem n1 n2))

(define-scheme-procedure (quotient n1 n2)
  (first-value (truncate n1 n2)))

(define-scheme-procedure (remainder n1 n2)
  (rem n1 n2))

(define-scheme-procedure (modulo n1 n2)
  (mod n1 n2))

(define-scheme-procedure (gcd . integers)
  (apply #'gcd integers))

(define-scheme-procedure (lcm . integers)
  (apply #'lcm integers))

(define-scheme-procedure (numerator q)
  (numerator q))

(define-scheme-procedure (denominator q)
  (denominator q))

(define-scheme-procedure (floor x)
  (first-value (floor x)))

(define-scheme-procedure (ceiling x)
  (first-value (ceiling x)))

(define-scheme-procedure (truncate x)
  (first-value (truncate x)))

(define-scheme-procedure (round x)
  (first-value (round x)))

;;; (rationalize x y)

(define-scheme-procedure (exp z)
  (exp z))

(define-scheme-procedure (log z &optional base)
  (if base
      (log z base)
      (log z)))

(define-scheme-procedure (sin z)
  (sin z))

(define-scheme-procedure (cos z)
  (cos z))

(define-scheme-procedure (tan z)
  (tan z))

(define-scheme-procedure (asin z)
  (asin z))

(define-scheme-procedure (acos z)
  (acos z))

(define-scheme-procedure (atan y &optional x)
  (if x
      (atan y x)
      (atan y)))

(define-scheme-procedure (square x)
  (expt x 2))

(define-scheme-procedure (sqrt x)
  (sqrt x))

(define-scheme-procedure (exact-integer-sqrt k)
  (let* ((s (isqrt k))
         (r (- k (expt s 2))))
    (values s r)))

(define-scheme-procedure (expt x1 x2)
  (expt x1 x2))

(define-scheme-procedure (make-rectangular x1 x2)
  (complex x1 x2))

(define-scheme-procedure (make-polar x1 x2)
  (* x1 (cis x2)))

(define-scheme-procedure (real-part z)
  (realpart z))

(define-scheme-procedure (imag-part z)
  (imagpart z))

(define-scheme-procedure (magnitude z)
  (abs z))

(define-scheme-procedure (angle z)
  (phase z))

(define-scheme-procedure (inexact z)
  (if (complexp z)
      (coerce z '(complex double-float))
      (coerce z 'double-float)))

(define-scheme-procedure (exact z)
  (if (complexp z)
      (complex (round (realpart z))
               (round (imagpart z)))
      (round z)))

;;; Input and output

;;; (number->string z &optional radix)
;;; (string->number string &optional radix)

;;;; 6.3 - Booleans

(define-scheme-predicate (not obj)
  (eq obj '%scheme-boolean:false))

(define-scheme-predicate (boolean? obj)
  (scheme-boolean-p obj))

(define-scheme-predicate (boolean=? . booleans)
  (cond ((null booleans) t)
        ((eq (car booleans) t)
         (every (lambda (x) (eq x t)) booleans))
        ((eq (car booleans) '%scheme-boolean:false)
         (every (lambda (x) (eq x '%scheme-boolean:false)) booleans))
        (t '%scheme-boolean:false)))

;;;; 6.4 - Pairs and lists

;;; Basic cons pair procedures

(define-scheme-predicate (pair? obj)
  (consp obj))

(define-scheme-procedure (cons obj1 obj2)
  (cons obj1 obj2))

(define-scheme-procedure (set-car! pair obj)
  (setf (car pair) obj))

(define-scheme-procedure (set-cdr! pair obj)
  (setf (cdr pair) obj))

;;; main library cxr procedures

(define-scheme-cxr (car pair))
(define-scheme-cxr (cdr pair))
(define-scheme-cxr (caar pair))
(define-scheme-cxr (cadr pair))
(define-scheme-cxr (cdar pair))
(define-scheme-cxr (cddr pair))

;;; cxr library procedures

(define-scheme-cxr (caaaar pair))
(define-scheme-cxr (caaadr pair))
(define-scheme-cxr (caaar pair))
(define-scheme-cxr (caadar pair))
(define-scheme-cxr (caaddr pair))
(define-scheme-cxr (caadr pair))
(define-scheme-cxr (cadaar pair))
(define-scheme-cxr (cadadr pair))
(define-scheme-cxr (cadar pair))
(define-scheme-cxr (caddar pair))
(define-scheme-cxr (cadddr pair))
(define-scheme-cxr (caddr pair))
(define-scheme-cxr (cdaaar pair))
(define-scheme-cxr (cdaadr pair))
(define-scheme-cxr (cdaar pair))
(define-scheme-cxr (cdadar pair))
(define-scheme-cxr (cdaddr pair))
(define-scheme-cxr (cdadr pair))
(define-scheme-cxr (cddaar pair))
(define-scheme-cxr (cddadr pair))
(define-scheme-cxr (cddar pair))
(define-scheme-cxr (cdddar pair))
(define-scheme-cxr (cddddr pair))
(define-scheme-cxr (cdddr pair))

;;; List procedures

(define-scheme-predicate (null? obj)
  (null obj))

(define-scheme-predicate (list? obj)
  (proper-list-p obj))

(define-scheme-procedure (make-list k &optional fill)
  (if fill
      (make-list k :initial-element fill)
      (make-list k)))

(define-scheme-procedure (list . obj)
  (apply #'list obj))

(define-scheme-procedure (length list)
  (length list))

(define-scheme-procedure (append . lists)
  (apply #'append lists))

(define-scheme-procedure (reverse list)
  (reverse list))

(define-scheme-procedure (list-tail list k)
  (nthcdr k list))

(define-scheme-procedure (list-ref list k)
  (nth k list))

(define-scheme-procedure (list-set! list k obj)
  (setf (nth k list) obj))

;;; (memq obj list)
;;; (memv obj list)
;;; (member obj list)

;;; (assq obj alist)
;;; (assv obj alist)
;;; (assoc obj alist &optional compare)

(define-scheme-procedure (list-copy obj)
  (copy-list obj))

;;;; 6.5 Symbols

(define-scheme-predicate (symbol? obj)
  (scheme-symbol-p obj))

;;; (symbol=? . symbols)
;;; (symbol->string symbol)
;;; (string->symbol string)

;;;; 6.6 Characters

;;; fixme: a unicode portability library might have to be used here

(define-scheme-predicate (char? obj)
  (typep obj 'character))

(define-scheme-predicate (char=? char . more-chars)
  (apply #'char= char more-chars))

(define-scheme-predicate (char<? char . more-chars)
  (apply #'char< char more-chars))

(define-scheme-predicate (char>? char . more-chars)
  (apply #'char> char more-chars))

(define-scheme-predicate (char<=? char . more-chars)
  (apply #'char<= char more-chars))

(define-scheme-predicate (char>=? char . more-chars)
  (apply #'char>= char more-chars))

(define-scheme-predicate (char-ci=? char . more-chars)
  (apply #'char-equal char more-chars))

(define-scheme-predicate (char-ci<? char . more-chars)
  (apply #'char-lessp char more-chars))

(define-scheme-predicate (char-ci>? char . more-chars)
  (apply #'char-greaterp char more-chars))

(define-scheme-predicate (char-ci<=? char . more-chars)
  (apply #'char-not-greaterp char more-chars))

(define-scheme-predicate (char-ci>=? char . more-chars)
  (apply #'char-not-lessp char more-chars))

(define-scheme-predicate (char-alphabetic? char)
  #+sbcl
  (not (not (sb-unicode:alphabetic-p char)))
  #-sbcl
  (error "Support for this procedure in this CL implementation has not yet been added."))

;;; (char-numeric? char)

(define-scheme-predicate (char-whitespace? char)
  #+sbcl
  (not (not (sb-unicode:whitespace-p char)))
  #-sbcl
  (error "Support for this procedure in this CL implementation has not yet been added."))

;;; (char-upper-case? letter)
;;; (char-lower-case? letter)

;;; (digit-value char)

;;; (char->integer char)
;;; (integer->char n)

;;; (char-upcase char)
;;; (char-downcase char)
;;; (char-foldcase char)

;;;; 6.7 Strings

(define-scheme-predicate (string? obj)
  (stringp obj))

(define-scheme-procedure (make-string k &optional char)
  (if char
      (make-string k :initial-element char)
      (make-string k)))

(define-scheme-procedure (string . char)
  (make-array (length char) :element-type 'character :initial-contents char))

(define-scheme-procedure (string-length string)
  (check-type string simple-string)
  (length string))

(define-scheme-procedure (string-ref string k)
  (char string k))

(define-scheme-procedure (string-set! string k char)
  (setf (char string k) char))

;;; (string=? . strings)
;;; (string-ci=? . strings)
;;; (string<? . strings)
;;; (string-ci<? . strings)
;;; (string>? . strings)
;;; (string-ci>? . strings)
;;; (string<=? . strings)
;;; (string-ci<=? . strings)
;;; (string>=? . strings)
;;; (string-ci>=? . strings)

(define-scheme-procedure (string-upcase string)
  #+sbcl
  (sb-unicode:uppercase string)
  #-sbcl
  (string-upcase string))

(define-scheme-procedure (string-downcase string)
  #+sbcl
  (sb-unicode:lowercase string)
  #-sbcl
  (string-downcase string))

(define-scheme-procedure (string-foldcase string)
  #+sbcl
  (sb-unicode:casefold string)
  #-sbcl
  (error "This procedure is not implemented for this implementation."))

(define-scheme-procedure (substring string start end)
  (subseq string start end))

(define-scheme-procedure (string-append . string)
  (apply #'concatenate 'string string))

(define-scheme-procedure (string->list string &optional start end)
 (subseq-coerce string 'list start end))

(define-scheme-procedure (list->string list)
  (coerce list 'string))

(define-scheme-procedure (string-copy string &optional start end)
  (copy-seq-or-subseq string start end))

(define-scheme-procedure (string-copy! to at from &optional (start 0) end)
  (replace to from :start1 at :start2 start :end2 end))

(define-scheme-procedure (string-fill! string fill &optional (start 0) end)
  (fill string fill :start start :end end))

;;;; 6.8 Vectors

(define-scheme-predicate (vector? obj)
  (typep obj 'scheme-vector))

(define-scheme-procedure (make-vector k &optional fill)
  (if fill
      (make-array k :initial-element fill)
      (make-array k)))

(define-scheme-procedure (vector . obj)
  (apply #'vector obj))

(define-scheme-procedure (vector-length vector)
  (check-type vector simple-vector)
  (length vector))

(define-scheme-procedure (vector-ref vector k)
  (svref vector k))

(define-scheme-procedure (vector-set! vector k obj)
  (setf (svref vector k) obj))

(define-scheme-procedure (vector->list vector &optional start end)
  (subseq-coerce vector 'list start end))

(define-scheme-procedure (list->vector list)
  (coerce list 'simple-vector))

(define-scheme-procedure (vector->string vector &optional start end)
  (subseq-coerce vector 'string start end))

(define-scheme-procedure (string->vector string &optional start end)
  (subseq-coerce string 'vector start end))

(define-scheme-procedure (vector-copy vector &optional start end)
  (copy-seq-or-subseq vector start end))

(define-scheme-procedure (vector-copy! to at from &optional (start 0) end)
  (replace to from :start1 at :start2 start :end2 end))

(define-scheme-procedure (vector-append . vector)
  (apply #'concatenate 'simple-vector vector))

(define-scheme-procedure (vector-fill! vector fill &optional (start 0) end)
  (fill vector fill :start start :end end))

;;;; 6.9 Bytevectors

(define-scheme-predicate (bytevector? obj)
  (typep obj 'bytevector))

(define-scheme-procedure (make-bytevector k &optional byte)
  (if byte
      (make-array k :element-type '(unsigned-byte 8) :initial-element byte)
      (make-array k :element-type '(unsigned-byte 8))))

(define-scheme-procedure (bytevector . byte)
  (make-array (length byte) :element-type '(unsigned-byte 8) :initial-contents byte))

(define-scheme-procedure (bytevector-length bytevector)
  (check-type bytevector bytevector)
  (length bytevector))

(define-scheme-procedure (bytevector-u8-ref bytevector k)
  (check-type bytevector bytevector)
  (aref bytevector k))

(define-scheme-procedure (bytevector-u8-set! bytevector k byte)
  (check-type bytevector bytevector)
  (setf (aref bytevector k) byte))

(define-scheme-procedure (bytevector-copy bytevector &optional start end)
  (check-type bytevector bytevector)
  (copy-seq-or-subseq bytevector start end))

(define-scheme-procedure (bytevector-copy! to at from &optional start end)
  (replace to from :start1 at :start2 start :end2 end))

(define-scheme-procedure (bytevector-append . bytevector)
  (apply #'concatenate '(simple-array (unsigned-byte 8) (*)) bytevector))

(define-scheme-procedure (utf8->string bytevector &optional (start 0) end)
  (octets-to-string bytevector :start start :end end))

(define-scheme-procedure (string->utf8 string &optional (start 0) end)
  (string-to-octets string :start start :end end))

;;;; 6.10 Control features

;;; (procedure? obj)
;;; (apply proc arg . args)
;;; (map proc list . lists)
;;; (string-map proc string . strings)
;;; (vector-map proc vector . vectors)
;;; (for-each proc list . lists)
;;; (string-for-each proc string . strings)
;;; (vector-for-each proc vector . vectors)
;;; (call-with-current-continuation proc)
;;; (call/cc proc)
;;; (values . objs)
;;; (call-with-values producer consumer)
;;; (dynamic-wind before thunk after)

;;;; 6.11 Exceptions

;;; (with-exception-hander handler thunk)
;;; (raise obj)
;;; (raise-continuable obj)
;;; (error message . objs)
;;; (error-object? obj)
;;; (error-object-message error-object)
;;; (error-object-irritants error-object)
;;; (read-error? obj)
;;; (file-error? obj)

;;;; 6.12 Environments and evaluation

;;; (environment . lists)
;;; (scheme-report-environment version)
;;; (null-environment version)
;;; (interaction-environment)
;;; (eval expr-or-def environment-specifier)

;;;; 6.13 Input and output

;;; Ports

;;; (call-with-port port proc)
;;; (call-with-input-file string proc)
;;; (call-with-output-file string proc)
;;; (input-port? obj)
;;; (output-port? obj)
;;; (textual-port? obj)
;;; (binary-port? obj)
;;; (port? obj)
;;; (input-port-open? port)
;;; (output-port-open? port)
;;; (current-input-port)
;;; (current-output-port)
;;; (current-error-port)
;;; (with-input-from-file string thunk)
;;; (with-output-to-file string thunk)
;;; (open-input-file string)
;;; (open-binary-input-file string)
;;; (open-output-file string)
;;; (open-binary-output-file string)
;;; (close-port port)
;;; (close-input-port port)
;;; (close-output-port port)
;;; (open-input-string string)
;;; (open-output-string)
;;; (get-output-string port)
;;; (open-input-bytevector bytevector)
;;; (open-output-bytevector)
;;; (get-output-bytevector port)

;;; Input

;;; (read &optional port)
;;; (read-char &optional port)
;;; (peek-char &optional port)
;;; (read-line &optional port)
;;; (eof-object? obj)
;;; (eof-object)
;;; (char-ready? &optional port)
;;; (read-string k &optional port)
;;; (read-u8 &optional port)
;;; (peek-u8 &optional port)
;;; (u8-ready? &optional port)
;;; (read-bytevector k &optional port)
;;; (read-bytevector! bytevector &optional port start end)

;;; Output

;;; (write obj &optional port)
;;; (write-shared obj &optional port)
;;; (write-simple obj &optional port)
;;; (display obj &optional port)
;;; (newline &optional port)
;;; (write-char char &optional port)
;;; (write-string string &optional port start end)
;;; (write-u8 byte &optional port)
;;; (write-bytevector bytevector &optional port start end)
;;; (flush-output-port &optional port)

;;;; 6.14 System interface

;;; (load filename &optional environment-specifier)
;;; (file-exists? filename)
;;; (delete-file filename)
;;; (command-line)
;;; (exit &optional obj)
;;; (emergency-exit &optional obj)
;;; (get-environment-variable &optional name)
;;; (current-second)
;;; (current-jiffy)
;;; (jiffies-per-second)
;;; (features)

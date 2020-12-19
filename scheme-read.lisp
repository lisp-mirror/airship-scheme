;;;; -*- mode: common-lisp; -*-

;;; Note: This uses :SKIP and :EOF as special symbols. When this
;;; Scheme is extended to support CL-style keyword syntax, these will
;;; have to be renamed to avoid confusion with reading the actual
;;; keywords :SKIP and :EOF.

(cl:in-package #:airship-scheme)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +read-base+ 10
    "
Integers are read in +read-base+ unless a prefix specifies otherwise.
This is similar to CL's *read-base*, but is a constant. This might be
replaced with a variable in the future.
")
  (defconstant +flonum-base+ 10
    "Flonums (floating point numbers) must always be in base 10."))

(defparameter *fold-case* nil
  "
Determines if `string-foldcase' should be applied by default or not.

Note that this is different than the case-inversion that is always
done for compatibility with the upcasing reader of Common Lisp. That
one is done character-by-character and does not apply to most of
Unicode, but is (hopefully) always reversible, unlike Unicode.
")

;;;; Conditions

(define-condition scheme-reader-error (error)
  ((%details
    :initarg :details
    :reader details
    :initform "An error occurred in the Scheme reader"))
  (:report (lambda (condition stream)
             (format stream "~A" (details condition))))
  (:documentation "An error in the Scheme reader, i.e. invalid syntax."))

(define-condition scheme-reader-eof-error (scheme-reader-error)
  ((%details
    :initarg :details
    :reader details
    :initform nil))
  (:report (lambda (condition stream)
             (format stream "Unexpected EOF read")
             (when (details condition)
               (format stream " ~A" (details condition)))))
  (:documentation "An error in the Scheme reader where an unexpected EOF was read"))

(define-condition scheme-type-error (type-error)
  ((%details
    :initarg :details
    :reader details
    :initform nil))
  (:report (lambda (condition stream)
             (format stream "Type error")
             (when (details condition)
               (format stream " ~A" (details condition)))
             (format stream "; expected type: ~A datum: ~A"
                     (type-error-expected-type condition)
                     (type-error-datum condition))))
  (:documentation "A type error internal to the Scheme runtime."))

;;; This should come up when an identifier tries to start with an
;;; invalid Unicode character. This is in section 7.1.1 of r7rs.pdf
(defun unicode-reader-error ()
  (error 'scheme-reader-error
         :details #.(concatenate 'string
                                 "An identifier cannot start with a Unicode character "
                                 "in the general categories of Nd, Mc, or Me. To portably "
                                 "do this, first wrap the identifier in vertical lines "
                                 "like |foo|.")))

;;; Note: A configuration option could make this a style warning for
;;; added compatibility.
(defun whitespace-in-u8-error (whitespace?)
  (error-unless whitespace?
                'scheme-reader-error
                :details #.(concatenate 'string
                                        "In a strict interpretation of the R7RS-small "
                                        "standard in section 7.1.1, whitespace between #u8 "
                                        "and its parentheses is not permitted.")))

(defun invalid-infnan-error ()
  (error 'scheme-reader-error
         :details #.(concatenate 'string
                                 "Tokens that start with \"+inf.\", \"-inf.\", "
                                 "\"+nan.\", and \"-nan.\" that are not currently "
                                 "valid numeric syntax are reserved for future "
                                 "expansions to the Airship Scheme numeric syntax.")))

;;; A simple macro for a simple EOF error.
(defmacro eof-error (details)
  `(error 'scheme-reader-eof-error :details ,details))

;;;; Characters and types

(deftype complex-number-separator ()
  "
A complex number is either written in rectangular notation (with
either + or - as the separator) or in polar notation (with @ as the
separator).
"
  `(member #\+ #\- #\@))

(define-function (complex-number-separator? :inline t) (character)
  (and (typep character 'complex-number-separator) character))

(deftype delimiter ()
  "Characters, or EOF, that represent a delimiter in Scheme syntax."
  `(member #\Space #\Newline #\( #\) #\; #\" #\Tab :eof))

(define-function (delimiter? :inline t) (character)
  "Tests to see if the character (or :eof) is a delimiter."
  (and (typep character 'delimiter) character))

(define-function (%delimiter? :inline t) (stream)
  "Tests to see if the next character in a stream is a delimiter."
  (delimiter? (peek-char* stream)))

(define-function (%negative? :inline t) (character)
  "Tests to see if the character represents negation."
  (eql #\- character))

(define-function (%sign :inline t) (character)
  "Returns -1 if the character represents negation; otherwise, 1."
  (if (%negative? character) -1 1))

;;;; Numbers

;;; Reads an integer of the given radix
(defun read-scheme-integer (stream &optional (radix 10))
  (check-type radix (integer 2 16))
  (loop :for match := (read-case (stream x)
                        ((:or (:range #\0 #\9)
                              (:range #\a #\f)
                              (:range #\A #\F))
                         (or (digit-char-p x radix)
                             (progn (unread-char x stream) nil)))
                        (:eof nil)
                        (t (unread-char x stream) nil))
        :for length :from 0
        :with number := 0
        :while match
        :do (setf number (+ match (* number radix)))
        :finally (return (values number length))))

;;; Stops when the stream no longer matches the string, returning the
;;; point where it stopped.
(define-function (always :inline t) (string stream)
  (let* ((i 0)
         (char nil)
         (match? (loop :for c* :across string
                       :for c := (read-char stream nil nil)
                       :always (progn
                                 (incf i)
                                 (setf char c)
                                 (and c (char-equal c c*))))))
    (when (and (not match?) char)
      (unread-char char stream))
    (values match? (1- i))))

;;; Reads the final character if an NaN or inf candidate.
(define-function %read-final-char (result stream first?)
  (if result
      (read-case (stream char)
        (#\0 (let ((next-char (peek-char* stream)))
               (cond
                 ((delimiter? next-char)
                  result)
                 ((char-equal #\i next-char)
                  (when first?
                    (skip-read-char stream))
                  (if (or (not first?) (%delimiter? stream))
                      (f:with-float-traps-masked t
                        (values (if first? (complex 0 result) result) t))
                      (invalid-infnan-error)))
                 ((and first? (complex-number-separator? next-char))
                  result)
                 (t
                  (invalid-infnan-error)))))
        (t (invalid-infnan-error)))
      (invalid-infnan-error)))

;;; Reads the exponent of a NaN or infinite flonum.
(defun read-exponent* (stream &optional unread-if-no-match?)
  (read-case (stream exponent-char)
    ((:or #\e #\E #\d #\D) 'double-float)
    ((:or #\f #\F) 'single-float)
    ((:or #\l #\L) 'long-float)
    ((:or #\s #\S) 'short-float)
    (t
     (when (and unread-if-no-match?
                (not (eql exponent-char :eof)))
       (unread-char exponent-char stream))
     nil)))

;;; Reads a NaN candidate, either as a NaN or as an identifier.
;;;
;;; As an extension, the exponentiation suffix is permitted (with 0 as
;;; the only allowed exponent) as a way to get a NaN of a different
;;; floating point type.
(defun %read-nan (sign-prefix stream no-symbol? first?)
  (let ((negate? (%negative? sign-prefix))
        (string "nan.0"))
    (multiple-value-bind (match? index) (always string stream)
      (let ((next-char (peek-char* stream)))
        (cond ((not match?)
               (if (< index (- (length string) 1))
                   (progn
                     (error-when no-symbol?
                                 'scheme-reader-error
                                 :details "Invalid numerical syntax.")
                     (read-scheme-symbol stream
                                         :prefix (format nil
                                                         "~A~A"
                                                         sign-prefix
                                                         (subseq string 0 index))))
                   (invalid-infnan-error)))
              ((delimiter? next-char)
               (nan 'double-float negate?))
              ((char-equal #\i next-char)
               (when first?
                 (skip-read-char stream))
               (if (or (not first?) (%delimiter? stream))
                   (f:with-float-traps-masked t
                     (let ((result (nan 'double-float negate?)))
                       (values (if first? (complex 0 result) result) t)))
                   (invalid-infnan-error)))
              ((and first? (complex-number-separator? next-char))
               (nan 'double-float negate?))
              (t
               (%read-final-char (nan (read-exponent* stream) negate?) stream first?)))))))

;;; Reads an inf candidate, either as a trivial imaginary number, a
;;; floating point infinity, or as an identifier.
;;;
;;; As an extension, the exponentiation suffix is permitted (with 0 as
;;; the only allowed exponent) as a way to get an infinity of a
;;; different floating point type.
(defun %read-inf-or-i (sign-prefix stream no-symbol? first?)
  (let ((negate? (%negative? sign-prefix))
        (string "inf.0"))
    (skip-read-char stream)
    (if (%delimiter? stream)
        (values (complex 0 (if negate? -1 1)) t)
        (multiple-value-bind (match? index) (always (subseq string 1) stream)
          (let ((next-char (peek-char* stream)))
            (cond ((not match?)
                   (if (< index (- (length string) 2))
                       (progn
                         (error-when no-symbol?
                                     'scheme-reader-error
                                     :details "Invalid numerical syntax.")
                         (read-scheme-symbol stream
                                             :prefix (format nil
                                                             "~A~A"
                                                             sign-prefix
                                                             (subseq string 0 (1+ index)))))
                       (invalid-infnan-error)))
                  ((delimiter? next-char)
                   (inf 'double-float negate?))
                  ((char-equal #\i next-char)
                   (when first?
                     (skip-read-char stream))
                   (if (or (not first?) (%delimiter? stream))
                       (f:with-float-traps-masked t
                         (let ((result (inf 'double-float negate?)))
                           (values (if first? (complex 0 result) result) t)))
                       (invalid-infnan-error)))
                  ((and first? (complex-number-separator? next-char))
                   (inf 'double-float negate?))
                  (t
                   (%read-final-char (inf (read-exponent* stream) negate?) stream first?))))))))

;;; Reads a numeric sign if present.
(defun %read-sign (stream)
  (case (peek-char* stream)
    (:eof (eof-error "when a number was expected"))
    ((#\+ #\-) (read-char stream))
    (t nil)))

;;; Checks the radix if the number is to be read as a flonum.
(defun check-flonum-radix (radix)
  (error-unless (= +flonum-base+ radix)
                'scheme-reader-error
                :details (format nil
                                 "A literal flonum must be in base ~D."
                                 +flonum-base+)))

;;; Reads the exponent part of a flonum, after the exponent character
;;; is read.
(defun %read-exponent (number radix stream float-type)
  (check-flonum-radix radix)
  (let ((sign (%sign (%read-sign stream))))
    (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
      (error-when (zerop length*)
                  'scheme-reader-error
                  :details "An exponent was expected but none was provided")
      (* (coerce number float-type)
         (expt +flonum-base+ (* number* sign))))))

;;; Reads the exponent of a flonum.
(defun read-exponent (number radix stream)
  (let ((float-type (read-exponent* stream t)))
    (if float-type
        (%read-exponent number radix stream float-type)
        number)))

;;; Reads a possible suffix for a number.
(defun %read-scheme-number-suffix (number radix stream)
  (read-case (stream match)
    (#\/
     (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
       (error-when (zerop length*)
                   'scheme-reader-error
                   :details "A fraction needs a denominator after the / sign.")
       (/ number number*)))
    (#\.
     (check-flonum-radix radix)
     (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
       (let ((number (+ number (/ number* (expt (double-float* +flonum-base+) length*)))))
         (read-exponent number radix stream))))
    (:eof number)
    (t
     (unread-char match stream)
     (read-exponent number radix stream))))

;;; Reads a number that isn't a NaN or infinity.
(defun %read-regular-scheme-number (radix sign-prefix stream)
  (multiple-value-bind (number length) (read-scheme-integer stream radix)
    ;; A leading decimal point implicitly has a 0 in front.
    ;; Otherwise, no number at the start is an error.
    (let ((next-char (peek-char* stream)))
      (error-when (and (zerop length)
                       (not (eql #\. next-char)))
                  'scheme-reader-error
                  :details "No number could be read when a number was expected.")
      (when (and (zerop length) (eql #\. next-char))
        (setf (values number length) (values 0 1))))
    (let* ((sign (%sign sign-prefix))
           (number (* sign
                      (if (%delimiter? stream)
                          number
                          (%read-scheme-number-suffix number radix stream)))))
      number)))

(defun %read-infnan-or-regular-number (first? next-char radix sign-prefix stream no-symbol?)
  (cond ((and sign-prefix (char-equal next-char #\n))
         ;; Reads NaN or a symbol.
         (%read-nan sign-prefix stream no-symbol? first?))
        ((and sign-prefix (char-equal next-char #\i))
         ;; Reads +i, -i, inf, or a symbol.
         (%read-inf-or-i sign-prefix stream no-symbol? first?))
        ((or (digit-char-p next-char radix)
             (eql next-char #\.))
         ;; Reads a number like 4 or .4
         (%read-regular-scheme-number radix sign-prefix stream))
        ;; For symbols that begin with + or -, such as
        ;; CL-style constant names, e.g. +foo+, excluding +
        ;; or - themselves (the first case in the COND).
        ((and first? sign-prefix)
         (error-when no-symbol?
                     'scheme-reader-error
                     :details "Invalid numerical syntax.")
         (read-scheme-symbol stream :prefix (make-string 1 :initial-element sign-prefix)))
        ;; Everything else is an error here.
        ;;
        ;; Note: This won't error on e.g. "inf" or "nan"
        ;; without the prefix because those should be read
        ;; as a symbol, not as a potential number.
        (t
         (error 'scheme-reader-error
                :details (format nil
                                 "Failure to read a number when reading ~A"
                                 next-char)))))

;;; Reads a Scheme number in the given radix. If end? then it must be
;;; the end of the stream after reading the number.
;;;
;;; Note: Instead of an error, most failed candidates of a number
;;; could be read as a symbol, like in CL and Racket. This is
;;; potentially still valid as a symbol in R7RS-small if it began with
;;; a . instead of a number, such as .1foo
;;;
;;; Note: This extends the syntax by permitting an imaginary number to
;;; exist without a sign prefix in certain cases, e.g. "4i".
(defun %read-scheme-number (stream radix &optional no-symbol?)
  ;; A complex number has two different ways to have a second part:
  ;;
  ;;   {first}+{second}i or {first}-{second}i
  ;;   {first}@{second}
  (flet ((first-part (stream)
           (let ((sign-prefix (%read-sign stream))
                 (next-char (peek-char* stream)))
             (if (delimiter? next-char)
                 (intern (string sign-prefix))
                 (%read-infnan-or-regular-number t next-char radix sign-prefix stream no-symbol?))))
         ;; Note: Ending in a delimiter means there is no second part.
         ;; Ending in an #\i means that the "first" part was really
         ;; the second part.
         ;;
         ;; Note: Some edge case potential symbols are currently
         ;; errors because of the complex syntax, e.g. +inf.0@-inf.,
         ;; which looks like a valid infnan for most of the way.
         (second-part (number stream)
           (multiple-value-bind (number delimiter?)
               (if (%delimiter? stream)
                   (values number t)
                   (values
                    (read-case (stream match)
                      ((:or #\i #\I)
                       (complex 0 number))
                      (#\@
                       (let* ((sign-prefix* (%read-sign stream))
                              (next-char (peek-char* stream))
                              (number* (%read-infnan-or-regular-number nil next-char radix sign-prefix* stream t)))
                         (f:with-float-traps-masked t
                           (* (if (rationalp number) (double-float* number) number)
                              (cis (if (rationalp number) (double-float* number*) number*))))))
                      ((:or #\+ #\-)
                       (let* ((sign-prefix* match)
                              (next-char (peek-char* stream))
                              (number* (if (char-equal next-char #\i)
                                           (* (%sign sign-prefix*) 1)
                                           (%read-infnan-or-regular-number nil next-char radix sign-prefix* stream t))))
                         (f:with-float-traps-masked t
                           (read-case (stream match)
                             ((:or #\i #\I)
                              (complex number number*))
                             (t
                              (error 'scheme-reader-error
                                     :details "Invalid numerical syntax."))))))
                      (t
                       (error 'scheme-reader-error
                              :details "Invalid numerical syntax.")))
                    (%delimiter? stream)))
             (error-unless delimiter?
                           'scheme-reader-error
                           :details "Invalid numerical syntax.")
             number)))
    ;; Note: These aren't necessarily the real and imaginary part.
    ;; Also, imaginary-first-part? doesn't detect all cases of an
    ;; imaginary first part because most of the checks for a trailing
    ;; i happens in the function second-part.
    (multiple-value-bind (first-part imaginary-first-part?) (first-part stream)
      (if (or imaginary-first-part? (symbolp first-part))
          first-part
          (second-part first-part stream)))))

;;; Reads a number for the Scheme reader or string-to-number.
(defun read-scheme-number (stream &optional (radix 10))
  (let* ((next-char (peek-char* stream))
         (possible-number (if (eql next-char #\#)
                              (progn
                                (skip-read-char stream)
                                (%read-special stream :radix radix))
                              (%read-scheme-number stream radix t))))
    (if (numberp possible-number)
        possible-number
        nil)))

;;; Converts a string to a number using Scheme's numeric syntax. This
;;; is used for string->number.
(defun string-to-number (string &optional (radix 10))
  (with-input-from-string (in string)
    (handler-case (read-scheme-number in radix)
      (scheme-reader-error nil))))

;;;; Misc reader syntax

;;; A line comment skips the rest of the stream unless there is a
;;; newline that ends the comment..
(defun read-line-comment (stream)
  (loop :for match := (read-case (stream c)
                        (#\Newline :newline)
                        (:eof :eof)
                        (t nil))
        :until match
        :finally (return :skip)))

;;; A block comment comments everything between #| and |# and allows
;;; these to be nested. The final |# exits the block comment.
(defun read-block-comment (stream)
  (loop :for prior-match := nil :then match
        :for match := (read-case (stream c)
                        (#\| :pipe)
                        (#\# :special)
                        (:eof nil)
                        (t t))
        :until (or (not match)
                   (and (eql prior-match :pipe)
                        (eql match :special)))
        ;; Nested block comments must also match
        :when (and (eql prior-match :special)
                   (eql match :pipe))
          :do (read-block-comment stream)
        :finally (if match
                     (return :skip)
                     (eof-error "inside of a block comment"))))

;;; The standard supports these escape characters in strings and a few
;;; other places. For instance, \n becomes a newline.
(define-function (%one-char-escape :inline t) (char stream)
  (case char
    (#\n (code-char #x000a))
    (#\t (code-char #x0009))
    (#\a (code-char #x0007))
    (#\b (code-char #x0008))
    (#\r (code-char #x000d))
    ;; The main special case is with hex escapes, which are
    ;; "#\x{number};" where {number} is in base 16. Notice the
    ;; semicolon terminator.
    ;;
    ;; Note: This skips the semicolon, which isn't normally skipped
    ;; because delimiters aren't skipped.
    (#\x
     (prog1 (%read-hex-character stream #\;)
       (skip-read-char stream)))
    ;; The other special case is \{whitespace}*{newline} because it
    ;; needs to skip any whitespace between the slash and the newline,
    ;; but is only valid if it's only whitespace.
    ;;
    ;; For now this just uses the simplified Space-or-Tab whitespace.
    ((#\Space #\Tab #\Newline)
     (if (char= char #\Newline)
         nil
         (loop :with whitespace? := nil
               :for c := (read-case (stream c)
                           (#\Newline t)
                           ((:or #\Space #\Tab)
                            (setf whitespace? t)
                            nil)
                           (:eof (eof-error "when a newline was expected"))
                           (t (error 'scheme-reader-error
                                     :details (format nil "A newline expected, but ~A was read." c))))
               :until c
               :finally (return nil))))
    ;; Note: \", \\, and \| are specified. The rest are unspecified,
    ;; but use the CL approach of returning the character itself
    ;; rather than having an error. That's what this path represents.
    (t char)))

;;; Reads a string starting after the initial " that enters the string
;;; reader. A string must end on a non-escaped ".
(defun %read-string (stream)
  (loop :for match := (read-case (stream x)
                        (:eof nil)
                        (t x))
        :for after-escape? := nil :then escape?
        :for escape? := (and (eql match #\\)
                             (not after-escape?))
        :with buffer := (make-adjustable-string)
        :until (or (not match)
                   (and (not after-escape?)
                        (eql match #\")))
        :unless escape?
          :do (let ((result (if after-escape?
                                (%one-char-escape match stream)
                                match)))
                (when result
                  (vector-push-extend result buffer)))
        :finally (return (if match
                             (subseq buffer 0 (fill-pointer buffer))
                             (eof-error "inside of a string")))))

;;; Determine which base the number is in based on a literal syntax.
;;; For example, #x means that it's in hexadecimal.
(defun %find-read-base (stream &optional (radix 10))
  (let ((next-char (peek-char* stream)))
    (case next-char
      (#\#
       (skip-read-char stream)
       (read-case (stream match)
         ((:or #\b #\B) 2)
         ((:or #\o #\O) 8)
         ((:or #\d #\D) 10)
         ((:or #\x #\X) 16)
         (:eof (eof-error "after # when a radix was expected"))
         (t (error 'scheme-reader-error
                   :details (format nil "#~A is not a radix" match)))))
      (:eof (eof-error "when a number was expected"))
      (t radix))))

;;; Reads a number that has a provided base, such as #x for
;;; hexadecimal. If it is followed by #e then it is read as an exact
;;; (non-float) and if it is followed by #i then it is read as an
;;; inexact (float).
(defun %read-in-base (stream base)
  (let* ((next-char (peek-char* stream))
         (exactness (case next-char
                      (#\#
                       (skip-read-char stream)
                       (read-case (stream match)
                         ((:or #\e #\E) #'exact)
                         ((:or #\i #\I) #'inexact)
                         (:eof (eof-error "after # when either E or I was expected"))
                         (t (error 'scheme-reader-error
                                   :details (format nil "#~A is not an exactness/inexactness" match)))))
                      (:eof
                       (eof-error "when a number was expected"))
                      (t
                       nil)))
         (number (%read-scheme-number stream base t)))
    (if exactness
        (funcall exactness number)
        number)))

(defun %read-hex-character (stream &optional delimiter-char)
  (multiple-value-bind (number length)
      (read-scheme-integer stream 16)
    (error-when (or (zerop length)
                    (if delimiter-char
                        (not (eql #\; (peek-char* stream)))
                        (not (%delimiter? stream))))
                'scheme-reader-error
                :details "Invalid hexadecimal number.")
    (code-char number)))

;;; Literal reader syntax for a character. This is either one
;;; character, like #\a, or it is a hex escape, like #\x42, or it is a
;;; named character, like #\newline.
(defun %read-literal-character (stream)
  (read-case (stream c)
    ((:or #\x #\X)
     (if (%delimiter? stream)
         c
         (%read-hex-character stream)))
    (t
     (if (%delimiter? stream)
         c
         (progn
           (unread-char c stream)
           ;; Note: There might be non-interning ways to do this, but
           ;; this way is extensible.
           (case (read-scheme-symbol stream)
             (alarm     (code-char #x0007))
             (backspace (code-char #x0008))
             (delete    (code-char #x007f))
             (escape    (code-char #x001b))
             (newline   (code-char #x000a))
             (null      (code-char #x0000))
             (return    (code-char #x000d))
             (space     (code-char #x0020))
             (tab       (code-char #x0009))
             (t
              (error 'scheme-reader-error
                     :details "Currently, Airship Scheme only supports the required character names."))))))))

(defun %read-directive (stream)
  (let ((directive (read-scheme-symbol stream :*fold-case* t)))
    (cond ((eql 'fold-case directive)
           (setf *fold-case* t)
           :skip)
          ((eql 'no-fold-case directive)
           (setf *fold-case* nil)
           :skip)
          (t (error 'scheme-reader-error
                    :details (format nil
                                     "Unrecognized directive: #!~A"
                                     (invert-case (symbol-name directive))))))))

;;; This is for #-prefixed tokens that are a number or an error.
(defun %read-special (stream &key (radix 10) first? inside-list? quoted? labels)
  (read-case (stream x)
    ((:or #\e #\E)
     (let ((read-base (%find-read-base stream radix)))
       (exact (%read-scheme-number stream read-base t))))
    ((:or #\i #\I)
     (let ((read-base (%find-read-base stream radix)))
       (inexact (%read-scheme-number stream read-base t))))
    ((:or #\b #\B)
     (%read-in-base stream 2))
    ((:or #\o #\O)
     (%read-in-base stream 8))
    ((:or #\d #\D)
     (%read-in-base stream 10))
    ((:or #\x #\X)
     (%read-in-base stream 16))
    (#\!
     (%read-directive stream))
    ((:range #\0 #\9)
     (unread-char x stream)
     (let ((label-id (read-scheme-integer stream 10)))
       (read-case (stream x)
         (#\=
          (unless labels
            (setf labels (make-hash-table)))
          (error-when (hash-table-value-present? label-id labels)
                      'scheme-reader-error
                      :details (format nil "The label ~D appears more than once" label-id))
          (let ((labeled (read-scheme stream
                             :inside-list? inside-list?
                             :quoted? quoted?
                             :first? first?
                             :labels labels)))
            (error-when (eql labeled :dot)
                        'scheme-reader-error
                        :details "Attempted to label a dot")
            (error-when (eql labeled :eof)
                        'scheme-reader-eof-error
                        :details "when expecting something to label")
            (error-when (eql labeled #\))
                        'scheme-reader-error
                        :details "Nothing left in list to label")
            (setf (gethash label-id labels) labeled)))
         (#\#
          (error-unless (%delimiter? stream)
                        'scheme-reader-error
                        :details "Invalid label syntax")
          (error-unless (hash-table-value-present? label-id labels)
                        'scheme-reader-error
                        :details (format nil "Attempted to use label ~D before defining it" label-id))
          (gethash label-id labels))
         (:eof
          (eof-error "in the middle of a label"))
         (t
          (error 'scheme-reader-error
                 :details "Invalid label syntax")))))
    (t
     (error 'scheme-reader-error
            :details (format nil "Reader syntax #~A is not supported!" x)))))

(defun %read-bytevector* (labels stream)
  (handler-case (coerce (read-scheme-list stream :labels labels) 'bytevector?)
    (type-error (c)
      (error 'scheme-type-error
             :details "in reading a bytevector"
             :datum (type-error-datum c)
             :expected-type 'octet))))

;;; Arbitrary whitespace between #u8 and its parentheses is not
;;; required to be supported.
;;;
;;; Producing this non-conforming syntax is the default behavior in
;;; paredit for Emacs.
;;;
;;; i.e. Paredit produces #u8 () when only #u8() conforms to a strict
;;; reading of section 7.1.1 of R7RS-small.
;;;
;;; If you get this error because of paredit, then you can resolve
;;; this by adding the following to your .emacs file:
;;;
;;;   (setq paredit-space-for-delimiter-predicates '((lambda (endp delimiter) nil)))
(defun %read-bytevector (labels stream)
  (read-case (stream character)
    (#\8 (loop :with whitespace? := nil
               :for c := (read-case (stream c)
                           ((:or #\Space #\Tab #\Newline)
                            (whitespace-in-u8-error whitespace?)
                            (setf whitespace? t)
                            nil)
                           (#\( t)
                           (:eof (eof-error "when a \"(\" was expected"))
                           (t (error 'scheme-reader-error
                                     :details (format nil "\"(\" expected, but ~A was read." c))))
               :until c
               :finally (return (%read-bytevector* labels stream))))
    (:eof (eof-error "after #u when an 8 was expected"))
    (t (error 'scheme-reader-error
              :details (format nil "#u8 expected, but #u~A was read." character)))))

;;; Handles the #;-style comments for `read-scheme-character'.
(defun comment-next-form (inside-list? quoted? first? labels stream)
  (let ((skipped-read (read-scheme stream
                                   :inside-list? inside-list?
                                   :quoted? quoted?
                                   :first? first?
                                   :labels labels)))
    (case skipped-read
      (:dot (error 'scheme-reader-error
                   :details "Attempted to comment out a dot."))
      (#\) (error 'scheme-reader-error
                  :details "Expected to skip a token to match a #;-style comment, but none found."))
      (:eof (eof-error "after a #;-style comment"))))
  (read-scheme stream
               :inside-list? inside-list?
               :quoted? quoted?
               :first? first?
               :labels labels))

;;; Reads a token that starts with a # (hashtag).
(defun read-special (inside-list? quoted? first? labels stream)
  (read-case (stream x)
    (#\|
     (read-block-comment stream))
    ((:or #\t #\T)
     (if (or (%delimiter? stream)
             (and (always "rue" stream)
                  (%delimiter? stream)))
         t
         (error 'scheme-reader-error
                :details "Invalid character(s) after #t")))
    ((:or #\f #\F)
     (if (or (%delimiter? stream)
             (and (always "alse" stream)
                  (%delimiter? stream)))
         %scheme-boolean:f
         (error 'scheme-reader-error
                :details "Invalid character(s) after #f")))
    ((:or #\u #\U)
     (%read-bytevector labels stream))
    (#\\
     (%read-literal-character stream))
    (#\(
     (coerce (read-scheme-list stream :labels labels) 'vector?))
    (#\;
     (comment-next-form inside-list? quoted? first? labels stream))
    (:eof
     (eof-error "after a # when a character was expected"))
    (t
     (unread-char x stream)
     (%read-special stream
                    :first? first?
                    :inside-list? inside-list?
                    :quoted? quoted?
                    :labels labels))))

;;; Reads a Scheme symbol that is escaped with the literal ||
;;; notation, like |foo|.
(defun read-escaped-scheme-symbol (stream &key (package *package*) (*fold-case* *fold-case*))
  (loop :for after-escape? := nil :then escape?
        :for char := (read-case (stream c)
                       (#\| (if after-escape? c nil))
                       (#\\ (if after-escape? c :escape))
                       (:eof (eof-error "inside of a |"))
                       (t (let ((result (if after-escape?
                                            (%one-char-escape c stream)
                                            c)))
                            (or result :skip))))
        :for escape? := (eql char :escape)
        :with buffer := (make-adjustable-string)
        :while char
        :unless (not (characterp char))
          :do (vector-push-extend char buffer)
        :finally (return
                   (let ((result (subseq buffer 0 (fill-pointer buffer))))
                     (when *fold-case*
                       (setf result (string-foldcase result)))
                     (map-into result #'%invert-case result)
                     (intern result package)))))

;;; Reads until the delimiter and turns it into a Scheme symbol.
(defun read-scheme-symbol (stream &key (package *package*) prefix (*fold-case* *fold-case*))
  (check-type prefix sequence)
  (loop :for char := (read-case (stream c)
                       (#\(
                        (warn #.(concatenate 'string
                                             "Style warning: There should be a space before "
                                             "a \"(\" if it is directly following a symbol."))
                        (unread-char c stream)
                        nil)
                       ((:or #\" #\Space #\Newline #\) #\; #\Tab)
                        (unread-char c stream)
                        nil)
                       (:eof nil)
                       (t c))
        :with buffer := (if prefix
                            (make-array (length prefix)
                                        :element-type 'character
                                        :adjustable t
                                        :fill-pointer (length prefix)
                                        :initial-contents (map 'string #'%invert-case prefix))
                            (make-adjustable-string))
        :while char
        :do (vector-push-extend char buffer)
        :finally (return (let ((result (subseq buffer 0 (fill-pointer buffer))))
                           (when *fold-case*
                             (setf result (string-foldcase result)))
                           (map-into result #'%invert-case result)
                           (intern result package)))))

;;;; Core syntax

;;; A dot can represent a possible number (if not, it's a symbol), a
;;; part of a dotted list, or the start of a symbol.
(defun read-scheme-dot (match inside-list? quoted? first? stream)
  (error-unless inside-list?
                'scheme-reader-error
                :details "The dotted list syntax must be used inside of a list")
  (let ((next-char (peek-char* stream)))
    (cond ((eql :eof next-char)
           (eof-error "after a dot"))
          ((digit-char-p next-char)
           (unread-char match stream)
           (%read-scheme-number stream +flonum-base+))
          ((delimiter? next-char)
           (error-when quoted?
                       'scheme-reader-error
                       :details "A dot cannot directly follow a quote")
           (error-when first?
                       'scheme-reader-error
                       :details "An expression needs an item before the dot in a dotted list")
           :dot)
          (t
           (unread-char match stream)
           (read-scheme-symbol stream)))))

(defun read-quoted (quote-name inside-list? first? labels stream)
  (let ((quoted (read-scheme stream
                             :inside-list? inside-list?
                             :quoted? t
                             :first? first?
                             :labels labels)))
    (error-when (and inside-list? (eql quoted #\)))
                'scheme-reader-error
                :details "Nothing quoted!")
    (error-when (eql quoted :eof)
                'scheme-reader-eof-error
                :details "after a quote")
    `(,quote-name ,quoted)))

;;; Reads a character and determines what to do with it based on the
;;; Scheme syntax specification.
(defun %read-scheme-character (inside-list? quoted? first? labels stream)
  (read-case (stream match)
    (#\( (read-scheme-list stream :labels labels))
    (#\)
     (error-unless inside-list?
                   'scheme-reader-error
                   :details "Imbalanced parentheses")
     #\))
    (#\" (%read-string stream))
    ;; Note: If +read-base+ is not constant, then the digit range
    ;; would depend on it.
    ((:or (:range #\0 #\9) #\- #\+)
     (unread-char match stream)
     (%read-scheme-number stream +read-base+))
    ((:or #\Newline #\Space #\Tab) :skip)
    (#\# (read-special inside-list? quoted? first? labels stream))
    (#\' (read-quoted 'quote inside-list? first? labels stream))
    (#\` (read-quoted 'quasiquote inside-list? first? labels stream))
    (#\, (read-quoted (if (eql #\@ (peek-char* stream))
                          (progn
                            (skip-read-char stream)
                            'unquote-splicing)
                          'unquote)
                      inside-list?
                      first?
                      labels
                      stream))
    (#\; (read-line-comment stream))
    (#\| (read-escaped-scheme-symbol stream))
    (:eof
     (error-when inside-list?
                 'scheme-reader-error
                 :details "Imbalanced parentheses")
     :eof)
    (#\. (read-scheme-dot match inside-list? quoted? first? stream))
    ((:or :nd :mc :me)
     ;; Note: Many Schemes disregard this rule, but this is mandated
     ;; by section 7.1.1 of r7rs.pdf.
     (unicode-reader-error))
    (t
     (unread-char match stream)
     (read-scheme-symbol stream))))

(defun read-scheme-list (stream &key labels)
  "Reads a list, with special handling of the dotted list syntax."
  (loop :with s-expression := (list)
        :with last := (list)
        :for match := (read-scheme stream
                                   :inside-list? t
                                   :first? (endp s-expression)
                                   :labels labels)
        :for after-dot? := nil :then (or dot? after-dot?)
        :for dot? := (eql match :dot)
        :with already-dotted? := nil
        :until (eql match #\))
        :do
           (progn
             (error-when (and after-dot? dot?)
                         'scheme-reader-error
                         :details "More than one dot inside of a dotted list")
             (unless dot?
               (error-when already-dotted?
                           'scheme-reader-error
                           :details "More than one item after a dot in a dotted list")
               (let ((new-cons (if after-dot?
                                   (progn
                                     (setf already-dotted? t)
                                     match)
                                   (list match))))
                 (setf last
                       (if (endp s-expression)
                           (setf s-expression new-cons)
                           (progn
                             (nconc last new-cons)
                             new-cons))))))
        :finally
           (progn
             (error-when (and after-dot? (not already-dotted?))
                         'scheme-reader-error
                         :details "An expression needs an item after the dot in a dotted list")
             (return s-expression))))

(defun read-scheme (stream &key inside-list? quoted? first? (labels (make-hash-table)))
  "An implementation of Scheme's read procedure."
  (check-type labels (maybe hash-table))
  (loop :for match := (%read-scheme-character inside-list? quoted? first? labels stream)
        :while (eql match :skip)
        :finally (return match)))

(defun read-scheme-file (stream-or-path &key (labels (make-hash-table)))
  "
Reads an entire file from a stream or pathname into a list using the
Scheme reader.
"
  (check-type labels (maybe hash-table))
  (flet ((read-scheme-file* (stream)
           (loop :for match := (read-scheme stream :labels labels)
                 :until (eql match :eof)
                 :collect match)))
    (if (typep stream-or-path '(or string pathname))
        (with-open-file (stream stream-or-path)
          (read-scheme-file* stream))
        (read-scheme-file* stream-or-path))))

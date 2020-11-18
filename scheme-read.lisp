;;;; -*- mode: common-lisp; -*-

;;; TODO: directives (read in `read-special'; replaces all
;;; %invert-case with fold-case, which has to be done as a string, by
;;; Unicode's rules!)
;;;
;;; TODO: in `read-special', handle labels (for literal circular/etc.
;;; data structures)
;;;
;;; TODO: any missing escapes in `%read-string' and elsewhere, where
;;; also relevant

(cl:in-package #:airship-scheme)

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

;;; A simple macro for a simple EOF error.
(defmacro eof-error (details)
  `(error 'scheme-reader-eof-error :details ,details))

;;;; Common characters

(deftype delimiter ()
  "Characters, or EOF, that represent a delimiter in Scheme syntax."
  `(member #\Space #\Newline #\( #\) #\; #\" #\Tab :eof))

(define-function (delimiter? :inline t) (character)
  "Tests to see if the character (or :eof) is a delimiter."
  (and (typep character 'delimiter) character))

(define-function (%delimiter? :inline t) (stream)
  "Tests to see if the next character in a stream is a delimiter."
  (delimiter? (peek-char nil stream nil :eof)))

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
(define-function %read-final-char ((starting-string (simple-string 5))
                                   result
                                   (exponent-char character)
                                   sign-prefix
                                   stream)
  (let ((string-length 5))
    (flet ((read-as-symbol (char)
             (let* ((string-length* (+ 3 string-length))
                    (string (make-string string-length*)))
               (replace string starting-string :start1 1)
               (setf (aref string 0) sign-prefix
                     (aref string (- string-length* 2)) exponent-char
                     (aref string (- string-length* 1)) char)
               (read-scheme-symbol stream :prefix string)))
           ;; If there's nothing afterwards, then this shortcut can be
           ;; taken in creating the symbol.
           (read-as-symbol* (sign-prefix starting-string exponent-char)
             (intern (map 'string
                          #'%invert-case
                          (format nil "~A~A~A" sign-prefix starting-string exponent-char)))))
      (if result
          (read-case (stream char)
            (#\0 (if (%delimiter? stream)
                     result
                     (read-as-symbol char)))
            (:eof (read-as-symbol* sign-prefix starting-string exponent-char))
            (t (read-as-symbol char)))
          (let* ((string-length* (+ 2 string-length))
                 (string (make-string string-length*)))
            (replace string starting-string :start1 1)
            (setf (aref string 0) sign-prefix
                  (aref string (- string-length* 1)) exponent-char)
            (read-scheme-symbol stream :prefix string))))))

;;; Reads the exponent of a NaN or infinite flonum.
(defun read-exponent* (stream &optional unread-if-no-match?)
  (read-case (stream exponent-char)
    ((:or #\e #\E #\d #\D)
     (values 'double-float exponent-char))
    ((:or #\f #\F)
     (values 'single-float exponent-char))
    ((:or #\l #\L)
     (values 'long-float exponent-char))
    ((:or #\s #\S)
     (values 'short-float exponent-char))
    (t
     (when (and unread-if-no-match?
                (not (eql exponent-char :eof)))
       (unread-char exponent-char stream))
     (values nil exponent-char))))

;;; Reads a NaN candidate, either as a NaN or as an identifier.
;;;
;;; As an extension, the exponentiation suffix is permitted (with 0 as
;;; the only allowed exponent) as a way to get a NaN of a different
;;; floating point type.
(defun %read-nan (sign-prefix stream)
  (let ((negate? (%negative? sign-prefix))
        (string "nan.0"))
    (multiple-value-bind (match? index) (always string stream)
      (cond ((not match?)
             (read-scheme-symbol stream
                                 :prefix (format nil
                                                 "~A~A"
                                                 sign-prefix
                                                 (subseq string 0 index))))
            ((%delimiter? stream)
             (nan 'double-float negate?))
            (t
             (multiple-value-bind (result exponent-char)
                 (read-exponent* stream)
               (%read-final-char string (nan result negate?) exponent-char sign-prefix stream)))))))

;;; Reads an inf candidate, either as a trivial imaginary number, a
;;; floating point infinity, or as an identifier.
;;;
;;; As an extension, the exponentiation suffix is permitted (with 0 as
;;; the only allowed exponent) as a way to get an infinity of a
;;; different floating point type.
(defun %read-inf-or-i (sign-prefix stream)
  (let ((negate? (%negative? sign-prefix))
        (string "inf.0"))
    (skip-read-char stream)
    (if (%delimiter? stream)
        (complex 0 (if negate? -1 1))
        (multiple-value-bind (match? index) (always (subseq string 1) stream)
          (cond ((not match?)
                 (read-scheme-symbol stream
                                     :prefix (format nil
                                                     "~A~A"
                                                     sign-prefix
                                                     (subseq string 0 (1+ index)))))
                ((%delimiter? stream)
                 ;; Yes, most of INF's code is unreachable.
                 (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
                   (inf 'double-float negate?)))
                (t
                 (multiple-value-bind (result exponent-char)
                     (read-exponent* stream)
                   (%read-final-char string (inf result negate?) exponent-char sign-prefix stream))))))))

;;; Reads a numeric sign if present.
(defun %read-sign (stream)
  (case (peek-char nil stream nil :eof)
    (:eof (eof-error "when a number was expected"))
    ((#\+ #\-) (read-char stream))
    (t nil)))

;;; Checks the radix if the number is to be read as a flonum.
(defun check-flonum-radix (radix)
  (error-unless (= 10 radix)
                'scheme-reader-error
                :details "A literal flonum must be in base 10."))

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
         (expt 10 (* number* sign))))))

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
       (let ((number (+ number (/ number* (expt 10d0 length*)))))
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
    (let ((next-char (peek-char nil stream nil :eof)))
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

;;; Reads a Scheme number in the given radix. If end? then it must be
;;; the end of the stream after reading the number.
;;;
;;; TODO: Allow inf/nan/etc. to be used with the imaginary numbers,
;;; including potentially ending in i in either the first-part (with
;;; no second part) or the second-part (unless @ produces the second
;;; part).
;;;
;;; TODO: Take into account when #e or #x or so on are used when
;;; creating a symbol instead of a number. If #e or #x etc. are used,
;;; then a symbol can't be created.
;;;
;;; Note: Instead of an error, this failed candidate of a number could
;;; be read as a symbol, like in CL and Racket. This is potentially
;;; still valid as a symbol in R7RS-small if it began with a . instead
;;; of a number, such as .1foo
(defun %read-scheme-number (stream radix &optional end?)
  ;; A complex number has two different ways to have a second part:
  ;;
  ;;   {first}+{second}i or {first}-{second}i
  ;;   {first}@{second}
  (flet ((first-part (sign-prefix stream)
           (let ((next-char (peek-char nil stream nil :eof)))
             (cond ((delimiter? next-char)
                    sign-prefix)
                   ;; Either nan or a symbol
                   ((and sign-prefix (char-equal next-char #\n))
                    (%read-nan sign-prefix stream))
                   ;; Either +i, -i, an inf, or a symbol
                   ((and sign-prefix (char-equal next-char #\i))
                    (%read-inf-or-i sign-prefix stream))
                   ;; A regular number starts with a digit or .
                   ((or (digit-char-p next-char radix)
                        (eql next-char #\.))
                    (%read-regular-scheme-number radix sign-prefix stream))
                   ;; For symbols that begin with + or -, such as
                   ;; CL-style constant names, e.g. +foo+, excluding +
                   ;; or - themselves (the first case in the COND).
                   (sign-prefix
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
                                            next-char))))))
         ;; Note: Ending in a delimiter means there is no second part.
         ;; Ending in an #\i means that the "first" part was really
         ;; the second part.
         (second-part (number sign-prefix delimiter? stream)
           (multiple-value-bind (number delimiter?)
               (if delimiter?
                   (values number t)
                   (values
                    (read-case (stream match)
                      ((:or #\i #\I)
                       ;; Note: Removing this check would be a
                       ;; reasonable syntax extension over the
                       ;; standard.
                       (error-unless (and sign-prefix (%delimiter? stream))
                                     'scheme-reader-error
                                     :details "Invalid numerical syntax.")
                       (complex 0 number))
                      (#\@
                       (let* ((sign-prefix* (%read-sign stream))
                              (number* (%read-regular-scheme-number radix sign-prefix* stream)))
                         (* (if (rationalp number) (double-float* number) number)
                            (cis (if (rationalp number) (double-float* number*) number*)))))
                      ((:or #\+ #\-)
                       (let* ((sign-prefix* match)
                              (number* (%read-regular-scheme-number radix sign-prefix* stream)))
                         (read-case (stream match)
                           ((:or #\i #\I)
                            (complex number number*))
                           (t (error 'scheme-reader-error
                                     :details "Invalid numerical syntax.")))))
                      (t
                       (error 'scheme-reader-error
                              :details "Invalid numerical syntax.")))
                    (%delimiter? stream)))
             ;; In CL terminology, this stream contains "junk" after
             ;; the number.
             (error-when (and end? (not (eql delimiter? :eof)))
                         'scheme-reader-error
                         :details "Expected an EOF after reading the number.")
             number)))
    ;; Remember the original sign-prefix. Process the first part and
    ;; then if necessary, the second part.
    ;;
    ;; Note: These aren't necessarily the real and imaginary part.
    (let ((sign-prefix (%read-sign stream)))
      (second-part (first-part sign-prefix stream)
                   sign-prefix
                   (%delimiter? stream) stream))))

;;; Reads a number for the Scheme reader or string-to-number.
(defun read-scheme-number (stream &optional (radix 10) end?)
  (let* ((next-char (peek-char nil stream nil :eof))
         (possible-number (if (eql next-char #\#)
                              (progn
                                (skip-read-char stream)
                                (%read-special stream radix))
                              (%read-scheme-number stream radix end?))))
    (if (numberp possible-number)
        possible-number
        nil)))

;;; Converts a string to a number using Scheme's numeric syntax. This
;;; is used for string->number.
(defun string-to-number (string &optional (radix 10))
  (with-input-from-string (in string)
    (handler-case (read-scheme-number in radix t)
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
(define-function (%one-char-escape :inline t) (char)
  (case char
    (#\n (code-char #x000a))
    (#\t (code-char #x0009))
    (#\a (code-char #x0007))
    (#\b (code-char #x0008))
    (#\r (code-char #x000d))
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
          :do (vector-push-extend (if after-escape?
                                      (%one-char-escape match)
                                      match)
                                  buffer)
        :finally (return (if match
                             (subseq buffer 0 (fill-pointer buffer))
                             (eof-error "inside of a string")))))

;;; Determine which base the number is in based on a literal syntax.
;;; For example, #x means that it's in hexadecimal.
(defun %find-read-base (stream &optional (radix 10))
  (let ((next-char (peek-char nil stream nil :eof)))
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
  (let* ((next-char (peek-char nil stream nil :eof))
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
         (number (%read-scheme-number stream base)))
    (if exactness
        (funcall exactness number)
        number)))

;;; Literal reader syntax for a character. This is either one
;;; character, like #\a, or it is a hex escape, like #\x42, or it is a
;;; named character, like #\newline.
(defun %read-literal-character (stream)
  (read-case (stream c)
    ((:or #\x #\X)
     (if (%delimiter? stream)
         c
         (multiple-value-bind (number length)
             (read-scheme-integer stream 16)
           (error-when (or (zerop length)
                           (not (%delimiter? stream)))
                       'scheme-reader-error
                       :details "Invalid hexadecimal number.")
           (code-char number))))
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

;;; This is for #-prefixed tokens that are a number or an error.
(defun %read-special (stream &optional (radix 10))
  (read-case (stream x)
    ((:or #\e #\E)
     (let ((read-base (%find-read-base stream radix)))
       (exact (%read-scheme-number stream read-base))))
    ((:or #\i #\I)
     (let ((read-base (%find-read-base stream radix)))
       (inexact (%read-scheme-number stream read-base))))
    ((:or #\b #\B)
     (%read-in-base stream 2))
    ((:or #\o #\O)
     (%read-in-base stream 8))
    ((:or #\d #\D)
     (%read-in-base stream 10))
    ((:or #\x #\X)
     (%read-in-base stream 16))
    (t
     (error 'scheme-reader-error
            :details (format nil "Reader syntax #~A is not supported!" x)))))

(defun %read-bytevector* (stream)
  (handler-case (coerce (scheme-read stream :recursive? t) 'bytevector?)
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
(defun %read-bytevector (stream)
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
               :finally (return (%read-bytevector* stream))))
    (:eof (eof-error "after #u when an 8 was expected"))
    (t (error 'scheme-reader-error
              :details (format nil "#u8 expected, but #u~A was read." character)))))

;;; Reads a token that starts with a # (hashtag).
(defun read-special (stream)
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
     (%read-bytevector stream))
    (#\\
     (%read-literal-character stream))
    (#\(
     (coerce (scheme-read stream :recursive? t) 'vector?))
    (#\;
     :skip-next)
    (:eof
     (eof-error "after a # when a character was expected"))
    (t
     (unread-char x stream)
     (%read-special stream))))

;;; Reads a Scheme symbol that is escaped with the literal ||
;;; notation, like |foo|.
(defun read-escaped-scheme-symbol (stream &optional (package *package*))
  (loop :for after-escape? := nil :then escape?
        :for char := (read-case (stream c)
                       (#\| (if after-escape? c nil))
                       (#\\ (if after-escape? c :escape))
                       (:eof (eof-error "inside of a |"))
                       (t (%invert-case (if after-escape?
                                            (%one-char-escape c)
                                            c))))
        :for escape? := (eql char :escape)
        :with buffer := (make-adjustable-string)
        :while char
        :unless escape?
          :do (vector-push-extend char buffer)
        :finally (return (intern (subseq buffer 0 (fill-pointer buffer))
                                 package))))

;;; Reads until the delimiter and turns it into a Scheme symbol.
(defun read-scheme-symbol (stream &key (package *package*) prefix)
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
                       (t (%invert-case c)))
        :with buffer := (if (and prefix (typep prefix 'sequence))
                            (make-array (length prefix)
                                        :element-type 'character
                                        :adjustable t
                                        :fill-pointer (length prefix)
                                        :initial-contents (map 'string #'%invert-case prefix))
                            (make-adjustable-string))
        :while char
        :do (vector-push-extend char buffer)
        :finally (return (intern (subseq buffer 0 (fill-pointer buffer))
                                 package))))

;;;; Core syntax

;;; A dot can represent a possible number (if not, it's a symbol), a
;;; part of a dotted list, or the start of a symbol.
(defun read-scheme-dot (match stream)
  (let ((next-char (peek-char nil stream nil nil)))
    (cond ((not next-char)
           (eof-error "after a dot"))
          ((digit-char-p next-char)
           (unread-char match stream)
           (%read-scheme-number stream 10))
          ((delimiter? next-char)
           :dot)
          (t
           (unread-char match stream)
           (read-scheme-symbol stream)))))

;;; Reads a character and determines what to do with it based on the
;;; Scheme syntax specification.
(defun read-scheme-character* (stream)
  (read-case (stream match)
    (#\( (scheme-read stream :recursive? t))
    (#\) #\))
    (#\" (%read-string stream))
    ((:or (:range #\0 #\9) #\- #\+)
     (unread-char match stream)
     (%read-scheme-number stream 10))
    ((:or #\Newline #\Space #\Tab) :skip)
    (#\# (read-special stream))
    (#\' :quote)
    (#\` :quasiquote)
    (#\, (if (eql #\@ (peek-char nil stream nil :eof))
             (progn
               (skip-read-char stream)
               :unquote-splicing)
             :unquote))
    (#\; (read-line-comment stream))
    (#\| (read-escaped-scheme-symbol stream))
    (:eof :eof)
    (#\. (read-scheme-dot match stream))
    ((:or :nd :mc :me)
     ;; Note: Many Schemes disregard this rule, but this is mandated
     ;; by section 7.1.1 of r7rs.pdf.
     (unicode-reader-error))
    (t
     (unread-char match stream)
     (read-scheme-symbol stream))))

;;; Handles the #;-style comments for `read-scheme-character'.
(defun comment-next-form (stream)
  (let ((skipped-read (read-scheme-character stream)))
    (case skipped-read
      (:dot (error 'scheme-reader-error
                   :details "Attempted to comment out a dot."))
      (#\) (error 'scheme-reader-error
                  :details "Expected to skip a token to match a #;-style comment, but none found."))
      (:eof (eof-error "after a #;-style comment"))))
  (read-scheme-character stream))

;;; Wraps around `read-scheme-character*' to handle a few special
;;; cases, like #;-style comments.
(defun read-scheme-character (stream)
  (loop :for match := (let ((match* (read-scheme-character* stream)))
                        (if (eql match* :skip-next)
                            (comment-next-form stream)
                            match*))
        :while (eql match :skip)
        :finally (return match)))

(defun scheme-read (stream &key recursive? quoted? limit)
  (check-type limit (maybe a:non-negative-fixnum))
  (flet ((dotted? (match s-expression)
           (and (eql match :dot)
                (cond (quoted?
                       (error 'scheme-reader-error
                              :details "A dot cannot follow a quote"))
                      ((not recursive?)
                       (error 'scheme-reader-error
                              :details "The dotted list syntax must be used inside of a list"))
                      ((not s-expression)
                       (error 'scheme-reader-error
                              :details "An expression needs an item before the dot in a dotted list"))
                      (t t))))
         (end-of-read? (match)
           (or (and recursive?
                    (eql match #\)))
               (eql match :eof)))
         (check-dot (dotted-end? match)
           (cond (dotted-end?
                  (error 'scheme-reader-error
                         :details "More than one item after a dot in a dotted list"))
                 ((eql match :dot)
                  (error 'scheme-reader-error
                         :details "More than one dot inside of a dotted list"))
                 (t nil)))
         (check-end (old match after-dotted? dotted-end?)
           (cond ((or (and recursive? (eql match :eof))
                      (and (not recursive?) (eql old #\))))
                  (error 'scheme-reader-error
                         :details "Imbalanced parentheses"))
                 ((and after-dotted? (not dotted-end?))
                  (error 'scheme-reader-error
                         :details "An expression needs an item after the dot in a dotted list"))
                 (t nil)))
         (possibly-quote (match)
           (if (member match '(:quote :quasiquote :unquote :unquote-splicing))
               (let ((quoted (scheme-read stream :limit 1 :quoted? t :recursive? t))
                     (match* (ecase match
                               (:quote 'quote)
                               (:quasiquote 'quasiquote)
                               (:unquote 'unquote)
                               (:unquote-splicing 'unquote-splicing))))
                 (when (endp quoted)
                   (if recursive?
                       (error 'scheme-reader-error
                              :details "Nothing quoted!")
                       (eof-error "after a quote")))
                 `(,match* ,@quoted))
               match)))
    (loop :with limit* :of-type (maybe a:non-negative-fixnum) := limit
          :for old := nil :then (if (and match
                                         (not (eql match :dot)))
                                    match
                                    old)
          :for match := (if (and limit (zerop limit*))
                            :skip
                            (read-scheme-character stream))
          :for after-dotted? := nil :then (or dotted? after-dotted?)
          :for dotted? := (dotted? match s-expression)
          :with dotted-end := nil
          :until (or (and limit* (zerop limit*))
                     (end-of-read? match))
          :if (and (not (eql match :skip))
                   (not dotted?)
                   (not after-dotted?))
            :do (when limit* (decf limit*))
            :and
              :collect (possibly-quote match) :into s-expression
          :else
            :if (and (not (eql match :skip)) after-dotted?)
              :do (progn
                    (check-dot dotted-end match)
                    (setf dotted-end (list match)))
          :finally
             ;; Note: This isn't an efficient way to make a dotted
             ;; list, but is the efficient way worth the added cost
             ;; when building proper lists?
             (return
               (progn
                 (check-end old match after-dotted? dotted-end)
                 (if dotted-end
                     (progn
                       (setf (cdr (last s-expression)) (car dotted-end))
                       s-expression)
                     s-expression))))))

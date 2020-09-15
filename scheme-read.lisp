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

(defmacro eof-error (details)
  `(error 'scheme-reader-eof-error :details ,details))

(define-function (%delimiter? :inline t) (stream)
  (member (peek-char nil stream nil :eof)
          '(#\Space #\Newline #\( #\) #\; #\" #\Tab :eof)))

;;; If possible, this generates a NaN of the given type of float for
;;; +nan.0 and -nan.0
(defun nan (float-type)
  (let ((zero (coerce 0 float-type)))
    (f:with-float-traps-masked t (/ zero zero))))

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

;;; Reads a Scheme number in the given radix. If end? then it must be
;;; the end of the stream after reading the number.
;;;
;;; TODO: complex via ({NUMBER} +/- {NUMBER} i)
;;;
;;; TODO: complex via {real} @ {real}
;;;
;;; TODO: complex can support any number on either side (including
;;; infnan) except the #foo portion of the syntax, which is the
;;; prefix and must come first, before the complex.
(defun %read-scheme-number (stream radix &optional end?)
  (labels ((read-sign (stream)
             (case (peek-char nil stream nil :eof)
               (:eof (eof-error "when a number was expected"))
               ((#\+ #\-) (read-char stream))
               (t nil)))
           (flonum-radix-error ()
             (error-unless (= 10 radix)
                           'scheme-reader-error
                           :details "A literal flonum must be in base 10."))
           (read-exponent (number stream float-type)
             (flonum-radix-error)
             (let ((negate? (eql #\- (read-sign stream))))
               (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
                 (error-when (zerop length*)
                             'scheme-reader-error
                             :details "An exponent was expected but none was provided")
                 (* (coerce number float-type)
                    (expt 10
                          (* number*
                             (if negate? -1 1))))))))
    (let* ((sign-prefix (read-sign stream))
           (negate? (eql #\- sign-prefix))
           (next-char (peek-char nil stream nil :eof)))
      ;; The special cases, which can only happen if there's a sign
      ;; prefix, are inf.0, nan.0, or i. As an extension, the extended
      ;; exponentiation suffix is permitted (with 0 as the only
      ;; allowed exponent) as a way to get an infinity or NaN of a
      ;; different floating point type.
      ;;
      ;; There are also cases that are not numbers, but symbols. Most
      ;; trivially, these are + and -.
      ;;
      ;; TODO: fixme: failed candidates for inf and nan should be
      ;; symbols because they begin with + or - like many ordinary
      ;; symbols.
      (cond ((%delimiter? stream)
             sign-prefix)
            ((or (eql next-char #\n)
                 (eql next-char #\N))
             (if (loop :for c* :across "nan.0"
                       :for c := (read-char stream nil nil)
                       :always (and c (char-equal c c*)))
                 (if (%delimiter? stream)
                     (nan 'double-float)
                     (flet ((read-zero-character? (exponent-char stream)
                              (read-case (stream char)
                                (#\0 (if (%delimiter? stream)
                                         t
                                         (error 'scheme-reader-error
                                                :details "Invalid way to write a NaN literal.")))
                                (t
                                 (error 'scheme-reader-error
                                        :details (format nil
                                                         "Invalid character; ~Anan.0~A0 expected"
                                                         sign-prefix
                                                         exponent-char))))))
                       (read-case (stream exponent-char)
                         ((:or #\e #\E #\d #\D)
                          (when (read-zero-character? exponent-char stream)
                            (nan 'double-float)))
                         ((:or #\f #\F)
                          (when (read-zero-character? exponent-char stream)
                            (nan 'single-float)))
                         ((:or #\l #\L)
                          (when (read-zero-character? exponent-char stream)
                            (nan 'long-float)))
                         ((:or #\s #\S)
                          (when (read-zero-character? exponent-char stream)
                            (nan 'short-float)))
                         (t
                          (error 'scheme-reader-error
                                 :details "Invalid way to write a NaN literal.")))))
                 (error 'scheme-reader-error
                        :details (format nil
                                         "The reader expected ~Anan.0"
                                         sign-prefix))))
            ((or (eql next-char #\i)
                 (eql next-char #\I))
             (read-char stream nil :eof)
             (cond ((%delimiter? stream)
                    (* #C(0 1)
                       (if negate? -1 1)))
                   ((loop :for c* :across "nf.0"
                          :for c := (read-char stream nil nil)
                          :always (and c (char-equal c c*)))
                    (if (%delimiter? stream)
                        (if negate?
                            f:double-float-negative-infinity
                            f:double-float-positive-infinity)
                        (flet ((read-zero-character? (exponent-char stream)
                                 (read-case (stream char)
                                   (#\0 (if (%delimiter? stream)
                                            t
                                            (error 'scheme-reader-error
                                                   :details "Invalid way to write an infinity literal.")))
                                   (t
                                    (error 'scheme-reader-error
                                           :details (format nil
                                                            "Invalid character; ~Ainf.0~A0 expected"
                                                            sign-prefix
                                                            exponent-char))))))
                          (read-case (stream exponent-char)
                            ((:or #\e #\E #\d #\D)
                             (when (read-zero-character? exponent-char stream)
                               (if negate?
                                   f:double-float-negative-infinity
                                   f:double-float-positive-infinity)))
                            ((:or #\f #\F)
                             (when (read-zero-character? exponent-char stream)
                               (if negate?
                                   f:single-float-negative-infinity
                                   f:single-float-positive-infinity)))
                            ((:or #\l #\L)
                             (when (read-zero-character? exponent-char stream)
                               (if negate?
                                   f:long-float-negative-infinity
                                   f:long-float-positive-infinity)))
                            ((:or #\s #\S)
                             (when (read-zero-character? exponent-char stream)
                               (if negate?
                                   f:short-float-negative-infinity
                                   f:short-float-positive-infinity)))
                            (t
                             (error 'scheme-reader-error
                                    :details "Invalid way to write an infinity literal."))))))
                   (t
                    (error 'scheme-reader-error
                           :details (format nil
                                            "The reader expected ~Ai or ~Ainf.0"
                                            sign-prefix
                                            sign-prefix)))))
            ((and next-char
                  (or (digit-char-p next-char radix)
                      (eql next-char #\.)))
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
               (let ((number (cond ((%delimiter? stream)
                                    number)
                                   (t
                                    (read-case (stream match)
                                      (#\/
                                       (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
                                         (error-when (zerop length*)
                                                     'scheme-reader-error
                                                     :details "A fraction needs a denominator after the / sign.")
                                         (/ number number*)))
                                      (#\.
                                       (flonum-radix-error)
                                       (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
                                         (+ number (/ number* (expt 10d0 length*)))))
                                      ((:or #\e #\E #\d #\D)
                                       (read-exponent number stream 'double-float))
                                      ((:or #\f #\F)
                                       (read-exponent number stream 'single-float))
                                      ((:or #\l #\L)
                                       (read-exponent number stream 'long-float))
                                      ((:or #\s #\S)
                                       (read-exponent number stream 'short-float))
                                      (t
                                       (unread-char match stream)
                                       0)))))
                     (delimiter? (%delimiter? stream)))
                 ;; Note: Instead of an error, this failed candidate
                 ;; of a number could be read as a symbol, like in CL
                 ;; and Racket. This is potentially still valid as a
                 ;; symbol in R7RS-small if it began with a . instead
                 ;; of a number, such as .1foo
                 (error-unless delimiter?
                               'scheme-reader-error
                               :details "Invalid numerical syntax.")
                 ;; In CL terminology, this stream contains "junk" after the
                 ;; number.
                 (error-when (and end? (not (eql (car delimiter?) :eof)))
                             'scheme-reader-error
                             :details "Expected an EOF after reading the number.")
                 (* number (if negate? -1 1)))))
            ;; For symbols that begin with + or -, such as CL-style
            ;; constant names, e.g. +foo+
            (sign-prefix
             (read-scheme-symbol stream :prefix (make-string 1 :initial-element sign-prefix)))
            (t
             (error 'scheme-reader-error
                    :details (format nil
                                     "Failure to read a number when reading ~A"
                                     next-char)))))))

(defun read-scheme-number (stream &optional (radix 10) end?)
  (let* ((next-char (peek-char nil stream nil :eof))
         (possible-number (if (eql next-char #\#)
                              (progn
                                (read-char stream nil :eof)
                                (%read-special stream radix))
                              (%read-scheme-number stream radix end?))))
    (if (numberp possible-number)
        possible-number
        nil)))

(defun string-to-number (string &optional (radix 10))
  (with-input-from-string (in string)
    (handler-case (read-scheme-number in radix t)
      (scheme-reader-error nil))))

(defun read-line-comment (stream)
  (loop :for match := (read-case (stream c)
                        (#\Newline :newline)
                        (:eof :eof)
                        (t nil))
        :until match
        :finally (return :skip)))

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
        :with buffer := (make-array 16
                                    :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
        :until (or (not match)
                   (and (not after-escape?)
                        (eql match #\")))
        :unless escape?
          :do (if after-escape?
                  (vector-push-extend (%one-char-escape match) buffer)
                  (vector-push-extend match buffer))
        :finally (return (if match
                             (subseq buffer 0 (fill-pointer buffer))
                             (eof-error "inside of a string")))))

(defun %find-read-base (stream &optional (radix 10))
  (let ((next-char (peek-char nil stream nil :eof)))
    (case next-char
      (#\#
       (read-char stream nil nil t)
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

(defun %read-in-base (stream base)
  (let* ((next-char (peek-char nil stream nil :eof))
         (exactness (case next-char
                      (#\#
                       (read-char stream nil nil t)
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
           number)))
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

(defun read-special (stream)
  (read-case (stream x)
    (#\|
     (read-block-comment stream))
    ((:or #\t #\T)
     (if (or (%delimiter? stream)
             (and (loop :for c* :across "rue"
                        :for c := (read-char stream nil nil)
                        :always (and c (char-equal c c*)))
                  (%delimiter? stream)))
         t
         (error 'scheme-reader-error
                :details "Invalid character(s) after #t")))
    ((:or #\f #\F)
     (if (or (%delimiter? stream)
             (and (loop :for c* :across "alse"
                        :for c := (read-char stream nil nil)
                        :always (and c (char-equal c c*)))
                  (%delimiter? stream)))
         %scheme-boolean:f
         (error 'scheme-reader-error
                :details "Invalid character(s) after #f")))
    ;; Arbitrary whitespace between #u8 and its parentheses is not
    ;; required to be supported.
    ;;
    ;; Producing this non-conforming syntax is the default behavior in
    ;; paredit for Emacs.
    ;;
    ;; i.e. Paredit produces #u8 () when only #u8() conforms to a
    ;; strict reading of section 7.1.1 of R7RS-small.
    ;;
    ;; If you get this error because of paredit, then you can resolve
    ;; this by adding the following to your .emacs file:
    ;;
    ;;   (setq paredit-space-for-delimiter-predicates '((lambda (endp delimiter) nil)))
    ;;
    ;; The error might be overridable in the future, so the
    ;; whitespace-tracking is maintained even though the whitespace is
    ;; currently an error.
    ((:or #\u #\U)
     (read-case (stream c)
       (#\8 (loop :with whitespace? := nil
                  :for c := (read-case (stream c)
                              ((:or #\Space #\Tab #\Newline)
                               (unless whitespace?
                                 (error 'scheme-reader-error
                                        :details #.(concatenate
                                                    'string
                                                    "In a strict interpretation of the R7RS-small "
                                                    "standard in section 7.1.1, whitespace between #u8 "
                                                    "and its parentheses is not permitted.")))
                               (setf whitespace? t)
                               nil)
                              (#\( t)
                              (:eof (eof-error "when a \"(\" was expected"))
                              (t (error 'scheme-reader-error
                                        :details (format nil "\"(\" expected, but ~A was read." c))))
                  :until c
                  :finally
                     (return
                       (handler-case (coerce (scheme-read stream :recursive? t) 'bytevector?)
                         (type-error (c)
                           (error 'scheme-type-error
                                  :details "in reading a bytevector"
                                  :datum (type-error-datum c)
                                  :expected-type 'octet))))))
       (:eof (eof-error "after #u when an 8 was expected"))
       (t (error 'scheme-reader-error
                 :details (format nil "#u8 expected, but #u~A was read." c)))))
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

(defun read-escaped-scheme-symbol (stream &optional (package *package*))
  (loop :for after-escape? := nil :then escape?
        :for char := (read-case (stream c)
                       (#\| (if after-escape? c nil))
                       (#\\ (if after-escape? c :escape))
                       (:eof (eof-error "inside of a |"))
                       (t (if after-escape?
                              (%invert-case (%one-char-escape c))
                              (%invert-case c))))
        :for escape? := (eql char :escape)
        :with buffer := (make-array 16
                                    :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
        :while char
        :unless escape?
          :do (vector-push-extend char buffer)
        :finally (return (intern (subseq buffer 0 (fill-pointer buffer))
                                 package))))

(defun read-scheme-symbol (stream &key (package *package*) prefix)
  (loop :for char := (read-case (stream c)
                       (#\(
                        (warn "Style warning: There should be a space before a \"(\" if it is directly following a symbol.")
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
                                        :initial-contents prefix)
                            (make-array 16
                                        :element-type 'character
                                        :adjustable t
                                        :fill-pointer 0))
        :while char
        :do (vector-push-extend char buffer)
        :finally (return (intern (subseq buffer 0 (fill-pointer buffer))
                                 package))))

;;; A dot can represent a possible number (if not, it's a symbol), a
;;; part of a dotted list, or the start of a symbol.
(defun read-scheme-dot (match stream)
  (let ((next-char (peek-char nil stream nil nil)))
    (cond ((and next-char (digit-char-p next-char))
           (unread-char match stream)
           (%read-scheme-number stream 10))
          ((not next-char)
           (eof-error "after a dot"))
          ((%delimiter? stream)
           :dot)
          (t
           (unread-char match stream)
           (read-scheme-symbol stream)))))

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
               (read-char stream)
               :unquote-splicing)
             :unquote))
    (#\; (read-line-comment stream))
    (#\| (read-escaped-scheme-symbol stream))
    (:eof :eof)
    (#\. (read-scheme-dot match stream))
    ((:or :nd :mc :me)
     ;; Note: Many Schemes disregard this rule, but this is mandated
     ;; by section 7.1.1 of r7rs.pdf.
     (error 'scheme-reader-error
            :details #.(concatenate 'string
                                    "An identifier cannot start with a Unicode character "
                                    "in the general categories of Nd, Mc, or Me. To portably "
                                    "do this, first wrap the identifier in vertical lines "
                                    "like |foo|.")))
    (t
     (unread-char match stream)
     (read-scheme-symbol stream))))

(defun read-scheme-character (stream)
  (loop :for match := (let ((match* (read-scheme-character* stream)))
                        (if (eql match* :skip-next)
                            (progn
                              (let ((skipped-read (read-scheme-character stream)))
                                (case skipped-read
                                  (:dot (error 'scheme-reader-error
                                               :details "Attempted to comment out a dot."))
                                  (#\) (error 'scheme-reader-error
                                              :details "Expected to skip a token to match a #;-style comment, but none found."))
                                  (:eof (eof-error "after a #;-style comment"))))
                              (read-scheme-character stream))
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
                      (s-expression
                       t)
                      (t
                       (error 'scheme-reader-error
                              :details "An expression needs an item before the dot in a dotted list")))))
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
                 (t nil))))
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
              :collect (if (member match '(:quote :quasiquote :unquote :unquote-splicing))
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
                           match)
                :into s-expression
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

;;;; -*- mode: common-lisp; -*-

;;; TODO: directives (read in `read-special'; replaces all
;;; %invert-case with fold-case, which has to be done as a string, by
;;; Unicode's rules!)
;;;
;;; TODO: in `read-special', handle labels (for literal circular/etc.
;;; data structures)
;;;
;;; TODO: any missing escapes in `%read-string' and elsehwere, where
;;; also relevant
;;;
;;; TODO: (probably) in `read-scheme-number', read complex, infnan,
;;; the exponent marker e, and the extended s/f/d/l alternate exponent
;;; markers.
;;;
;;; TODO: the quasiquote syntax (` , ,@)
;;;
;;; TODO: finish reading characters

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
;;; the end of the stream after reading the number or else nil (which
;;; will turn into #f) is returned. This is used by string->number.
;;; Because of this, it also disables any errors.
(defun read-scheme-number (stream radix &optional end?)
  (let ((negate? (case (peek-char nil stream nil :eof)
                   (:eof
                    (if end?
                        nil
                        (eof-error "when a number was expected")))
                   ((#\+ #\-)
                    (let ((char (read-char stream)))
                      (char= char #\-)))
                   (t nil))))
    (multiple-value-bind (number length) (read-scheme-integer stream radix)
      (let ((number (cond ((%delimiter? stream)
                           number)
                          ((zerop length)
                           (if end?
                               nil
                               (error 'scheme-reader-error
                                      :details "No number could be read when a number was expected.")))
                          (t
                           (read-case (stream match)
                             (#\/
                              (/ number (read-scheme-integer stream radix)))
                             (#\.
                              (cond ((= 10 radix)
                                     (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
                                       (+ number (/ number* (expt 10d0 length*)))))
                                    (end?
                                     nil)
                                    (t
                                     (error 'scheme-reader-error
                                            :details "A literal flonum must be in base 10."))))
                             (t
                              (unread-char match stream)
                              nil)))))
            (delimiter? (%delimiter? stream)))
        (cond ((not delimiter?)
               (error-unless end?
                             'scheme-reader-error
                             :details "Invalid numerical syntax.")
               nil)
              ((not number)
               nil)
              ;; In CL terminology, this contains "junk" after the
              ;; number... or it's just an empty stream.
              ((and end?
                    (or (not (eql (car delimiter?) :eof))
                        (zerop length)))
               nil)
              (negate?
               (- number))
              (t
               number))))))

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
;;;
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

(defun %find-read-base (stream)
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
      (t 10))))

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
         (number (read-scheme-number stream base)))
    (if exactness
        (funcall exactness number)
        number)))

(defun %read-literal-character (stream)
  (read-case (stream c)
    ((:or #\x #\X)
     (if (%delimiter? stream)
         c
         ;; TODO: note that hex escapes can also appear inside "" and perhaps ||
         #| TODO: read to delimiter for hex escape |#))
    (t
     (if (%delimiter? stream)
         c
         ;; TODO: note that these can also appear inside "" and perhaps ||
         #| TODO: read to delimiter and see if it's a (case-sensitive) character name |#))))

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
    ;; This section is complicated because arbitrary whitespace might
    ;; be between #u8 and its parentheses, especially because of the
    ;; default behavior in paredit for Emacs.
    ;;
    ;; i.e. Paredit produces #u8 () when only #u8() conforms to a
    ;; strict reading of section 7.1.1 of R7RS-small.
    ;;
    ;; If you get this style warning because of paredit, then add the
    ;; following to your .emacs file:
    ;;
    ;;   (setq paredit-space-for-delimiter-predicates '((lambda (endp delimiter) nil)))
    ((:or #\u #\U)
     (read-case (stream c)
       (#\8 (loop :with whitespace? := nil
                  :for c := (read-case (stream c)
                              ((:or #\Space #\Tab #\Newline)
                               (unless whitespace?
                                 (warn #.(concatenate 'string
                                                      "Style warning: In a strict interpretation "
                                                      "of the standard, whitespace between #u8 and "
                                                      "its parentheses would not be permitted.")))
                               (setf whitespace? t)
                               nil)
                              (#\( t)
                              (:eof (eof-error "when a \"(\" was expected"))
                              (t (error 'scheme-reader-error
                                        :details (format nil "\"(\" expected, but ~A was read." c))))
                  :until c
                  :finally
                     (return
                       (handler-case (coerce (scheme-read stream t) 'bytevector?)
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
    ((:or #\e #\E)
     (let ((read-base (%find-read-base stream)))
       (exact (read-scheme-number stream read-base))))
    ((:or #\i #\I)
     (let ((read-base (%find-read-base stream)))
       (inexact (read-scheme-number stream read-base))))
    ((:or #\b #\B)
     (%read-in-base stream 2))
    ((:or #\o #\O)
     (%read-in-base stream 8))
    ((:or #\d #\D)
     (%read-in-base stream 10))
    ((:or #\x #\X)
     (%read-in-base stream 16))
    (#\(
     (coerce (scheme-read stream t) 'vector?))
    (#\;
     :skip-next)
    (:eof
     (eof-error "after a # when a character was expected"))
    (t
     (error 'scheme-reader-error
            :details (format nil "Reader syntax #~A is not supported!" x)))))

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

(defun read-scheme-symbol (stream &optional (package *package*))
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
        :with buffer := (make-array 16
                                    :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
        :while char
        :do (vector-push-extend char buffer)
        :finally (return (intern (subseq buffer 0 (fill-pointer buffer))
                                 package))))

(defun quote-item (item n)
  (loop :repeat n
        :for quoted := `',item :then `',quoted
        :finally (return quoted)))

(defun read-scheme-character (stream)
  (read-case (stream match)
    (#\( (scheme-read stream t))
    (#\) #\))
    (#\" (%read-string stream))
    ((:or (:range #\0 #\9) #\- #\+)
     (unread-char match stream)
     (read-scheme-number stream 10))
    ((:or #\Newline #\Space #\Tab) :skip)
    (#\# (read-special stream))
    (#\' :inc-quoted)
    (#\; (read-line-comment stream))
    (#\| (read-escaped-scheme-symbol stream))
    (:eof :eof)
    (#\. (case (peek-char nil stream nil :eof)
           (#\.
            (unread-char match stream)
            (read-scheme-symbol stream))
           (:eof
            (eof-error "inside of a dotted list"))
           (t :dot)))
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

(defun scheme-read (stream &optional recursive?)
  (flet ((dotted? (match s-expression quoted? before-dotted?)
           (and (eql match :dot)
                before-dotted?
                (cond ((not recursive?)
                       (error 'scheme-reader-error
                              :details "The dotted list syntax must be used inside of a list"))
                      (quoted?
                       (error 'scheme-reader-error
                              :details "A \".\" cannot follow a \"'\""))
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
         (check-end (skip-next old match after-dotted? dotted-end? quote-level)
           (cond ((plusp skip-next)
                  (error 'scheme-reader-error
                         :details "Expected to skip a token to match a #;-style comment, but none found."))
                 ((or (and recursive? (eql match :eof))
                      (and (not recursive?) (eql old #\))))
                  (error 'scheme-reader-error
                         :details "Imbalanced parentheses"))
                 ((and after-dotted? (not dotted-end?))
                  (error 'scheme-reader-error
                         :details "An expression needs an item after the dot in a dotted list"))
                 ((plusp quote-level)
                  (eof-error "after a quote"))
                 (t nil))))
    (loop :with skip-next :of-type a:non-negative-fixnum := 0
          :for old := nil :then (if (and match
                                         (not (eql match :dot)))
                                    match
                                    old)
          :for match := (let ((match* (read-scheme-character stream)))
                          (if (eql match* :skip-next)
                              (progn
                                (incf skip-next)
                                :skip)
                              match*))
            :then (let ((match* (read-scheme-character stream)))
                    (cond ((eql match* :skip-next)
                           (incf skip-next)
                           :skip)
                          ((or (eql match* :eof)
                               (eql match* #\)))
                           match*)
                          ((and (not (eql match* :skip))
                                (plusp skip-next))
                           (decf skip-next)
                           :skip)
                          (t match*)))
          :for prior-quote-level :of-type a:non-negative-fixnum
            := 0 :then quote-level
          :for quote-level :of-type a:non-negative-fixnum
            := (cond ((eql match :skip)
                      (or quote-level 0))
                     ((eql match :inc-quoted)
                      (progn
                        (setf match :skip)
                        (1+ (or quote-level 0))))
                     ((eql match :eof)
                      quote-level)
                     (t 0))
          :for after-dotted? := nil :then (or dotted? after-dotted?)
          :for dotted? := (dotted? match
                                   s-expression
                                   (plusp prior-quote-level)
                                   before-dotted?)
          :for before-dotted? := (eql match :skip)
          :with dotted-end := nil
          :with dotted-end? := nil
          :until (end-of-read? match)
          :if (and (not (eql match :skip))
                   (not dotted?)
                   (not after-dotted?))
            :collect (if (zerop prior-quote-level)
                         match
                         (quote-item match prior-quote-level))
              :into s-expression
          :else
            :if (and (not (eql match :skip)) after-dotted?)
              :do (progn
                    (check-dot dotted-end? match)
                    (setf dotted-end match
                          dotted-end? t))
          :finally
             ;; Note: This isn't an efficient way to make a dotted
             ;; list, but is the efficient way worth the added cost
             ;; when building proper lists?
             (return
               (progn
                 (check-end skip-next old match after-dotted? dotted-end? quote-level)
                 (if dotted-end?
                     (progn
                       (setf (cdr (last s-expression)) dotted-end)
                       s-expression)
                     s-expression))))))

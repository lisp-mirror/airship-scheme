;;;; -*- mode: common-lisp; -*-

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

(define-function (%delimiter? :inline t) (stream)
  (member (peek-char nil stream nil :eof)
          '(#\Space #\Newline #\( #\) #\; #\" #\Tab :eof)))

;;; TODO: Invalid identifier starts need to be invalid.

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

;;; TODO: complex, infnan, exponent marker, and s/f/d/l alternate
;;; exponent markers.
(defun read-scheme-number (stream radix)
  (let ((negate? (case (peek-char nil stream nil :eof)
                   (:eof (error 'scheme-reader-eof-error
                                :details "when a number was expected"))
                   ((#\+ #\-)
                    (let ((char (read-char stream)))
                      (char= char #\-)))
                   (t nil)))
        (number (let ((number (read-scheme-integer stream radix)))
                  (if (%delimiter? stream)
                      number
                      ;; TODO: Peek the stuff at the end after the number. These
                      ;; have to be delimiters, too.
                      (read-case (stream match)
                        (#\/
                         (/ number (read-scheme-integer stream radix)))
                        (#\.
                         (unless (= 10 radix)
                           (error 'scheme-reader-error
                                  :details "A literal flonum must be in base 10."))
                         (multiple-value-bind (number* length*) (read-scheme-integer stream radix)
                           (+ number (/ number* (expt 10d0 length*))))))))))
    (if negate? (- number) number)))

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
                     (error 'scheme-reader-eof-error
                            :details "inside of a block comment"))))

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
;;; TODO: any missing escapes (i.e. hex char and multiline)
;;;
;;; TODO: The escapes are all shared with |foo| symbols aren't they?
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
                             (error 'scheme-reader-eof-error
                                    :details "inside of a string")))))

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
         (:eof (error 'scheme-reader-eof-error
                      :details "after # when a radix was expected"))
         (t (error 'scheme-reader-error
                   :details (format nil "#~A is not a radix" match)))))
      (:eof
       (error 'scheme-reader-eof-error
              :details "when a number was expected"))
      (t
       10))))

(defun %read-in-base (stream base)
  (let* ((next-char (peek-char nil stream nil :eof))
         (exactness (case next-char
                      (#\#
                       (read-char stream nil nil t)
                       (read-case (stream match)
                         ((:or #\e #\E) #'exact)
                         ((:or #\i #\I) #'inexact)
                         (:eof (error 'scheme-reader-eof-error
                                      :details "after # when either E or I was expected"))
                         (t (error 'scheme-reader-error
                                   :details (format nil "#~A is not an exactness/inexactness" match)))))
                      (:eof
                       (error 'scheme-reader-eof-error
                              :details "when a number was expected"))
                      (t
                       nil)))
         (number (read-scheme-number stream base)))
    (if exactness
        (funcall exactness number)
        number)))

;;; TODO: directives, characters, labels
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
    ((:or #\u #\U)
     (read-case (stream c)
       (#\8 (loop :for c := (read-case (stream c)
                              ((:or #\Space #\Tab #\Newline) nil)
                              (#\( t)
                              (:eof (error 'scheme-reader-eof-error
                                           :details "when a \"(\" was expected"))
                              (t (error 'scheme-reader-error
                                        :details "\"(\" expected, but ~A was read." c)))
                  :until c
                  :finally
                     (return
                       (handler-case (coerce (scheme-read stream t) 'bytevector?)
                         (type-error (c)
                           (error 'scheme-type-error
                                  :details "in reading a bytevector"
                                  :datum (type-error-datum c)
                                  :expected-type 'octet))))))
       (:eof (error 'scheme-reader-eof-error
                    :details "after #u when an 8 was expected"))
       (t (error 'scheme-reader-error
                 :details (format nil "#u8 expected, but #u~A was read." c)))))
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
     (error 'scheme-reader-eof-error
            :details "after a # when a character was expected"))
    (t
     (error 'scheme-reader-error
            :details (format nil "Reader syntax #~A is not supported!" x)))))

(defun read-escaped-scheme-symbol (stream &optional (package *package*))
  (loop :for after-escape? := nil :then escape?
        :for char := (read-case (stream c)
                       (#\| (if after-escape? c nil))
                       (#\\ (if after-escape? c :escape))
                       (:eof
                        (error 'scheme-reader-eof-error
                               :details "inside of a |"))
                       ;; TODO: handle other necessary escapes
                       (t (if after-escape?
                              (%invert-case c)
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
            (error 'scheme-reader-eof-error
                   :details "inside of a dotted list"))
           (t :dot)))
    (t
     (unread-char match stream)
     (read-scheme-symbol stream))))

;;; TODO: ` , ,@
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
                  (error 'scheme-reader-eof-error
                         :details "after a quote"))
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

;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme)

;;; TODO: case insensitivity, where valid
;;;
;;; TODO: Invalid identifier starts need to be invalid.

;;; Reads an integer of the given radix
;;;
;;; TODO: Call from read-scheme-number to handle all of the special
;;; syntax of Scheme numbers. Determine if it is a syntax error there.
;;; The entry point might be # or - or + or a digit and potentially
;;; other things.
(defun read-scheme-integer (stream &optional (radix 10))
  (check-type radix (integer 2 16))
  (loop :for match := (read-case (stream x)
                        ((:or (:range #\0 #\9)
                              (:range #\a #\f))
                         (or (digit-char-p x radix)
                             (progn (unread-char x stream) nil)))
                        (:eof nil)
                        (t (unread-char x stream) nil))
        :for length :from 0
        :with number := 0
        :while match
        :do (setf number (+ match (* number radix)))
        :finally (return (values number length))))

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
                     (error "End of file inside of a block comment!"))))

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
                             (error "End of file reached before end of string!")))))

(define-function (%delimiter? :inline t) (stream)
  (member (peek-char nil stream nil :eof)
          '(#\Space #\Newline #\( #\) #\; #\" #\Tab :eof)))

;;; TODO: directives, characters, labels
;;;
;;; TODO: If #e then read the next token and force (exact ...) on the
;;; result. Do the same for #i but with (inexact ...). This might come
;;; before or might come after the radix prefix. The radix prefix s
;;; much simpler, especially since those must be exact, although
;;; e.g. #i#x42 is valid.
(defun read-special (stream)
  (read-case (stream x)
    (#\|
     (read-block-comment stream))
    (#\t
     (if (or (%delimiter? stream)
             (and (loop :for c* :across "rue"
                        :for c := (read-char stream nil nil)
                        :always (and c (eql c c*)))
                  (%delimiter? stream)))
         t
         (error "Invalid character(s) after #t")))
    (#\f
     (if (or (%delimiter? stream)
             (and (loop :for c* :across "alse"
                        :for c := (read-char stream nil nil)
                        :always (and c (eql c c*)))
                  (%delimiter? stream)))
         %scheme-boolean:f
         (error "Invalid character(s) after #f")))
    (#\u
     (read-case (stream c)
       (#\8 (loop :for c := (read-case (stream c)
                              ((:or #\Space #\Tab #\Newline) nil)
                              (#\( t)
                              (:eof (error "\"(\" expected, but end of file reached."))
                              (t (error "\"(\" expected, but ~A was read." c)))
                  :until c
                  :finally
                     (return
                       (let ((s-expression (scheme-read stream t)))
                         ;; TODO: type checking before this point will
                         ;; give a more useful error message...
                         ;; perhaps as a special read path that only
                         ;; reads numbers.
                         (make-array (length s-expression) :initial-contents s-expression
                                                           :element-type 'octet)))))
       (:eof (error "#u8 expected, but end of file reached."))
       (t (error "#u8 expected, but #u~A was read." c))))
    (#\(
     (let ((s-expression (scheme-read stream t)))
       (make-array (length s-expression) :initial-contents s-expression)))
    (#\;
     :skip-next)
    (:eof
     (error "End of file after # when another character was expected!"))
    (t
     (error "Reader syntax #~A is not supported!" x))))

(defun read-escaped-scheme-symbol (stream &optional (package *package*))
  (loop :for after-escape? := nil :then escape?
        :for char := (read-case (stream c)
                       (#\| (if after-escape? c nil))
                       (#\\ (if after-escape? c :escape))
                       (:eof
                        (error "Escaped symbol never closed."))
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
  (if (eql item :eof)
      (error "An EOF cannot be quoted.")
      (loop :repeat n
            :for quoted := `',item :then `',quoted
            :finally (return quoted))))

;;; TODO: ` , ,@
;;;
;;; TODO: non-integer numbers
;;;
;;; TODO: Reread R7RS.pdf and chapter 7
(defun scheme-read (stream &optional recursive?)
  (flet ((read-scheme-character (stream)
           (read-case (stream match)
             (#\( (scheme-read stream t))
             (#\) #\))
             (#\" (%read-string stream))
             ((:range #\0 #\9)
              (unread-char match stream)
              (read-scheme-integer stream))
             ((:or #\Newline #\Space #\Tab) :skip)
             (#\# (read-special stream))
             (#\' :inc-quoted)
             (#\; (read-line-comment stream))
             (#\| (read-escaped-scheme-symbol stream))
             (:eof :eof)
             (#\. (if (char= #\. (peek-char nil stream))
                      (progn
                        (unread-char match stream)
                        (read-scheme-symbol stream))
                      #\.))
             (t
              (unread-char match stream)
              (read-scheme-symbol stream))))
         (dotted? (match s-expression quoted? before-dotted?)
           (and (eql match #\.)
                before-dotted?
                (cond ((not recursive?)
                       (error "The dotted list syntax must be used inside of a list."))
                      (quoted?
                       (error "A \".\" cannot follow a \"'\"."))
                      (s-expression
                       t)
                      (t
                       (error "Invalid dotted list syntax. An expression needs an item before the dot.")))))
         (end-of-read? (match)
           (or (and recursive?
                    (eql match #\)))
               (eql match :eof)))
         ;; TODO: Neither of these check for EOF immediately after a dot
         (check-dot (dotted-end? match)
           (cond (dotted-end?
                  (error "Invalid dotted list syntax. More than one item after a dot in a dotted list."))
                 ((eql match #\.)
                  (error "Invalid dotted list syntax. More than one dot in a list."))
                 (t nil)))
         (check-end (skip-next old match after-dotted? dotted-end?)
           (cond ((plusp skip-next)
                  (error "Expected to skip a token to match a #;-style comment, but none found."))
                 ((or (and recursive? (eql match :eof))
                      (and (not recursive?) (eql old #\))))
                  (error "Imbalanced parentheses."))
                 ((and after-dotted? (not dotted-end?))
                  (error "Invalid dotted list syntax. An expression needs an item after the dot."))
                 (t nil))))
    (loop :with skip-next := 0
          :for old := nil :then (if (and match
                                         (not (eql match #\.)))
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
          :for prior-quote-level := 0 :then quote-level
          :for quote-level := (cond ((eql match :skip)
                                     (or quote-level 0))
                                    ((eql match :inc-quoted)
                                     (progn
                                       (setf match :skip)
                                       (1+ (or quote-level 0))))
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
                 (check-end skip-next old match after-dotted? dotted-end?)
                 (if dotted-end?
                     (progn
                       (setf (cdr (last s-expression)) dotted-end)
                       s-expression)
                     s-expression))))))

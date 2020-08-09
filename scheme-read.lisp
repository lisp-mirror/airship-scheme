;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme)

;;; Reads an integer of the given radix
;;;
;;; TODO: Call from read-scheme-number to handle all of the special
;;; syntax of Scheme numbers. Determine if it is a syntax error there.
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
        :until match))

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
        :finally (unless match
                   (error "End of file inside of a block comment!"))))

;;; Reads a string starting after the initial " that enters the string
;;; reader. A string must end on a non-escaped ".
;;;
;;; TODO: Implement the other kinds of escape as well as any other
;;; missing string features
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
                  (vector-push-extend (case match
                                        (#\n (code-char #x000a))
                                        (#\t (code-char #x0009))
                                        (#\r (code-char #x000d))
                                        (t match))
                                      buffer)
                  (vector-push-extend match buffer))
        :finally (return (if match
                             (subseq buffer 0 (fill-pointer buffer))
                             (error "End of file reached before end of string!")))))

(define-function (%end-of-token? :inline t) (stream)
  (member (peek-char nil stream nil :eof)
          '(#\Space #\Newline #\) #\; #\Tab :eof)))

;;; TODO: vectors, bytevectors, #; comments, directives, characters,
;;; numeric exactness, numeric radixes, labels, etc.
(defun read-special (stream)
  (let ((start (read-case (stream x)
                 (#\| :block-comment)
                 (#\t :maybe-true)
                 (#\f :maybe-false)
                 (:eof :eof)
                 (t x))))
    (case start
      (:eof
       (error "End of file after # when another character was expected!"))
      (:block-comment
       (read-block-comment stream))
      ;; #t or #true is true
      (:maybe-true
       (if (or (%end-of-token? stream)
               (and (loop :for c* :across "rue"
                          :for c := (read-char stream nil nil)
                          :always (and c (eql c c*)))
                    (%end-of-token? stream)))
           t
           (error "Invalid character(s) after #t")))
      ;; #f or #false is false
      (:maybe-false
       (if (or (%end-of-token? stream)
               (and (loop :for c* :across "alse"
                          :for c := (read-char stream nil nil)
                          :do (print c)
                          :always (and c (eql c c*)))
                    (%end-of-token? stream)))
           %scheme-boolean:f
           (error "Invalid character(s) after #f"))))))

(defun read-scheme-symbol (stream &optional (package *package*))
  (loop :for char := (read-case (stream c)
                       ((:or #\Space #\Newline #\) #\; #\Tab)
                        (unread-char c stream)
                        nil)
                       (:eof nil)
                       (t c))
        :with buffer := (make-array 16
                                    :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
        :while char
        :do (vector-push-extend (%invert-case char) buffer)
        :finally (return (intern (subseq buffer 0 (fill-pointer buffer))
                                 package))))

;;; TODO: ' ` , ,@
;;;
;;; TODO: non-integer numbers, including the possibility that symbols
;;; start with integers, such as '1foo
;;;
;;; TODO: make sure . is really . because .., ..., etc., are all valid
;;; symbols instead of failed attempts at writing dotted lists
;;;
;;; TODO: |escaped symbols|
;;;
;;; TODO: everything else
;;;
;;; Recursively collects all characters that aren't integers (read as
;;; base-10 integers), whitespace (ignored), parentheses (used for the
;;; recursion) or part of line comments (ignored) into lists.
(defun scheme-read (stream &optional recursive?)
  (flet ((read-scheme-character (stream)
           (read-case (stream match)
             (#\( (scheme-read stream t))
             (#\) #\))
             (#\" (%read-string stream))
             ((:range #\0 #\9)
              (unread-char match stream)
              (read-scheme-integer stream))
             ((:or #\Newline #\Space #\Tab) :whitespace)
             (#\# (read-special stream))
             (#\; (read-line-comment stream))
             (:eof :eof)
             (#\. #\.)
             (t
              (unread-char match stream)
              (read-scheme-symbol stream)))))
    (loop :for old := nil :then (if (and match
                                         (not (eql match #\.)))
                                    match
                                    old)
          :for match := (read-scheme-character stream)
          :for after-dotted? := nil :then (or dotted? after-dotted?)
          :for dotted? := (and (eql match #\.)
                               (or (and s-expression before-dotted?)
                                   (error "Invalid dotted list syntax. An expression needs an item before the dot.")))
          :for before-dotted? := (eql match :whitespace)
          :with dotted-end := nil
          :with dotted-end? := nil
          :until (or (and recursive?
                          (eql match #\)))
                     (eql match :eof))
          :if (and (not (eql match :whitespace))
                   (not dotted?)
                   (not after-dotted?))
            :collect match :into s-expression
          :else
            :if (and (not (eql match :whitespace)) after-dotted?)
              :do (if dotted-end?
                      (error "Invalid dotted list syntax.")
                      (setf dotted-end match
                            dotted-end? t))
          :finally (cond ((or (and recursive? (eql match :eof))
                              (and (not recursive?) (eql old #\))))
                          (error "Imbalanced parentheses."))
                         ((and after-dotted? (not dotted-end?))
                          (error "Invalid dotted list syntax. An expression needs an item after the dot."))
                         ;; Note: This isn't an efficient way to make
                         ;; a dotted list, but is the efficient way
                         ;; worth the added cost when building proper
                         ;; lists?
                         (t
                          (return (if dotted-end?
                                      (progn
                                        (setf (cdr (last s-expression)) dotted-end)
                                        s-expression)
                                      s-expression)))))))

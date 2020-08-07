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

;;; Reads a character while not in a special state, i.e. it is outside
;;; of a string, a number, etc.
(defun read-scheme-character (stream)
  (read-case (stream x)
    (#\Newline :newline)
    ((:or #\Space #\Tab) :whitespace)
    (#\# :special)
    (#\\ :escape)
    (#\; :comment)
    (:eof nil)
    (t x)))

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

;;; Only these can follow #t, #true, #f, or #false
(define-function (%end-of-special? :inline t) (stream)
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
       (if (or (%end-of-special? stream)
               (and (loop :for c* :across "rue"
                          :for c := (read-char stream nil nil)
                          :always (and c (eql c c*)))
                    (%end-of-special? stream)))
           t
           (error "Invalid character(s) after #t")))
      ;; #f or #false is false
      (:maybe-false
       (if (or (%end-of-special? stream)
               (and (loop :for c* :across "alse"
                          :for c := (read-char stream nil nil)
                          :do (print c)
                          :always (and c (eql c c*)))
                    (%end-of-special? stream)))
           %scheme-boolean:f
           (error "Invalid character(s) after #f"))))))

;;; TODO: ' ` , ,@ .
;;;
;;; TODO: non-integer numbers
;;;
;;; TODO: symbols and |escaped symbols|
;;;
;;; TODO: everything else
;;;
;;; Recursively collects all characters that aren't integers (read as
;;; base-10 integers), whitespace (ignored), parentheses (used for the
;;; recursion) or part of line comments (ignored) into lists.
(defun scheme-read (stream &optional recursive?)
  (loop :for old := nil :then match
        :for match := (read-scheme-character stream)
        :for temp := nil
        :until (or (and recursive?
                        (eql match #\)))
                   (null match))
        :if (eql match :comment)
          :do (read-line-comment stream)
        :else
          :if (eql match :special)
            :do (setf temp (read-special stream))
        :else
          :if (not (or (eql match :whitespace) (eql match :newline)))
            :collect (cond ((eql match #\()
                            (scheme-read stream t))
                           ((eql match #\")
                            (%read-string stream))
                           ((and (characterp match) (char<= #\0 match #\9))
                            (unread-char match stream)
                            (read-scheme-integer stream))
                           (t match))
        :when temp
          :collect temp
        :finally (when (or (and recursive? (not match))
                           (and (not recursive?) (eql old #\))))
                   (error "Imbalanced parentheses."))))

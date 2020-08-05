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

;;; TODO: Stub
;;;
;;; TODO: Check for balanced parentheses before EOF
;;;
;;; Recursively collects all characters that aren't integers (read as
;;; base-10 integers), whitespace (ignored), parentheses (used for the
;;; recursion) or part of line comments (ignored) into lists.
(defun scheme-read (stream &optional recursive?)
  (loop :for match := (read-scheme-character stream)
        :for in-comment? := (or (eql match :comment)
                                (and in-comment?
                                     (not (or (eql match :newline)
                                              (eql match :eof)))))
        :until (or (and recursive?
                        (eql match #\)))
                   (null match))
        :unless (or in-comment? (eql match :whitespace))
          :collect (cond ((eql match #\()
                          (scheme-read stream t))
                         ((and (characterp match) (char<= #\0 match #\9))
                          (unread-char match stream)
                          (read-scheme-integer stream))
                         (t match))))

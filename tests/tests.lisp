;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme/tests)

(defun read-scheme* (string)
  (with-input-from-string (stream string)
    (scheme::read-scheme stream)))

(5am:def-suite airship-scheme/tests)

(5am:def-suite airship-scheme/scheme-read
  :in airship-scheme/tests)

(5am:in-suite airship-scheme/scheme-read)

(5am:test boolean
  "Are true and false read correctly?"
  (5am:is (eq (read-scheme* "#t") t))
  (5am:is (eq (read-scheme* "#f") %scheme-boolean:f)))

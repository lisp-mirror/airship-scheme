;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme)

;; TODO: Stub
(defun scheme-read (stream)
  (loop :for match := (read-case (stream x)
                        ((:range #\0 #\9) (format t "~S~%" x) (cons :digit x))
                        ((:range #\A #\Z) (format t "~S~%" x) (cons :letter x))
                        ((:range #\a #\z) (format t "~S~%" x) (cons :letter x))
                        ;; ((:or #\Space #\Newline #\Tab) (format t "~S~%" x) (cons :whitespace x))
                        ;; ((:or #\( #\)) (format t "~S~%" x) (cons :parens x))
                        )
        :while match
        :collect match))

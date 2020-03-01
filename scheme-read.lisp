;; TODO: temporary name
(in-package #:scheme)

;; TODO: Stub
(defun scheme-read (stream)
  (read-case (stream c)
    ((:range #\0 #\9) (format t "~S~%" c) (cons :digit c))
    ((:range #\A #\Z) (format t "~S~%" c) (cons :alphanumeric c))
    ((:range #\a #\z) (format t "~S~%" c) (cons :alphanumeric c))))

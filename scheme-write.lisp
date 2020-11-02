;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme)

;;; Note: ignores short-float and long-float in SBCL to avoid having
;;; an unreachable code note.
(defun write-flonum-suffix (number stream)
  "
Writes the suffix of a special flonum. This assumes that a double
float is the default, writing no suffix.
"
  (etypecase number
    (double-float
     nil)
    (single-float
     (write-string "f0" stream)
     nil)
    #-sbcl
    (short-float
     (write-string "s0" stream)
     nil)
    #-sbcl
    (long-float
     (write-string "l0" stream)
     nil)))

(defun write-scheme-number (number &optional (stream *standard-output*) (*print-base* 10))
  "Writes a number in the way that Scheme reads numbers."
  (let ((stream (if (eq stream t) *standard-output* stream)))
    (etypecase number
      (rational
       (if (> *print-base* 10)
           (format stream "~A" (string-downcase (format nil "~A" number)))
           (format stream "~A" number)))
      (float (let ((*read-default-float-format* 'double-float))
               (cond ((infinitep number)
                      (format stream "~:[-~;+~]inf.0" (plusp number))
                      (write-flonum-suffix number stream))
                     ((nanp number)
                      (format stream "~:[+~;-~]nan.0" (sign-bit? number))
                      (write-flonum-suffix number stream))
                     (t
                      (format stream "~A" number)))))
      (complex
       (write-scheme-number (realpart number) stream *print-base*)
       (when (and (finitep (imagpart number)) (plusp (imagpart number)))
         (write-char #\+ stream))
       (write-scheme-number (imagpart number) stream *print-base*)
       (write-char #\i stream))))
  nil)

;;; TODO: write-scheme

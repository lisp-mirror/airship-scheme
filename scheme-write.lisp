;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme)

(defun write-scheme-number (number &optional (stream *standard-output*) (*print-base* 10))
  "Writes a number in the way that Scheme reads numbers."
  (let ((stream (if (eq stream t) *standard-output* stream)))
    (etypecase number
      (rational
       (if (> *print-base* 10)
           (format stream "~A" (string-downcase (format nil "~A" number)))
           (format stream "~A" number)))
      ;; Note: This doesn't round-trip floats to the same floating
      ;; point type at the moment. Every float is read in as a double.
      (float (let ((*read-default-float-format* (type-of number)))
               (cond ((infinitep number)
                      (if (plusp number)
                          (write-string "+inf.0" stream)
                          (write-string "-inf.0" stream)))
                     ;; TODO: is there a NaN sign test?
                     ((nanp number) (write-string "+nan.0" stream))
                     (t (format stream "~A" number)))))
      (complex
       (write-scheme-number (realpart number) stream *print-base*)
       (when (plusp (imagpart number))
         (write-char #\+ stream))
       (write-scheme-number (imagpart number) stream *print-base*)
       (write-char #\i stream))))
  nil)

;;; TODO: write-scheme

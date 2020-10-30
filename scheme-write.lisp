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
      ;; TODO: handle short/long float in Lisps that have them (s0, l0)
      (float (let ((*read-default-float-format* 'double-float))
               (cond ((infinitep number)
                      (format stream
                              "~:[-~;+~]inf.0~:[~;f0~]"
                              (plusp number)
                              (typep number 'single-float)))
                     ;; TODO: the NaN sign can be accessed through float-features
                     ((nanp number)
                      (format stream
                              "+nan.0~:[~;f0~]"
                              (typep number 'single-float)))
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

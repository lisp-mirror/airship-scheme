;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme)

(define-function (make-adjustable-string :inline t) (&optional (length 16))
  "Creates an adjustable string of the given initial length."
  (make-array length
              :element-type 'character
              :adjustable t
              :fill-pointer 0))

(define-function (nil-to-false :inline t) (item)
  "
Handles the result of a CL predicate, which uses NIL as its false
value, by turning nil into the #f that Airship Scheme expects.
"
  (if item item '%scheme-boolean:f))

(define-function (false-to-nil :inline t) (item)
  "
Processes the result of a Scheme procedure by turning Scheme's #f into
the NIL that CL expects to be the false value.
"
  (if (eq item '%scheme-boolean:f) nil item))

;;; Note: This is intentionally not a Unicode-friendly case inversion.
;;; This is a case inversion that is compatible with the upcasing done
;;; by a Common Lisp reader. This is ideally a self-inverse function.
;;; Respecting Unicode rules would mean, e.g. sigma and final sigma
;;; would have the same upper case form.
;;;
;;; If the assumption is false in the major CL implementations, then
;;; more complicated logic would need to be done.
(define-function (%invert-case :inline t :return character) ((character character))
  "
Inverts the case of a character in a CL-compatible way rather than in
a Unicode-proper way.
"
  (cond ((upper-case-p character) (char-downcase character))
        ((lower-case-p character) (char-upcase character))
        (t character)))

(define-function (invert-case :return simple-string) ((simple-string simple-string))
  "
Inverts the case of a string, representing a symbol name, for maximum
CL interoperability. Source code is typically written in lower case
and in CL that is typically then upper-cased, so inverting the case
will allow case sensitivity in Scheme while still keeping the symbols
in a form that CL expects.
"
  (map 'simple-string
       #'%invert-case
       simple-string))

(define-function (scheme-symbol-name :inline t) ((symbol symbol))
  "Interns a Scheme symbol using one package, with its case inverted."
  (invert-case (symbol-name symbol)))

(define-function (scheme-symbol :inline t) ((string simple-string))
  "Interns a Scheme symbol using one package, with its case inverted."
  (intern (invert-case string) '#:r7rs))

(defun coerce-subseq (sequence result-type &optional start end)
  "Coerces a subsequence into the result type"
  (let ((subseq (if start
                    (subseq sequence start end)
                    sequence)))
    (coerce subseq result-type)))

(defun copy-seq-or-subseq (sequence &optional start end)
  "Either copies a subsequence or a sequence"
  (if start
      (subseq sequence start end)
      (copy-seq sequence)))

(define-function (skip-read-char :inline t) (stream)
  "Call this when the result is to be ignored."
  (read-char stream nil nil t))

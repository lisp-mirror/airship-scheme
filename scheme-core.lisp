(in-package #:airship-scheme)

;;; Symbols are read into this package.
(defpackage #:r7rs
  (:use))

(deftype bytevector ()
  '(simple-array octet))

(deftype scheme-boolean ()
  `(or (eql t) (eql %scheme-boolean:f)))

(deftype scheme-vector ()
  `simple-vector)

;;; Turns a CL predicate with NIL-as-false into the #f that Airship
;;; Scheme expects.
(define-function (nil-to-false :inline t) (item)
  (if item item '%scheme-boolean:f))

;;; Converts any #f value that the Scheme is using into the NIL that
;;; CL expects to use as false.
(define-function (false-to-nil :inline t) (item)
  (if (eq item '%scheme-boolean:f) nil item))

;;; Invert case for CL interoperability.
(define-function (invert-case :return simple-string) ((simple-string simple-string))
  (map 'simple-string
       (lambda (c)
         (cond ((upper-case-p c) (char-downcase c))
               ((lower-case-p c) (char-upcase c))
               (t c)))
       simple-string))

(define-function (nanp :inline t) ((number number))
  (and (floatp number) (float-features:float-nan-p number)))

(define-function (infinitep :inline t) ((number number))
  (and (floatp number) (float-features:float-infinity-p number)))

(define-function (finitep :inline t) ((number number))
  (not (and (floatp number) (or (infinitep number) (nanp number)))))

(define-function (scheme-boolean-p :inline t) (object)
  (typep object 'scheme-boolean))

(define-function (scheme-symbol-p :inline t) (object)
  (and object
       (symbolp object)
       (not (scheme-boolean-p object))))

;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme)

;;;; Type Definitions

(deftype exact ()
  "An exact number might be real or complex, but is not a float."
  `(or rational (complex rational)))

(deftype inexact ()
  "An inexact number is just a float, real or complex."
  `(or float (complex float)))

(deftype scheme-boolean ()
  "
The two symbols that represent a Scheme Boolean, which externally are
known as #t or #f
"
  `(or (eql t) (eql %scheme-boolean:f)))

(deftype scheme-vector ()
  "A Scheme vector is just a T vector"
  `simple-vector)

(deftype scheme-string ()
  "A Scheme string is just a simple string."
  'simple-string)

(deftype bytevector ()
  "A Scheme bytevector is just an octet vector"
  `(simple-array octet))

;;;; Type Predicates

(define-function (exactp :inline t) (number)
  "Tests if a number is exact"
  (and (typep number 'exact) t))

(define-function (inexactp :inline t) (number)
  "Tests if a number is inexact"
  (and (typep number 'inexact) t))

(define-function (scheme-boolean-p :inline t) (object)
  "Tests if an object is either a Scheme #t or a Scheme #f"
  (and (typep object 'scheme-boolean) t))

(define-function (scheme-symbol-p :inline t) (object)
  "Tests if an object is a Scheme symbol"
  (and object
       (symbolp object)
       (not (scheme-boolean-p object))))

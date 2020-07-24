;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme)

;;;; Define type definitions

;;; TODO: The define-scheme-predicate could go here, too
(defmacro define-scheme-type ((name &rest lambda-list) &body body)
  (let ((docstring (if (and (stringp (car body)) (cdr body))
                       (list (car body))
                       nil)))
    `(progn
       (deftype ,name ,lambda-list
         ,@body)
       (define-function (,name :inline t) (object)
         ,@docstring
         (and (typep object ',name) t)))))

;;;; Type definitions

(define-scheme-type (exact?)
  "An exact number might be real or complex, but is not a float."
  `(or rational (complex rational)))

(define-scheme-type (exact-integer?)
  "An exact integer in Scheme is just a CL integer."
  `integer)

(define-scheme-type (inexact?)
  "An inexact number is just a float, real or complex."
  `(or float (complex float)))

(define-scheme-type (flonum?)
  "A Scheme flonum is just a double-float."
  'double-float)

(define-scheme-type (boolean?)
  "
The two symbols that represent a Scheme Boolean, which externally are
known as #t or #f
"
  `(or (eql t) (eql %scheme-boolean:f)))

(define-scheme-type (vector?)
  "A Scheme vector is just a T vector"
  `simple-vector)

(define-scheme-type (string?)
  "A Scheme string is just a simple string."
  'simple-string)

(define-scheme-type (bytevector?)
  "A Scheme bytevector is just an octet vector"
  `(simple-array octet (*)))

(define-scheme-type (symbol?)
  "Tests if an object is a Scheme symbol"
  `(and symbol (not null) (not boolean?)))

;;;; Type Conversion

(define-function (inexact :inline t) ((z number))
  "Converts a number to a Scheme inexact."
  (etypecase z
    ((and complex exact?) (coerce z '(complex double-float)))
    (exact? (coerce z 'double-float))
    (number z)))

;;; Note: This uses rationalize. cl:rationalize is not the same thing
;;; as Scheme's rationalize. Racket's inexact->exact behaves more like
;;; cl:rational instead, but rationalize produces less surprising
;;; fractions.
(define-function (exact :inline t) ((z number))
  "Converts a number to a Scheme exact."
  (etypecase z
    ((and complex inexact?) (complex (rationalize (realpart z))
                                    (rationalize (imagpart z))))
    (inexact? (rationalize z))
    (number z)))

;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme)

;;;; Define type definitions

;;;; TODO: The define-scheme-predicate could go here, too
(defmacro %define-scheme-type ((name &rest lambda-list) predicate &body body)
  (let ((docstring (if (and (stringp (car body)) (cdr body))
                       (list (car body))
                       nil)))
    `(progn
       (deftype ,name ,lambda-list
         ,@body)
       (define-function (,name :inline t) (object)
         ,@docstring
         (and ,predicate t)))))

;;; For types with no built-in predicate
(defmacro define-scheme-type ((name &rest lambda-list) &body body)
  `(%define-scheme-type (,name ,@lambda-list) (typep object ',name)
     ,@body))

;;; For CL types that use a predicate instead of typep
(defmacro define-scheme-type* ((name &rest lambda-list) predicate &body body)
  `(%define-scheme-type (,name ,@lambda-list) (,predicate object)
     ,@body))

;;;; Type definitions

(define-scheme-type* (number?) numberp
  'number)

(define-scheme-type* (complex?) numberp
  'number)

(define-scheme-type* (real?) realp
  'real)

(define-scheme-type (rational?)
  '(or rational float))

;;; TODO: integer?

(define-scheme-type (exact?)
  "An exact number might be real or complex, but is not a float."
  `(or rational (complex rational)))

(define-scheme-type (inexact?)
  "An inexact number is just a float, real or complex."
  `(or float (complex float)))

(define-scheme-type (flonum?)
  'double-float)

(define-scheme-type (exact-integer?)
  `integer)

(define-scheme-type* (zero?) zerop
  '(or (real 0 0) (complex (real 0 0))))

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

(define-scheme-type (char?)
  "A Scheme char is just a character."
  'character)

(define-scheme-type (bytevector?)
  "A Scheme bytevector is just an octet vector"
  `(simple-array octet (*)))

(define-scheme-type (symbol?)
  "Tests if an object is a Scheme symbol"
  `(and symbol (not null) (not boolean?)))

(define-scheme-type* (list?) a:proper-list-p
  "Scheme's list? tests for a proper list"
  `a:proper-list)

(define-scheme-type* (%list?) listp
  "
A lower-level, faster list test that permits improper lists, which
don't end in NIL.
"
  `list)

(define-scheme-type* (pair?) consp
  "A pair? in Scheme is a cons cell."
  `cons)

(define-scheme-type* (null?) null
  "A null? in Scheme is nil."
  `null)

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

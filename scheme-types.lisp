;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme)

;;;; Helper functions useful for SATISFIES types or standalone tests

(define-function (mathematical-integer-p :inline t) ((number number))
  (zerop (nth-value 1 (round number))))

(define-function (%nanp :inline t) ((number number))
  (and (floatp number) (f:float-nan-p number)))

(defun nanp (number)
  "Tests if a number is NaN"
  (or (%nanp number)
      (and (complexp number)
           (or (%nanp (realpart number))
               (%nanp (imagpart number))))))

(define-function (%infinitep :inline t) ((number number))
  (and (floatp number) (f:float-infinity-p number)))

(defun infinitep (number)
  "Tests if a number is an infinity"
  (or (%infinitep number)
      (and (complexp number)
           (or (%infinitep (realpart number))
               (%infinitep (imagpart number))))))

(define-function (finitep :inline t) ((number number))
  "Tests if a number is both not NaN and not an infinity"
  (not (or (infinitep number) (nanp number))))

;;;; Type definition macros

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

;;;; Numeric types

(define-scheme-type* (number?) numberp
  'number)

(define-scheme-type* (complex?) numberp
  'number)

(define-scheme-type* (real?) realp
  'real)

(define-scheme-type (rational?)
  `(or rational float))

(define-scheme-type (integer?)
  "
A Scheme integer? is a mathematical integer, which means that it is
either a CL integer or it is a number (probably a float) that
satisfies the mathematical definition of an integer. Since this is a
SATISFIES type, it should be used sparingly.
"
  `(or integer
       (and number (satisfies mathematical-integer-p))))

(define-scheme-type (exact?)
  "An exact number might be real or complex, but is not a float"
  `(or rational (complex rational)))

(define-scheme-type (inexact?)
  "An inexact number is just a float, real or complex"
  `(or float (complex float)))

(define-scheme-type (flonum?)
  'double-float)

(define-scheme-type (exact-integer?)
  "An exact integer is anything of the low-level CL integer type"
  'integer)

(define-scheme-type* (finite?) finitep
  "Any number that is neither an infinity nor a NaN"
  `(satisfies finitep))

(define-scheme-type* (infinite?) infinitep
  "Any floating point infinity"
  `(satisfies infinitep))

(define-scheme-type* (nan?) nanp
  "Any float that's a NaN"
  `(satisfies nanp))

(define-scheme-type* (zero?) zerop
  "
A zero? is any way to represent zero, real or complex. A complex zero
can exist if floating point.
"
  ;; Note: (complex (float 0 0)) might also work instead of (complex
  ;; (real 0 0))
  `(or (real 0 0)
       (complex (real 0 0))))

;;;; Other types

(define-scheme-type (boolean?)
  "
The two symbols that represent a Scheme Boolean, which externally are
known as #t or #f
"
  `(or (eql t) (eql %scheme-boolean:f)))

(define-scheme-type (vector?)
  "A Scheme vector is just a T vector"
  'simple-vector)

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
  'a:proper-list)

(define-scheme-type* (%list?) listp
  "
A lower-level, faster list test that permits improper lists, which
don't end in NIL.
"
  'list)

(define-scheme-type* (pair?) consp
  "A pair? in Scheme is a cons cell."
  'cons)

(define-scheme-type* (null?) null
  "A null? in Scheme is nil."
  'null)

(define-scheme-type* (port?) streamp
  'stream)

(define-scheme-type* (input-port?) input-stream-p
  `(satisfies input-stream-p))

(define-scheme-type* (output-port?) output-stream-p
  `(satisfies output-stream-p))

;;;; Type creation

;;; TODO: handle short/long float in Lisps that have them (s0, l0)
(defun sign-bit? (float)
  "
Determines if the sign bit is 1 or not, which is used in the creation
of NaNs.
"
  (etypecase float
    (single-float (logbitp (- (expt 2 5) 1) (f:single-float-bits float)))
    (double-float (logbitp (- (expt 2 6) 1) (f:double-float-bits float)))))

(defun nan (float-type &optional negate?)
  "
If possible, this creates a NaN with the given sign and of the given
type of float.

This is used for literal NaNs in the Scheme reader.
"
  (and float-type
       (let* ((zero (coerce 0 float-type))
              (nan (f:with-float-traps-masked t (/ zero zero)))
              (-nan? (sign-bit? nan)))
         (if negate?
             (if -nan? nan (- nan))
             (if -nan? (- nan) nan)))))

(define-function (inf :inline t) (float-type &optional negate?)
  "
If possible, this creates a positive or negative infinity of the given
type of float.

This is used for literal infinities in the Scheme reader.
"
  (declare (optimize (speed 3)))
  ;; Tell SBCL not to warn us about unreachable branches being deleted
  ;; because that's kind of the point of inlining this function.
  (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
    (case float-type
      (double-float
       (if negate?
           f:double-float-negative-infinity
           f:double-float-positive-infinity))
      (single-float
       (if negate?
           f:single-float-negative-infinity
           f:single-float-positive-infinity))
      (long-float
       (if negate?
           f:long-float-negative-infinity
           f:long-float-positive-infinity))
      (short-float
       (if negate?
           f:short-float-negative-infinity
           f:short-float-positive-infinity))
      (t nil))))

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

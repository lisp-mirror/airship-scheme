(in-package #:airship-scheme)

;;; Symbols are read into this package.
(defpackage #:r7rs
  (:use))

(deftype bytevector ()
  "A Scheme bytevector is just an octet vector"
  `(simple-array octet))

(deftype scheme-boolean ()
  "
The two symbols that represent a Scheme Boolean, which externally are
known as #t or #f
"
  `(or (eql t) (eql %scheme-boolean:f)))

(deftype scheme-vector ()
  "A Scheme vector is just a T vector"
  `simple-vector)

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

(define-function (invert-case :return simple-string) ((simple-string simple-string))
  "
Inverts the case of a string, representing a symbol name, for maximum
CL interoperability. Source code is typically written in lower case
and in CL that is typically then upper-cased, so inverting the case
will allow case sensitivity in Scheme while still keeping the symbols
in a form that CL expects.
"
  (map 'simple-string
       (lambda (c)
         (cond ((upper-case-p c) (char-downcase c))
               ((lower-case-p c) (char-upcase c))
               (t c)))
       simple-string))

(define-function (scheme-symbol :inline t) ((symbol symbol))
  "Interns a Scheme symbol using one package, with its case inverted."
  (intern (invert-case (symbol-name symbol))
          '#:r7rs))

(define-function (nanp :inline t) ((number number))
  "Tests if a number is NaN"
  (and (floatp number) (float-features:float-nan-p number)))

(define-function (infinitep :inline t) ((number number))
  "Tests if a number is an infinity"
  (and (floatp number) (float-features:float-infinity-p number)))

(define-function (finitep :inline t) ((number number))
  "Tests if a number is both not NaN and not an infinity"
  (not (and (floatp number) (or (infinitep number) (nanp number)))))

;;; TODO: fixme: This old function might be defining Scheme exactness
;;; incorrectly.
(defun exactp (number)
  "Tests if a number is exact"
  (cond ((rationalp number) t)
        ((complexp number) (and (rationalp (realpart number))
                                (rationalp (imagpart number))))
        (t nil)))

(define-function (scheme-boolean-p :inline t) (object)
  "Tests if an object is either a Scheme #t or a Scheme #f"
  (and (typep object 'scheme-boolean) t))

(define-function (scheme-symbol-p :inline t) (object)
  "Tests if an object is a Scheme symbol"
  (and object
       (symbolp object)
       (not (scheme-boolean-p object))))

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

(defun generate-lambda-list (list)
  "Generates a lambda list for `define-scheme-procedure'"
  (typecase list
    (cons (loop :for sublist :on list
                :by (lambda (x)
                      (let ((rest (rest x)))
                        (if (not (listp rest))
                            `(&rest ,rest)
                            rest)))
                :collect (car sublist)))
    (t `(&rest ,list))))

;;; TODO: The external-to-CL versions of these procedures should call
;;; the function within the trampoline with #'identity as the
;;; continuation.
(defmacro define-scheme-procedure (variable-and-scheme-lambda-list &body body)
  "Defines a Scheme procedure based on a Common Lisp body."
  (destructuring-bind (name &rest arguments)
      variable-and-scheme-lambda-list
    (let ((arguments (if (listp arguments)
                         (cdr (generate-lambda-list variable-and-scheme-lambda-list))
                         `(&rest ,arguments)))
          (continuation (gensym #.(symbol-name '#:c))))
      `(define-function ,(intern (symbol-name name) '#:r7rs) ,(list* `(,continuation function) arguments)
         (multiple-value-call ,continuation (progn ,@body))))))

(defmacro define-scheme-predicate ((variable &rest arguments) &body body)
  "
Defines a Scheme procedure based on a Common Lisp body, while also
converting a NIL return value to #f
"
  `(define-scheme-procedure (,variable ,@arguments)
     (nil-to-false (progn ,@body))))

(defmacro define-scheme-cxr ((variable pair))
  "
Defines a CXR procedure (e.g. CAR) with Scheme's slightly different
rules for input.
"
  `(define-scheme-procedure (,variable ,pair)
     (if (null ,pair)
         (error "Attempted to use a cxr operation on an empty list")
         (,variable ,pair))))

;;; TODO: This is temporary. When Scheme library support is added, the
;;; libraries would actually generate something almost like this, but
;;; only for the symbols that are specified in the library definition,
;;; with potential renaming as a possibility.
(defmacro with-r7rs-global-environment (&body body)
  "
Puts every R7RS procedure into one big LET to achieve Lisp-1 behavior
from within the host Lisp-2. This means that FUNCALL (or
MULTIPLE-VALUE-CALL) is always required internally within the Scheme
when calling procedures. That is, procedures and variables now share
the same namespace, which is 'global' because this is the parent
environment to all Scheme-defined procedures.

e.g. Scheme's (foo 42) is really (funcall foo continuation 42)

Direct usage of this macro would look like this:

   (with-r7rs-global-environment
     (funcall r7rs::odd? #'identity 1))

Written in Scheme, it would look like this:

   (odd? 1)

And the return value would be printed as T if the result is printed as
Common Lisp or #t if the result is printed as Scheme.
"
  (let* ((standard-procedures (let ((l (list)))
                                (do-symbols (s :r7rs l)
                                  (push s l))))
         (procedure-variables (mapcar (lambda (s)
                                        `(,s (function ,s)))
                                      standard-procedures)))
    `(let ,procedure-variables
       (declare (ignorable ,@standard-procedures))
       ,@body)))

;;; TODO:

(defun cps-transform-procedure (continuation identifier rest)
  (format t "parent: ~S~%" identifier)
  (loop :for item :in rest
        :do (cps-transform continuation item)))

(defun cps-transform (continuation expression)
  (etypecase expression
    (list (destructuring-bind (identifier &rest rest) expression
            (cps-transform-procedure continuation identifier rest)
            (let ((g (gensym)))
              (format t "~S~%" `(lambda (,g) (,identifier ,continuation ,g))))))
    (symbol
     (format t "~S~%" expression)
     expression)))

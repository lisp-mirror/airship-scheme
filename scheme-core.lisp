;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme)

(defpackage #:r7rs
  (:documentation "A package that the core R7RS symbols are read into.")
  (:use))

(defpackage #:%scheme-thunk
  (:documentation "A package to namespace the internal Scheme thunk.")
  (:use)
  (:export #:thunk))

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
    (null nil)
    (t `(&rest ,list))))

;;; TODO: The external-to-CL versions of these procedures should call
;;; the function within the trampoline with #'values as the
;;; continuation.
(defmacro %define-scheme-procedure ((name continuation &rest scheme-lambda-list) &body body)
  "
Defines a Scheme procedure with a Common Lisp body and an explicit
continuation.
"
  `(define-function ,(intern (symbol-name name) '#:r7rs)
       ,(list* `(,continuation function) (generate-lambda-list scheme-lambda-list))
     (multiple-value-call ,continuation (progn ,@body))))

;;; TODO: Explicit continuation in the call to %define
(defmacro define-scheme-procedure ((name &rest scheme-lambda-list) &body body)
  "Defines a Scheme procedure based on a Common Lisp body."
  `(%define-scheme-procedure (,name ,(gensym #.(symbol-name '#:k)) ,@scheme-lambda-list)
     ,@body))

(defmacro define-scheme-predicate ((name &rest scheme-lambda-list) &body body)
  "
Defines a Scheme procedure based on a Common Lisp body, while also
converting a NIL return value to #f
"
  `(define-scheme-procedure (,name ,@scheme-lambda-list)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cps-transform-procedure (continuation identifier rest)
    (loop :with items := (reverse rest)
          :for item :in items
          :for gensym := (if (listp item) (gensym (symbol-name '#:k)) nil)
          :when gensym
            :collect (list gensym item) :into gensyms
          :collect (if gensym gensym item) :into args
          :finally
             ;; returns either a continuation or the top-level
             ;; continuation function call
             (return (loop :with k* := `(funcall ,identifier ,continuation ,@(reverse args))
                           :for (gensym item) :in gensyms
                           :for k := (funcall (cps-transform* gensym item) (or k k*))
                           :finally (return (or k k*))))))

  (defun cps-transform* (gensym expression)
    (let ((gensym (or gensym (gensym))))
      (lambda (continuation)
        `(lambda (,gensym)
           ,(typecase expression
              ;; Note: Assumes the Scheme boolean, not the CL boolean.
              (null (error "Syntax Error: () is an empty procedure call."))
              (list (destructuring-bind (identifier-or-expression &rest rest) expression
                      (etypecase identifier-or-expression
                        (list
                         (let ((k (gensym (symbol-name '#:k))))
                           (funcall (cps-transform* k identifier-or-expression)
                                    (funcall (cps-transform* continuation (cons k rest)) gensym))))
                        (symbol
                         (case identifier-or-expression
                           ;; TODO: ensure that if hasn't been redefined
                           ;;
                           ;; TODO: Replace IF with a simplified transformation
                           ;;
                           ;; (r7rs::if
                           ;;  (destructuring-bind (test then &optional else) rest
                           ;;    (let* ((k (if (listp test)
                           ;;                  (gensym (symbol-name '#:k))
                           ;;                  test))
                           ;;           (then (cps-transform continuation then))
                           ;;           (else (if else
                           ;;                     (cps-transform continuation else)
                           ;;                     ;; Note: unspecified
                           ;;                     ''%scheme-boolean:f))
                           ;;           ;; Note: uses the Scheme boolean
                           ;;           (continuation-branch `(if (eq ,k '%scheme-boolean:f)
                           ;;                                     ,else
                           ;;                                     ,then)))
                           ;;      (if (listp test)
                           ;;          (cps-transform `(lambda (,k) ,continuation-branch) test)
                           ;;          continuation-branch))))
                           (t (cps-transform-procedure continuation identifier-or-expression rest)))))))
              ;; (symbol expression)
              (t expression))))))

  ;; TODO: remove the transformation when it's not necessary
  (defun cps-transform (expression)
    (let ((k (gensym (symbol-name '#:k))))
      (funcall (cps-transform* k expression) k))))

;;; example:
;;; (let ((x 2) (y 3))
;;;   (with-cps-transform #'identity (r7rs::+ (r7rs::* x x) y)))
(defmacro with-cps-transform (expression)
  "Applies a continuation passing style transform to the expression."
  (cps-transform expression))

(define-function (thunk? :inline t) (object)
  "Determines if an object is a thunk."
  (and (listp object)
       (eq (car object) '%scheme-thunk:thunk)))

(define-function (call-next :inline t) (thunk)
  "Calls the contents of a thunk."
  (funcall (cdr thunk)))

(defun trampoline (object)
  "
Iterates through tail-recursive functions that are wrapped in a thunk
until it stops getting thunks.
"
  (do ((item object (call-next item)))
      ((not (thunk? item)) item)))

(defmacro thunk (object)
  "Creates a thunk."
  `(cons '%scheme-thunk:thunk
         (lambda () ,object)))

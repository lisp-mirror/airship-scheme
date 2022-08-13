(cl:in-package #:airship-scheme)

;;;; This file implements Common Lisp macro equivalents for macros or
;;;; special forms that only exist inside of Scheme. This is because
;;;; Airship Scheme exists to expose Scheme functionality to Common
;;;; Lisp, not just as an implementation of Scheme inside of Common
;;;; Lisp.

;;; TODO: Also support define-function, which has no equivalent in
;;; Scheme because Scheme is a Lisp-1. This makes this macro
;;; significantly more complicated.
;;;
;;; Note that this is the internal form of BEGIN rather than the
;;; top-level form of BEGIN.
(defmacro begin (&body body)
  "
This Scheme-style macro BEGIN behaves like the Common Lisp PROGN,
except any number of variable DEFINEs at the start of the BEGIN macro
are turned into bindings of a LET*. The Lisp-2 nature of Common Lisp
means that the DEFINE only defines variables, not functions (in
Scheme, called procedures).

This is actually a more powerful form of BEGIN than the Scheme
standard permits because Schemes are allowed to fail on something like
this:

  (+ (begin (define x 42) (+ x x)) 3)

This is because begin-with-defines in Scheme is just passing through
the defines to a place where it is potentially valid, such as a
top-level form or in the body of a procedure's define or lambda.

On the other hand, this macro is handling the internal defines inside
of the begin itself. This will fail to behave like Scheme at the top
level, but work as expected inside of things like DEFUN. It's just
that it will also work in places where it might fail in some Schemes.

In this sense, Common Lisp does things backwards compared to Scheme.
Some Common Lisp macros have implicit PROGNs so Scheme-style macros in
Common Lisp should have implicit BEGINs that behave like extended
PROGNs, whereas Scheme's BEGIN just passes things through.
"
  (loop :for sublist :on body
        :for item := (car sublist)
        :while (and (listp item) (eql (car item) 'define))
        :collect (cdr item) :into bindings
        :finally (return
                   `(let* ,bindings
                      (macrolet ((define (variable &optional binding)
                                   (declare (ignore variable binding))
                                   (error "Define must come at the beginning of a BEGIN.")))
                        ,@sublist)))))

;;; This exists so tools like SLIME see a define inside of begin even
;;; though begin doesn't use it. Schemes have lexical, not dynamic,
;;; globals so this doesn't behave exactly like the Scheme version. It
;;; also only does variables, not functions.
(defmacro define (variable &optional binding)
  `(defparameter ,variable ,binding))

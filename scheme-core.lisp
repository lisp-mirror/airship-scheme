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

(define-function (scheme-symbol :inline t) ((symbol symbol))
  (intern (invert-case (symbol-name symbol))
          '#:r7rs))

(define-function (nanp :inline t) ((number number))
  (and (floatp number) (float-features:float-nan-p number)))

(define-function (infinitep :inline t) ((number number))
  (and (floatp number) (float-features:float-infinity-p number)))

(define-function (finitep :inline t) ((number number))
  (not (and (floatp number) (or (infinitep number) (nanp number)))))

;;; TODO: fixme: This old function might be defining Scheme exactness
;;; incorrectly.
(defun exactp (number)
  (cond ((rationalp number) t)
        ((complexp number) (and (rationalp (realpart number))
                                (rationalp (imagpart number))))
        (t nil)))

(define-function (scheme-boolean-p :inline t) (object)
  (typep object 'scheme-boolean))

(define-function (scheme-symbol-p :inline t) (object)
  (and object
       (symbolp object)
       (not (scheme-boolean-p object))))

(defun coerce-subseq (sequence result-type &optional start end)
  (let ((subseq (if start
                    (subseq sequence start end)
                    sequence)))
    (coerce subseq result-type)))

(defun copy-seq-or-subseq (sequence &optional start end)
  (if start
      (subseq sequence start end)
      (copy-seq sequence)))

(defun generate-lambda-list (list)
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
  (destructuring-bind (name &rest arguments)
      variable-and-scheme-lambda-list
    (let ((arguments (if (listp arguments)
                         (cdr (generate-lambda-list variable-and-scheme-lambda-list))
                         `(&rest ,arguments)))
          (continuation (gensym #.(symbol-name '#:c))))
      `(define-function ,(intern (symbol-name name) '#:r7rs) ,(list* `(,continuation function) arguments)
         (multiple-value-call ,continuation (progn ,@body))))))

(defmacro define-scheme-predicate ((variable &rest arguments) &body body)
  `(define-scheme-procedure (,variable ,@arguments)
     (nil-to-false (progn ,@body))))

(defmacro define-scheme-cxr ((variable pair))
  `(define-scheme-procedure (,variable ,pair)
     (if (null ,pair)
         (error "Attempted to use a cxr operation on an empty list")
         (,variable ,pair))))

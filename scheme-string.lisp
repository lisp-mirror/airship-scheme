;;;; -*- mode: common-lisp; -*-

;;;; Note: SBCL requires the use of SB-UNICODE for full Unicode
;;;; support for these various functions, but other Unicode-supporting
;;;; implementations just directly support full Unicode in the CL
;;;; package for things like upcasing.

(in-package #:airship-scheme)

(define-function (utf8-to-string :inline t) (octets &key (start 0) end)
  (octets-to-string octets
                    :start start
                    :end end
                    #+sbcl :external-format #-sbcl :encoding :utf-8))

(define-function (string-to-utf8 :inline t) (string &key (start 0) end)
  (string-to-octets string
                    :start start
                    :end end
                    #+sbcl :external-format #-sbcl :encoding :utf-8))

(define-function (digit-value :inline t) (char)
  #+sbcl
  (sb-unicode:numeric-value char)
  #-sbcl
  (digit-char-p char))

(define-function (char-numeric-p :inline t) (char)
  (and (digit-value char) t))

(define-function (char-alphabetic-p :inline t) (char)
  #+sbcl
  (sb-unicode:alphabetic-p char)
  #-sbcl
  (alpha-char-p char))

;;; Note: This might not be correct for all implementations, since as
;;; noted before, a CL implementation can support full Unicode without
;;; requiring calls to a Unicode library. Unfortunately, CL has no
;;; whitespace test built in.
(define-function (char-whitespace-p :inline t) (char)
  #+sbcl
  (and (sb-unicode:whitespace-p char) t)
  #-sbcl
  (or (char= char #\Newline)
      (char= char #\Space)
      (char= char #\Tab)))

(define-function (char-upper-case-p :inline t) (letter)
  #+sbcl
  (sb-unicode:uppercase-p letter)
  #-sbcl
  (upper-case-p letter))

(define-function (char-lower-case-p :inline t) (letter)
  #+sbcl
  (sb-unicode:lowercase-p letter)
  #-sbcl
  (lower-case-p letter))

(define-function (char-upcase* :inline t) ((char character))
  #+sbcl
  (let ((s (make-string 1 :initial-element char)))
    (declare (dynamic-extent s))
    (char (sb-unicode:uppercase s) 0))
  #-sbcl
  (char-upcase char))

(define-function (char-downcase* :inline t) ((char character))
  #+sbcl
  (let ((s (make-string 1 :initial-element char)))
    (declare (dynamic-extent s))
    (char (sb-unicode:lowercase s) 0))
  #-sbcl
  (char-downcase char))

;;; Note: This is another function which might not be correct for all
;;; implementations.
(define-function (char-foldcase :inline t) ((char character))
  #+sbcl
  (let ((s (make-string 1 :initial-element char)))
    (declare (dynamic-extent s))
    (char (sb-unicode:casefold s) 0))
  #-sbcl
  (char-downcase char))

(define-function (string-upcase* :inline t) (string)
  #+sbcl
  (sb-unicode:uppercase string)
  #-sbcl
  (string-upcase string))

(define-function (string-downcase* :inline t) (string)
  #+sbcl
  (sb-unicode:lowercase string)
  #-sbcl
  (string-downcase string))

;;; Note: This might not be correct for all implementations.
(define-function (string-foldcase :inline t) (string)
  #+sbcl
  (sb-unicode:casefold string)
  #-sbcl
  (string-downcase string))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function (compare :inline t) ((function function) (items list))
    "
Defines a short-circuiting predicate on an arbitrary-length list.
"
    (when (endp items)
      (error "Expected at least one item"))
    (loop :for old-item := nil :then item
          :for item :in items
          :for match := t :then (funcall function old-item item)
          :unless match :do (return nil)
          :finally (return t)))
  #+sbcl
  (define-function (compare-foldcase :inline t) ((function function) (strings list))
    "
Defines a short-circuiting string predicate on an arbitrary-length
list of strings, while doing a Unicode foldcase on each string.
"
    (when (endp strings)
      (error "Expected at least one item"))
    (loop :for old-string := nil :then string*
          :for string :in strings
          :for string* := (string-foldcase string)
          :for match := t :then (funcall function old-string string*)
          :unless match :do (return nil)
          :finally (return t))))

(defmacro define-string-predicate ((binary-name n-ary-name) binary-predicate &key foldcase)
  (let ((compare (if foldcase 'compare-foldcase 'compare)))
    `(progn
       (define-function (,binary-name :inline t) (string-1 string-2)
         (,binary-predicate string-1 string-2))
       (define-compiler-macro ,n-ary-name (&whole whole &rest strings)
         (if strings
             (if (endp (cdr strings))
                 ;; TODO: optimize the one-arg version properly
                 whole
                 (if (endp (cddr strings))
                     (list ',binary-name (car strings) (cadr strings))
                     whole))
             whole))
       (define-function ,n-ary-name (&rest strings)
         (,compare (function ,binary-name) strings)))))

(defmacro define-string-predicates (&body predicates)
  `(progn
     ,@(mapcar (lambda (definition)
                 `(define-string-predicate ,@definition))
               predicates)))

#+sbcl
(define-string-predicates
  ((%string=?     string=?)     sb-unicode:unicode=)
  ((%string-ci=?  string-ci=?)  sb-unicode:unicode-equal)
  ((%string<?     string<?)     sb-unicode:unicode<)
  ((%string-ci<?  string-ci<?)  sb-unicode:unicode< :foldcase t)
  ((%string>?     string>?)     sb-unicode:unicode>)
  ((%string-ci>?  string-ci>?)  sb-unicode:unicode> :foldcase t)
  ((%string<=?    string<=?)    sb-unicode:unicode<=)
  ((%string-ci<=? string-ci<=?) sb-unicode:unicode<= :foldcase t)
  ((%string>=?    string>=?)    sb-unicode:unicode>=)
  ((%string-ci>=? string-ci>=?) sb-unicode:unicode>= :foldcase t))

#-sbcl
(define-string-predicates
  ((%string=?     string=?)     string=)
  ((%string-ci=?  string-ci=?)  string-equal)
  ((%string<?     string<?)     string<)
  ((%string-ci<?  string-ci<?)  string-lessp)
  ((%string>?     string>?)     string>)
  ((%string-ci>?  string-ci>?)  string-greaterp)
  ((%string<=?    string<=?)    string<=)
  ((%string-ci<=? string-ci<=?) string-not-greaterp)
  ((%string>=?    string>=?)    string>=)
  ((%string-ci>=? string-ci>=?) string-not-lessp))

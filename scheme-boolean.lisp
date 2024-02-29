;;;; -*- mode: common-lisp; -*-

;;;; A package for #t and #f
;;;;
;;;; Scheme distinguishes between #f and the empty list. This package
;;;; is used to give a unique package that contains the symbol that #f
;;;; will be read as in the Scheme reader, i.e. '%scheme-boolean:f
;;;;
;;;; There should be effectively no performance loss comparing to this
;;;; 'f instead of cl:nil.

(defpackage #:%scheme-boolean
  (:use)
  (:import-from #:cl
                ;; Imported to avoid various problems
                #:defpackage
                #:function
                #:in-package
                #:nil
                #:quote
                ;; Actually used (reexported)
                #:t)
  (:export #:t #:f))

(defconstant %scheme-boolean:f '%scheme-boolean:f)

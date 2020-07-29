;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme)

;;;; Extra procedures
;;;;
;;;; Procedures used by various SRFIs or core libraries, but not in
;;;; R7RS-small.

;;;; SRFI 112

;;;; Note: It might also be useful to expose
;;;; (lisp-implementation-type) and (lisp-implementation-version) so
;;;; that the user knows what the host Common Lisp is.

(define-scheme-procedure (implementation-name)
  "Airship Scheme")

(define-scheme-procedure (implementation-version)
  "0.0.0.0")

(define-scheme-procedure (cpu-architecture)
  (machine-type))

(define-scheme-procedure (machine-name)
  (machine-instance))

(define-scheme-procedure (os-name)
  (software-type))

(define-scheme-procedure (os-version)
  (software-version))

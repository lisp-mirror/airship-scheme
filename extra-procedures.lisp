;;;; -*- mode: common-lisp; -*-

(in-package #:airship-scheme)

;;;; Extra procedures
;;;;
;;;; Procedures used by various SRFIs or core libraries, but not in
;;;; R7RS-small.

;;;; cl-environment

(define-scheme-procedure (internal-time-units-per-second)
  internal-time-units-per-second)

(define-scheme-procedure (internal-real-time)
  (get-internal-real-time))

(define-scheme-procedure (internal-run-time)
  (get-internal-run-time))

(define-scheme-procedure (lisp-implementation-type)
  (lisp-implementation-type))

(define-scheme-procedure (lisp-implementation-version)
  (lisp-implementation-version))

(define-scheme-procedure (user-homedir-pathname)
  (user-homedir-pathname))

;;; SRFI 112

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

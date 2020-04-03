;;;; -*- mode: scheme; -*-

;;; A simple library consisting of the public procedures and macros
;;; for all of the example programs.

(define-library (examples base)
  (import (scheme base)
          (scheme write))
  (export hello)
  (include "hello-world.scm"))

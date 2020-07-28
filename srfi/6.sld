;;;; -*- mode: scheme; -*-

(define-library (srfi 6)
  (functional)
  (import (srfi 172 functional))
  (export get-output-string
          open-input-string
          open-output-string))

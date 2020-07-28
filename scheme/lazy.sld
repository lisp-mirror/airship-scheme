;;;; -*- mode: scheme; -*-

(define-library (scheme lazy)
  (import (airship r7rs))
  (export delay delay-force force make-promise promise?))

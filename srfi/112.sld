;;;; -*- mode: scheme; -*-

(define-library (srfi 112)
  ;; TODO: probably not the final name
  (import (airship extras))
  (export cpu-architecture
          implementation-name
          implementation-version
          machine-name
          os-name
          os-version))

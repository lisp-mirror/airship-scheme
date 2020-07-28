;;;; -*- mode: scheme; -*-

(define-library (scheme process-context)
  (import (airship r7rs))
  (export command-line emergency-exit exit get-environment-variable
          get-environment-variables))

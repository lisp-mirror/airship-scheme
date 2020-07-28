;;;; -*- mode: scheme; -*-

(define-library (scheme file)
  (import (airship r7rs))
  (export call-with-input-file call-with-output-file delete-file file-exists?
          open-binary-input-file open-binary-output-file open-input-file
          open-output-file with-input-from-file with-output-to-file))

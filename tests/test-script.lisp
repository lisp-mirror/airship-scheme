(defpackage #:test-script
  (:use #:cl))

(in-package #:test-script)

;;; Load the test suite and UIOP at read time.
#.(progn (ql:quickload :fiveam :silent t)
         (ql:quickload :uiop :silent t)
         nil)

;;; Turn on full type inference to catch more type errors when
;;; compiling.
#+sbcl
(setf sb-ext:*derive-function-types* t)

;;; Load the dependencies first because if they have warnings, there's
;;; nothing we can do about that.
(dolist (system (asdf:system-depends-on (asdf:find-system :airship-scheme)))
  (ql:quickload system :silent t))

;;; The first "test" is to compile with no warnings.
(let ((asdf:*compile-file-warnings-behaviour* :error))
  (ql:quickload :airship-scheme :verbose t)
  (ql:quickload :airship-scheme/tests))

;;; Run the tests.
;;;
;;; Ideally, we could just work with the final return value of the
;;; fiveam tests from asdf:test-system, but asdf:test-system always
;;; returns t, so fiveam has to be used directly.
(defun run-tests ()
  (fiveam:run! 'airship-scheme/tests:airship-scheme/tests))

;;; This lets Gitlab CI know that something went wrong.
(unless (run-tests)
  (uiop:quit 1))

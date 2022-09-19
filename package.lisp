;;;; -*- mode: common-lisp; -*-

(cl:defpackage #:airship-scheme
  (:use #:cl
        #:zr-utils)
  ;; Uses a fast, implementation-specific version if available and
  ;; otherwise uses the slow, portable version
  (:export #:read-scheme)
  (:local-nicknames (:a :alexandria)
                    (:f :float-features)))

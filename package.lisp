;;;; -*- mode: common-lisp; -*-

(cl:defpackage #:airship-scheme
  (:use #:cl
        #:zr-utils)
  ;; Uses a fast, implementation-specific version if available and
  ;; otherwise uses the slow, portable version
  (:import-from #-sbcl #:babel #+sbcl #:sb-ext
                #:string-to-octets
                #:octets-to-string)
  (:export #:scheme-read)
  (:local-nicknames (:a :alexandria)
                    (:f :float-features)))

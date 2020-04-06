;;;; -*- mode: common-lisp; -*-

(cl:defpackage #:airship-scheme
  (:use #:cl
        #:zr-utils)
  (:import-from #:alexandria
                #:proper-list-p)
  (:import-from #:babel
                #:octets-to-string
                #:string-to-octets)
  (:import-from #:float-features)
  (:export #:scheme-read))

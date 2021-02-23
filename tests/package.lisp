;;;; -*- mode: common-lisp; -*-

(cl:defpackage #:airship-scheme/tests
  (:use #:airship-scheme
        #:cl)
  (:import-from #:5am
                #:is)
  (:local-nicknames (:scheme :airship-scheme)
                    (:f :float-features)))

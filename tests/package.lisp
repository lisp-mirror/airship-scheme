;;;; -*- mode: common-lisp; -*-

(defpackage #:airship-scheme/tests
  (:use #:airship-scheme
        #:cl)
  (:import-from #:5am
                #:is)
  (:export #:airship-scheme/tests)
  (:local-nicknames (:scheme :airship-scheme)
                    (:f :float-features)))

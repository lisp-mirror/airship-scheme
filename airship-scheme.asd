;;;; -*- mode: common-lisp; -*-

(in-package #:cl)

(asdf:defsystem #:airship-scheme
  :serial t
  :description "A new r7rs Scheme implementation, designed to run within a Common Lisp environment."
  :version "0.0.0"
  :author "Michael Babich"
  :maintainer "Michael Babich"
  :license "MIT"
  :homepage "https://gitlab.com/mbabich/airship-scheme"
  :bug-tracker "https://gitlab.com/mbabich/airship-scheme/issues"
  :source-control (:git "https://gitlab.com/mbabich/airship-scheme.git")
  :depends-on (:alexandria
               :float-features
               :trivial-features
               :zr-utils)
  :components ((:file "package")
               (:file "scheme-boolean")
               (:file "util")
               (:file "scheme-core")
               (:file "scheme-types")
               (:file "scheme-string")
               (:file "equality")
               (:file "scheme-write")
               (:file "scheme-read")
               (:file "standard-procedures")
               (:file "extra-procedures")
               (:file "cl-macros"))
  :in-order-to ((asdf:test-op (asdf:test-op "airship-scheme/tests"))))

(asdf:defsystem #:airship-scheme/tests
  :serial t
  :description "The tests for the Common Lisp side of Airship Scheme."
  :version "0.0.0"
  :author "Michael Babich"
  :maintainer "Michael Babich"
  :license "MIT"
  :depends-on (:airship-scheme
               :fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (o s) (uiop:symbol-call :fiveam
                                                 :run!
                                                 (cl:intern (cl:symbol-name '#:airship-scheme/tests)
                                                            '#:airship-scheme/tests))))

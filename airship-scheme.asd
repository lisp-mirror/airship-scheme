;;;; -*- mode: common-lisp; -*-

(in-package #:cl)

(asdf:defsystem #:airship-scheme
  :serial t
  :description "A new r7rs Scheme implementation, designed to interoperate with a Common Lisp environment."
  :version "0.0.0.0"
  :author "Michael Babich"
  :maintainer "Michael Babich"
  :license "MIT"
  :homepage "https://gitlab.com/mbabich/airship-scheme"
  :bug-tracker "https://gitlab.com/mbabich/airship-scheme/issues"
  :source-control (:git "https://gitlab.com/mbabich/airship-scheme.git")
  :depends-on (:alexandria
               #-sbcl
               :babel
               :float-features
               :trivial-features
               :zr-utils)
  :components ((:file "package")
               (:file "scheme-boolean")
               (:file "scheme-core")
               (:file "scheme-types")
               (:file "scheme-string")
               (:file "equality")
               (:file "scheme-write")
               (:file "scheme-read")
               (:file "standard-procedures")
               (:file "extra-procedures")))

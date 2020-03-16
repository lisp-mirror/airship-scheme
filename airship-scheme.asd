(in-package #:cl)

(asdf:defsystem #:airship-scheme
  :serial t
  :description "A new r7rs Scheme implementation, designed to be run within a Common Lisp environment."
  :version "0.0.0.0"
  :author "Michael Babich"
  :maintainer "Michael Babich"
  :license "MIT"
  :homepage "https://gitlab.com/mbabich/airship-scheme"
  :bug-tracker "https://gitlab.com/mbabich/airship-scheme/issues"
  :source-control (:git "https://gitlab.com/mbabich/airship-scheme.git")
  :depends-on (:alexandria
               :float-features
               ;; Note: Temporary dependency until the utils are spun out of the game engine repository
               :zombie-raptor/util)
  :components ((:file "package")
               (:file "scheme-boolean")
               (:file "scheme-read")))

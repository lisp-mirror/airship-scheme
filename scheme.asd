(in-package #:cl)

;; TODO: Temporary name
(asdf:defsystem #:scheme
  :serial t
  :description "A new r7rs Scheme implementation, designed to be run within a Common Lisp environment."
  :author "Michael Babich"
  :license "MIT"
  :depends-on (:alexandria
               :float-features
               ;; Note: Temporary dependency until the utils are spun out of the game engine repository
               :zombie-raptor)
  :components ((:file "package")
               (:file "scheme-read")))

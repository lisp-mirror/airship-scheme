;;;; -*- mode: common-lisp; -*-

;;;; Non-string equality testing

(in-package #:airship-scheme)

(define-function (%symbol= :inline t :check-type t) ((symbol-1 symbol) (symbol-2 symbol))
  (eq symbol-1 symbol-2))

(defun symbol= (&rest symbols)
  "Tests if one or more symbols are equal to each other"
  (compare #'%symbol= symbols))

(defun eqv? (x y)
  "
Tests if two objects are Scheme-equivalent to each other, using the
rules provided in the r7rs-small specification.
"
  (typecase x
    (symbol (typecase y (symbol (%symbol= x y))))
    (inexact? (typecase y (inexact? (= x y))))
    (exact? (typecase y (exact? (= x y))))
    (character (typecase y (character (char= x y))))
    (t (eq x y))))

;;; TODO: If circular and equal?, then this iterates too much because
;;; it goes to the first detected part of the cycle rather than to the
;;; start of it. It can't just stop at the detection of the cycle
;;; because of e.g. '(-1 0 . #1=(1 2 3 4 5 6 7 8 9 . #1#))
(define-function list-equal? ((list1 list) (list2 list))
  ;; Note: Tested in a more verbose way so that the list lengths match
  ;; in the ALWAYS test and so lists with cycles always terminate.
  (loop :with end? := nil
        :with cycle-x := nil
        :with cycle-y := nil
        :for x := list1 :then (cdr x)
        :for y := list2 :then (cdr y)
        ;; For cycle testing to ensure termination
        :for x-fast := list1 :then (cddr x-fast)
        :for y-fast := list2 :then (cddr y-fast)
        :for i :from 0
        :until end?
        ;; Recursive equality test
        :always (or (and (endp x) (endp y))
                    (equal? (car x) (car y)))
        :do
           ;; End test
           (when (or (endp x) (endp y) (eq x cycle-x) (eq y cycle-y))
             (setf end? t))
           ;; Cycle tests
           (when (plusp i)
             (when (and x-fast (not cycle-x) (eq x x-fast))
               (setf cycle-x x))
             (when (and y-fast (not cycle-y) (eq y y-fast))
               (setf cycle-y y)))))

(defun vector-equal? (x y)
  (and (typep y (type-of x))
       (= (length x) (length y))
       (loop :for a :across x
             :for b :across y
             :always (equal? a b))))

;;; TODO: use a sequence-generic comparison when extensible-sequences is used
(defun equal? (x y)
  (typecase x
    (list (and (listp y) (list-equal? x y)))
    (vector (and (vectorp y) (vector-equal? x y)))
    (t (eqv? x y))))

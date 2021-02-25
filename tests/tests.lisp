;;;; -*- mode: common-lisp; -*-

(cl:in-package #:airship-scheme/tests)

(defun read-scheme* (string)
  (with-input-from-string (stream string)
    (scheme::read-scheme stream)))

(5am:def-suite airship-scheme/tests)

(5am:def-suite airship-scheme/scheme-read
  :in airship-scheme/tests)

(5am:in-suite airship-scheme/scheme-read)

(5am:test boolean
  "Are true and false read correctly?"
  (is (eq (read-scheme* "#t") t))
  (is (eq (read-scheme* "#f") %scheme-boolean:f)))

;;; Note: This should match the list of numbers in examples/syntax.scm
(5am:test numeric-syntax
  "Are numbers read correctly?"
  (is (eql (read-scheme* "1")
           1))
  (is (eql (read-scheme* "+1")
           1))
  (is (eql (read-scheme* "-1")
           -1))
  (is (eql (read-scheme* "1.0")
           1.0d0))
  (is (eql (read-scheme* "4.")
           4.0d0))
  (is (eql (read-scheme* ".4")
           0.4d0))
  (is (eql (read-scheme* "-.4")
           -0.4d0))
  (is (eql (read-scheme* "+.4")
           0.4d0))
  (is (eql (read-scheme* "1.0f0")
           1.0f0))
  (is (eql (read-scheme* "-1.2s3")
           -1.2s3))
  (is (eql (read-scheme* "1.0l-1")
           1.0l-1))
  (is (eql (read-scheme* "3d1")
           3d1))
  (is (eql (read-scheme* "87e2")
           87d2))
  (is (eql (read-scheme* "4/3")
           4/3))
  (is (eql (read-scheme* "-123/456")
           -123/456))
  (is (eql (read-scheme* "-3/2")
           -3/2))
  (is (eql (read-scheme* "+inf.0")
           f:double-float-positive-infinity))
  (is (eql (read-scheme* "-inf.0")
           f:double-float-negative-infinity))
  (let ((nan (read-scheme* "+nan.0")))
    (is (and (scheme::nanp nan)
             (typep nan 'double-float)
             (not (scheme::sign-bit? nan)))))
  (let ((nan (read-scheme* "-nan.0")))
    (is (and (scheme::nanp nan)
             (typep nan 'double-float)
             (scheme::sign-bit? nan))))
  (is (eql (read-scheme* "+inf.0f0")
           f:single-float-positive-infinity))
  (is (eql (read-scheme* "-inf.0f0")
           f:single-float-negative-infinity))
  (let ((nan (read-scheme* "+nan.0f0")))
    (is (and (scheme::nanp nan)
             (typep nan 'single-float)
             (not (scheme::sign-bit? nan)))))
  (let ((nan (read-scheme* "-nan.0f0")))
    (is (and (scheme::nanp nan)
             (typep nan 'single-float)
             (scheme::sign-bit? nan))))
  (is (eql (read-scheme* "-4i")
           #C(0 -4)))
  (is (eql (read-scheme* "+3i")
           #C(0 3)))
  (is (eql (read-scheme* "7i")
           #C(0 7)))
  (is (eql (read-scheme* "4-i")
           #C(4 -1)))
  (is (eql (read-scheme* "9+i")
           #C(9 1)))
  (is (eql (read-scheme* "+inf.0+inf.0i")
           (complex f:double-float-positive-infinity
                    f:double-float-positive-infinity)))
  (let ((z (read-scheme* "+inf.0-nan.0i")))
    (is (eql (realpart z)
             f:double-float-positive-infinity))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'double-float)
               (scheme::sign-bit? i)))))
  (let ((z (read-scheme* "+nan.0+inf.0i")))
    (is (eql (imagpart z)
             f:double-float-positive-infinity))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'double-float)
               (not (scheme::sign-bit? x))))))
  (is (eql (read-scheme* "4-inf.0i")
           (complex 4.0d0
                    f:double-float-negative-infinity)))
  (let ((z (read-scheme* "-12+nan.0f0i")))
    (is (eql (realpart z)
             -12.0f0))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'single-float)
               (not (scheme::sign-bit? i))))))
  (is (eql (read-scheme* "+inf.0-3i")
           (complex f:double-float-positive-infinity
                    -3.0d0)))
  (let ((z (read-scheme* "-nan.0+42i")))
    (is (eql (imagpart z)
             42.0d0))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'double-float)
               (scheme::sign-bit? x)))))
  (let ((z (read-scheme* "-nan.0f0-333i")))
    (is (eql (imagpart z)
             -333.0f0))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'single-float)
               (scheme::sign-bit? x)))))
  (let ((z (read-scheme* "-nan.0f0+inf.0f0i")))
    (is (eql (imagpart z)
             f:single-float-positive-infinity))
    (let ((x (realpart z)))
      (is (and (scheme::nanp x)
               (typep x 'single-float)
               (scheme::sign-bit? x)))))
  (is (eql (read-scheme* "+inf.0f0+22i")
           (complex f:single-float-positive-infinity
                    22f0)))
  (let ((z (read-scheme* "+inf.0f0-nan.0f0i")))
    (is (eql (realpart z)
             f:single-float-positive-infinity))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'single-float)
               (scheme::sign-bit? i)))))
  (let ((z (read-scheme* "333+nan.0f0i")))
    (is (eql (realpart z)
             333.0f0))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'single-float)
               (not (scheme::sign-bit? i))))))
  (is (eql (read-scheme* "+inf.0i")
           (complex 0d0
                    f:double-float-positive-infinity)))
  (is (eql (read-scheme* "-inf.0f0i")
           (complex 0f0
                    f:single-float-negative-infinity)))
  (let ((z (read-scheme* "-nan.0i")))
    (is (eql (realpart z) 0d0))
    (let ((i (imagpart z)))
      (is (and (scheme::nanp i)
               (typep i 'double-float)
               (scheme::sign-bit? i)))))
  (let ((z (read-scheme* "+nan.0d0i")))
    (is (eql (realpart z) 0d0)
        (let ((i (imagpart z)))
          (is (and (scheme::nanp i)
                   (typep i 'double-float)
                   (not (scheme::sign-bit? i)))))))
  (is (eql (read-scheme* "4/3-3/4i")
           #C(4/3 -3/4)))
  (is (eql (read-scheme* "3e4-4e4i")
           #C(3d4 -4d4)))
  (is (eql (read-scheme* "-4.0f30+3.0f20i")
           #C(-4.0f30 3.0f20)))
  (is (eql (read-scheme* "-1+4i")
           #C(-1 4)))
  (is (eql (read-scheme* "+4-3i")
           #C(4 -3)))
  (is (eql (read-scheme* "3+2i")
           #C(3 2)))
  (let* ((z (read-scheme* "4@5"))
         (z* (* 4d0 (cis 5d0)))
         (z-z* (- z z*)))
    (is (and (typep z '(complex double-float))
             (< (abs (realpart z-z*)) double-float-epsilon)
             (< (abs (imagpart z-z*)) double-float-epsilon))))
  (let* ((z (read-scheme* "-3.0@+4e3"))
         (z* (* -3d0 (cis 4d3)))
         (z-z* (- z z*)))
    (is (and (typep z '(complex double-float))
             (< (abs (realpart z-z*)) double-float-epsilon)
             (< (abs (imagpart z-z*)) double-float-epsilon))))
  (let* ((z (read-scheme* "-321.0f-3@+432f12"))
         (z* (* -321.0f-3 (cis 432f12)))
         (z-z* (- z z*)))
    (is (and (typep z '(complex single-float))
             (< (abs (realpart z-z*)) single-float-epsilon)
             (< (abs (imagpart z-z*)) single-float-epsilon))))
  (let ((z (read-scheme* "4@+inf.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "-7@-nan.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (eql (read-scheme* "+inf.0f0@111")
       (complex f:single-float-negative-infinity
                f:single-float-negative-infinity))
  (eql (read-scheme* "+inf.0@-3")
       (complex f:double-float-negative-infinity
                f:double-float-negative-infinity))
  (let ((z (read-scheme* "+nan.0@42")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "+inf.0@-inf.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "-nan.0@+nan.0")))
    (is (and (typep z '(complex double-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z)))))
  (let ((z (read-scheme* "+nan.0f0@-nan.0f0")))
    (is (and (typep z '(complex single-float))
             (scheme::nanp (realpart z))
             (scheme::nanp (imagpart z))))))

(5am:test bases-and-exactness
  "Are numbers in different bases and exactness read correctly?"
  (is (eql (read-scheme* "#x42") 66))
  (is (eql (read-scheme* "#xFFF") 4095))
  (is (eql (read-scheme* "#Xfff") 4095))
  (is (eql (read-scheme* "#XffF") 4095))
  (is (eql (read-scheme* "#b101") 5))
  (is (eql (read-scheme* "#o777") 511))
  (is (eql (read-scheme* "#d1999") 1999))
  (is (eql (read-scheme* "#xabcdef") 11259375))
  (is (eql (read-scheme* "#i33") 33.0d0))
  (is (eql (read-scheme* "#e876") 876))
  (is (eql (read-scheme* "#e876.0") 876))
  (is (eql (read-scheme* "#e32.1") 321/10))
  (is (eql (read-scheme* "#x#iee") 238.0d0))
  (is (eql (read-scheme* "#i#xcd") 205.0d0))
  (is (eql (read-scheme* "#e#x1a") 26))
  (is (eql (read-scheme* "#x#e93fc3a") 9698362))
  (is (eql (read-scheme* "#i#o4321") 2257.0d0))
  (is (eql (read-scheme* "#b#i1110101010100001") 60065.0d0)))
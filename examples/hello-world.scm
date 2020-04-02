;;; A simple hello world of arbitrary length implemented via tail
;;; recursion. hello* is the helper procedure to hello. The main
;;; procedure is hello, which should be called with 0 or more
;;; arguments.
;;;
;;; This demonstrates several things, including tail recursion and the
;;; use of \n in strings.

(define (hello* names)
  (display (car names))
  (if (null? (cdr names))
      (display "!\n")
      (begin
        (display ", ")
        (hello* (cdr names)))))

(define (hello . names)
  (if (null? names)
      (display "Hello world!\n")
      (begin
        (display "Hello ")
        (hello* names))))

;;;; R7RS Standard Libraries

(define-library (scheme base)
  (export * + - ... / < <= = => > >= _ abs and append apply assoc assq assv
          begin binary-port? boolean=? boolean? bytevector bytevector-append
          bytevector-copy bytevector-copy! bytevector-length bytevector-u8-ref
          bytevector-u8-set! bytevector? caar cadr
          call-with-current-continuation call-with-port call-with-values call/cc
          car case cdar cddr cdr ceiling char->integer char-ready? char<=?
          char<? char=? char>=? char>? char? close-input-port close-output-port
          close-port complex? cond cond-expand cons current-error-port
          current-input-port current-output-port define define-record-type
          define-syntax define-values denominator do dynamic-wind else
          eof-object eof-object? eq? equal? eqv? error error-object-irritants
          error-object-message error-object? even? exact exact-integer-sqrt
          exact-integer? exact? expt features file-error? floor floor-quotient
          floor-remainder floor/ flush-output-port for-each gcd
          get-output-bytevector get-output-string guard if include include-ci
          inexact inexact? input-port-open? input-port? integer->char integer?
          lambda lcm length let let* let*-values let-syntax let-values
          letrec letrec* letrec-syntax list list->string list->vector list-copy
          list-ref list-set! list-tail list? make-bytevector make-list
          make-parameter make-string make-vector map max member memq memv min
          modulo negative? newline not null? number->string number? numerator
          odd? open-input-bytevector open-input-string open-output-bytevector
          open-output-string or output-port-open? output-port? pair?
          parameterize peek-char peek-u8 port? positive? procedure? quasiquote
          quote quotient raise raise-continuable rational? rationalize
          read-bytevector read-bytevector! read-char read-error? read-line
          read-string read-u8 real? remainder reverse round set! set-car!
          set-cdr! square string string->list string->number string->symbol
          string->utf8 string->vector string-append string-copy string-copy!
          string-fill! string-for-each string-length string-map string-ref
          string-set! string<=? string<? string=? string>=? string>? string?
          substring symbol->string symbol=? symbol? syntax-error syntax-rules
          textual-port? truncate truncate-quotient truncate-remainder truncate/
          u8-ready? unless unquote unquote-splicing utf8->string values vector
          vector->list vector->string vector-append vector-copy vector-copy!
          vector-fill! vector-for-each vector-length vector-map vector-ref
          vector-set! vector? when with-exception-handler write-bytevector
          write-char write-string write-u8 zero?)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme case-lambda)
  (export case-lambda)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme char)
  (export char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
          char-downcase char-foldcase char-lower-case? char-numeric? char-upcase
          char-upper-case? char-whitespace? digit-value string-ci<=? string-ci<?
          string-ci=? string-ci>=? string-ci>? string-downcase string-foldcase
          string-upcase)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme complex)
  (export angle imag-part magnitude make-polar make-rectangular real-part)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme cxr)
  (export caaaar caaadr caaar caadar caaddr caadr cadaar cadadr cadar caddar
          cadddr caddr cdaaar cdaadr cdaar cdadar cdaddr cdadr cddaar cddadr
          cddar cdddar cddddr cdddr)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme eval)
  (export environment eval)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme file)
  (export call-with-input-file call-with-output-file delete-file file-exists?
          open-binary-input-file open-binary-output-file open-input-file
          open-output-file with-input-from-file with-output-to-file)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme inexact)
  (export (acos asin atan cos exp finite? infinite? log nan? sin sqrt tan))
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme lazy)
  (export delay delay-force force make-promise promise?)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme load)
  (export load)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme process-context)
  (export command-line emergency-exit exit get-environment-variable
          get-environment-variables)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme read)
  (export read)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme repl)
  (export interaction-environment)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme time)
  (export current-jiffy current-second jiffies-per-second)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme write)
  (export display write write-shared write-simple)
  (include-lisp "standard-procedures.lisp"))

(define-library (scheme r5rs)
  (export * + - / < <= = > >= abs acos and angle append apply asin assoc assq
          assv atan begin boolean? caaaar caaadr caaar caadar caaddr caadr caar
          cadaar cadadr cadar caddar cadddr caddr cadr
          call-with-current-continuation call-with-input-file
          call-with-output-file call-with-values car case cdaaar cdaadr cdaar
          cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
          cdr ceiling char->integer char-alphabetic? char-ci<=? char-ci<?
          char-ci=? char-ci>=? char-ci>? char-downcase char-lower-case?
          char-numeric? char-ready? char-upcase char-upper-case?
          char-whitespace? char<=? char<? char=? char>=? char>? char?
          close-input-port close-output-port complex? cond cons cos
          current-input-port current-output-port define define-syntax delay
          denominator display do dynamic-wind eof-object? eq? equal? eqv? eval
          even? exact->inexact exact? exp expt floor for-each force gcd if
          imag-part inexact->exact inexact? input-port? integer->char integer?
          interaction-environment lambda lcm length let let* let-syntax letrec
          letrec-syntax list list->string list->vector list-ref list-tail list?
          load log magnitude make-polar make-rectangular make-string make-vector
          map max member memq memv min modulo negative? newline not
          null-environment null? number->string number? numerator odd?
          open-input-file open-output-file or output-port? pair? peek-char
          positive? procedure? quasiquote quote quotient rational? rationalize
          read read-char real-part real? remainder reverse round
          scheme-report-environment set! set-car! set-cdr! sin sqrt string
          string->list string->number string->symbol string-append string-ci<=?
          string-ci<? string-ci=? string-ci>=? string-ci>? string-copy
          string-fill! string-length string-ref string-set! string<=? string<?
          string=? string>=? string>? string? substring symbol->string symbol?
          tan truncate values vector vector->list vector-fill! vector-length
          vector-ref vector-set! vector? with-input-from-file
          with-output-to-file write write-char zero?)
  (include-lisp "standard-procedures.lisp"))

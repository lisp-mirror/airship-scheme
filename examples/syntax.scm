;;;; -*- mode: scheme; -*-

;;;; The first line is the Emacs mode line. It tells Emacs and other
;;;; tools that this file is a Scheme file. Tools should already know
;;;; that .scm files are Scheme, but it's currently needed for .sld
;;;; (R7RS library) files.

;;;; This file demonstrates the syntax of Airship Scheme, which is a
;;;; slightly extended R7RS syntax. This file has many more comments
;;;; than a typical file should have.
;;;;
;;;; Line comments start with at least one ';'. Comments on their own
;;;; line will start with ';;;;' or ';;;' if they're top
;;;; level. Generally, ';;;' comments are about the form immediately
;;;; following the comment.

;;;; Defining globals

;;;; First, let's see the global variable and global procedure
;;;; definitions.

;;; This is a global variable. It uses the `define` macro.
(define a-global-variable 42)

;;; Scheme is a Lisp-1, meaning procedures (functions) and variables
;;; share the same namespace. This means that the long form of a
;;; function's definition is just a `define` followed by a `lambda`.
;;;
;;; Schemes tend to prefer the word "procedure" instead of the word
;;; "function" because it reserves the term for pure/mathematical
;;; functions, seeing the typical programming "function" that could
;;; have side effects as just a "procedure".
;;;
;;; This will also demonstrate the two other kinds of semicolon
;;; comments. Note that these are all conventions. The language itself
;;; only enforces that everything after a semicolon until the next
;;; newline is a comment.
(define a-long-form-procedure
  ;; This comment is internal to a procedure. It only has two
  ;; semicolons and is meant to describe the following form.
  (lambda (x) ; This comment applies to the current line.
    (* x 42)))

;;; Scheme allows, as syntactic sugar, a slight modification of the
;;; `define` syntax to avoid the unnecessary `lambda`.
;;;
;;; Unlike most Lisps, the name of the procedure is part of the list,
;;; not just its arguments. This is how Scheme can tell the difference
;;; between a simple variable definition or the short form of a
;;; procedure definition even though both use `define`.
;;;
;;; Besides the name, this should be equivalent to the previous
;;; procedure definition.
(define (a-short-form-procedure x)
  (* x 42))

;;;; Other comments

;;;; We know about the semicolon, but R7RS supports a few other ways
;;;; to comment things out.

;;; The first define should not be read, but the second should be
;;; read. This is because #; will skip the next non-#; form.
#; (define foobar 42) (define foobar 43)

;;; Note that #; #; shouldn't apply the first #; to skip the second
;;; #;. Instead, #; #; should skip two forms and evaluate the third.
;;;
;;; It's unlikely that your syntax highlighting can handle this.
#; #; (define barfoo 42) (define barfoo 43) (define barfoo 44)

;;; There's one other way to comment in R7RS, which is the block
;;; comment syntax, which works like this:
#| This isn't valid Scheme syntax but it shouldn't matter because it's
in a comment. Note that this comment form also supports more than one
line. A proper Scheme style prefers using the semicolon comments,
though. |#

;;; This can be nested. The first |# shouldn't end the block
;;; comment. Here's an example of that:
#| #| (display "Hello, world!\n") |# (display "Hello world\n") |#

;;;; Numeric syntax

;;; The following list is an example of most of the possible ways to
;;; write a number. Most of the syntactic complexity comes from
;;; complex numbers and the floating point infinities/NaNs. This
;;; latter type of syntax is called "infnan" in the R7RS-small
;;; specification.
;;;
;;; Airship Scheme uses the optional, CL-style specifiers for the
;;; different floating point types: short-float (s), single-float (f),
;;; double-float (d), and long-float (l). Airship Scheme uses the same
;;; floating point types as the host CL. Virtually every CL will have
;;; distinct single and double float types, and some will have long
;;; floats, but only a few will have short floats.
;;;
;;; Unlike in some programming languages, the exponent is required,
;;; e.g. "1f0". On the other hand, "1f" is invalid.
;;;
;;; CL defaults to single-float when no float type is specified, but
;;; the default in Scheme is double-float so a number like 4e3 or 5.0
;;; is a double-float, not a single-float.
;;;
;;; The standard is silent on how to extend the "infnan" syntax to
;;; these different floating point types. Racket uses syntax like
;;; "+inf.f" for the single-float infnan syntax, but this looks ugly
;;; and doesn't even look like a number. It also prevents extensions
;;; to the infnan syntax that permit nonzero integers after the dot.
;;; Airship Scheme uses "s0", "f0", "d0", and "l0" because this is the
;;; main way to turn regular numbers into the respective float type.
;;; The "0" means to multiply by 10^0, i.e. 1.
;;;
;;; In Airship Scheme, anything that starts in "+" or "-" followed by
;;; "inf." or "nan." is either a number or an error; it cannot be a
;;; symbol. These syntax errors reserve the infnan syntax for future
;;; extensions to the language without breaking existing code.
;;;
;;; Besides the addition of the literal infnan, the main difference
;;; here between Scheme and Common Lisp is the way complex numbers are
;;; written. In Common Lisp, a complex number is written in the style
;;; of "#C(2 3)", while the Scheme style is "2+3i". This makes the
;;; Scheme numeric syntax considerably more complicated. Scheme also
;;; supports polar notation, e.g. "-3@4".
;;;
;;; Also note that in CL, "4." becomes the integer "4" while in
;;; Scheme, "4." becomes the flonum "4.0".
;;;
;;; In a strict vanilla Scheme (as specified by the grammar in the
;;; R7RS-small appendix), writing the imaginary number 7i as "7i" is
;;; invalid; instead, it must be written as "+7i". However, there is
;;; no point to make that into invalid syntax in Airship Scheme
;;; because, unlike in CL, anything that starts with a number has to
;;; either be a number or invalid. That is, in Scheme, symbols
;;; starting with a number must be quoted with ||s, which means that
;;; "7i" can't be read as a symbol. It's actually more work to make
;;; "7i" an error instead of an imaginary number so it's easier to
;;; make yet another syntax extension to R7RS-small. Chibi Scheme, the
;;; first conforming R7RS-small implementation, also has this syntax
;;; extension.
(define numbers
  (list 1
        +1
        -1
        1.0
        4.
        .4
        -.4
        +.4
        1.0f0
        -1.2s3
        1.0l-1
        3d1
        87e2
        4/3
        123/-456
        -3/2
        +inf.0
        -inf.0
        +nan.0
        -nan.0
        +inf.0f0
        -inf.0f0
        +nan.0f0
        -nan.0f0
        -4i
        +3i
        7i
        4-i
        9+i
        +inf.0+inf.0i
        +inf.0-nan.0i
        +nan.0+inf.0i
        4-inf.0i
        -12+nan.0f0i
        +inf.0-3i
        -nan.0+42i
        -nan.0f0-333i
        -nan.0f0+inf.0f0i
        +inf.0f0+22i
        +inf.0f0-nan.0f0i
        333+nan.0f0i
        +inf.0i
        -inf.0f0i
        -nan.0i
        +nan.0d0i
        4/3-3/4i
        3e4-4e4i
        -4.0f30+3.0f20i
        -1+4i
        +4-3i
        3+2i
        4@5
        -3.0@+4e3
        -321.0f-3@+432f12
        4@+inf.0
        -7@-nan.0
        +inf.0f0@111
        +inf.0@-3
        +nan.0@42
        +inf.0@-inf.0
        -nan.0@+nan.0
        +nan.0f0@-nan.0f0))

;;;; TODO: The rest of the syntax will be demonstrated here, but this
;;;; file is currently incomplete.

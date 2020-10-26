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

;;;; TODO: Not every kind of literal number is included here yet.

(define numbers
  (list 1
        -1
        1.0
        4.
        .4
        1.0f0
        -1.2s3
        1.0l-1
        3d1
        87e2
        4/3
        -3/2
        +inf.0
        -inf.0
        +nan.0
        -nan.0
        -4i
        +3i))

;;;; TODO: The rest of the syntax will be demonstrated here, but this
;;;; file is currently incomplete.

# Changelog

This changelog uses the style from [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
 - Common Lisp implementations of the Scheme character and string
   functionality, including a wrapper over `sb-unicode`, the Unicode
   functionality of SBCL, when present.
 - Common Lisp implementations of the main Scheme equality predicates.
 - A separate false value from nil, which is not false in Scheme.
 - A `define-scheme-type` macro family, which partially `Lisp-1`ifies
   the separate type namespace by creating a predicate for each type
   that is defined, using the predicate's name as the type name, e.g.
   `number?`.
 - A custom reader for the [R7RS-small] version of Scheme.
 - The number portion of the Scheme writer, as `write-scheme-number`,
   for `number->string`.
 - An internal representation for Scheme that supports tail recursion
   and continuations.
 - Macros for writing parts of this Scheme from Common Lisp, as
   `define-scheme-procedure` (`nil` is the empty list) and
   `define-scheme-predicate` (`nil` is turned into `#f`).
 - A hello world example file and an example library that exports that
   hello world.
 - A substantial number of standard procedures from [R7RS-small]:
   - Almost all of sections 6.1 through 6.9, only excluding
     `rationalize` and `string->number`.
   - Some standard procedures from later sections, especially basic
     ports (streams in CL) from section 6.13.
 - Some Scheme libraries:
   - Library definitions for the `(scheme)` libraries described in
     [R7RS-small] Appendix A.
   - Some SRFIs entirely contained within R7RS-small: 6, 9, 87, and 98.
   - [SRFI 112], except for the library definition itself.
   - Subsets of the [R7RS-small] libraries, specified as [SRFI 172].

[R7RS-small]: https://small.r7rs.org/attachment/r7rs.pdf
[SRFI 112]: https://srfi.schemers.org/srfi-112/srfi-112.html
[SRFI 172]: https://srfi.schemers.org/srfi-172/srfi-172.html
[Unreleased]: https://gitlab.com/mbabich/airship-scheme/-/compare/bd61fb8f...master

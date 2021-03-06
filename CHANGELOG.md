# Changelog

This changelog uses the style from [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
 - Common Lisp implementations of the Scheme character and string
   functionality, including a wrapper over `sb-unicode`, the Unicode
   functionality of SBCL, when present.
 - Common Lisp implementations of the main Scheme equality predicates.
 - A separate false value from `nil`, which is not false in Scheme.
 - A `define-scheme-type` macro family, which partially `Lisp-1`ifies
   the separate type namespace by creating a predicate for each type
   that is defined, using the predicate's name as the type name, e.g.
   `number?`.
   - Some efficient, custom types that match CL's type system,
     including `%list?` for lists that aren't necessarily proper
     lists.
 - A custom reader for the [R7RS-small] version of Scheme.
   - A CL-style syntax extension for specifying short, single, double,
     and long floats, which is permitted by the R7RS standard.
   - A few other minor syntax extensions, particularly around numbers,
     to permit some commonly allowed syntax that is technically not
     fully portable. For instance, both `2i` and `+2i` are read as
     `0+2i`, but `2i` is technically an extension to the syntax.
   - Many potential numbers become symbols, especially starting with
     `+` and `-`. This is necessary to interoperate with CL `+foo+`
     style constants.
 - A custom writer that displays s-expressions in Scheme syntax
   instead of Common Lisp syntax.
 - An internal representation for Scheme that supports tail recursion
   and continuations.
 - Macros for writing parts of this Scheme from Common Lisp, as
   `define-scheme-procedure` (`nil` is the empty list) and
   `define-scheme-predicate` (`nil` is turned into `#f`).
 - A hello world example file and an example library that exports that
   hello world.
 - A substantial number of standard procedures from [R7RS-small]:
   - Almost all of sections 6.1 through 6.9, only excluding
     `rationalize`.
   - Section 6.13, i.e. ports (streams in CL).
 - Some Scheme libraries:
   - Library definitions for the `(scheme)` libraries described in
     [R7RS-small] Appendix A.
   - Some SRFIs entirely contained within R7RS-small: 6, 9, 87, and 98.
   - [SRFI 112] and [SRFI 172].

[R7RS-small]: https://small.r7rs.org/attachment/r7rs.pdf
[SRFI 112]: https://srfi.schemers.org/srfi-112/srfi-112.html
[SRFI 172]: https://srfi.schemers.org/srfi-172/srfi-172.html
[Unreleased]: https://gitlab.com/mbabich/airship-scheme/-/compare/bd61fb8f...master

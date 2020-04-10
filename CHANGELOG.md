# Changelog

This changelog uses the style from [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
 - The standard procedures from [R7RS-small] sections 6.1 through 6.9.
 - A separate false value from nil, which is not false in Scheme.
 - A wrapper over the Unicode functionality of SBCL (`sb-unicode`).
 - A custom reader for the [R7RS-small] version of Scheme.
 - The number portion of the Scheme writer, as `write-scheme-number`, for `number->string`.
 - An internal representation for Scheme that supports tail recursion and continuations.
 - Stub library definitions for the Scheme libraries described in [R7RS-small] Appendix A.
 - A hello world example file and an example library that exports that hello world.
 - Subsets of the [R7RS-small] libraries, specified as [SRFI 172].

[R7RS-small]: https://small.r7rs.org/attachment/r7rs.pdf
[SRFI 172]: https://srfi.schemers.org/srfi-172/srfi-172.html
[Unreleased]: https://gitlab.com/mbabich/airship-scheme/-/compare/bd61fb8f...master

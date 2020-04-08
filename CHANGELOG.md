# Changelog

This changelog uses the style from [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
 - The standard procedures from r7rs sections 6.1 through 6.9.
 - A separate false value from nil, which is not false in Scheme.
 - A wrapper over the Unicode functionality of SBCL.
 - A custom reader for the r7rs version of Scheme.
 - The number portion of the Scheme writer, as `write-scheme-number`.
 - An internal representation for Scheme that supports tail recursion and continuations.
 - Stub library definitions for the Scheme libraries described in r7rs Appendix A.
 - A hello world example file and an example library that exports that hello world.

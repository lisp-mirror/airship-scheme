Airship Scheme
==============

A new r7rs Scheme implementation, designed to be run within a Common
Lisp (CL) environment.

And yes, [an "airship scheme" is a type of
scheme](https://en.wikipedia.org/wiki/Imperial_Airship_Scheme)!

Warning
-------

This repository is currently a work-in-progress. Expect major changes
to happen rapidly and without warning.

The basic language is currently **not** working because some of the
key functionality is incomplete.

Overview
--------

Airship Scheme is a new implementation of the standardized Scheme
programming language, implemented in Common Lisp and designed to run
within a Common Lisp environment. This enabling interoperability
between Scheme and Common Lisp code. Unlike with the existing
approaches that compile Scheme to languages like C or JavaScript, the
Common Lisp host language already contains many elements that Scheme
requires, including the cons cell (or cons pair) data structure and
the numeric tower.

On the other hand, the Scheme to CL compilation process is no longer
as trivial as it was in the days of
[Pseudoscheme](http://mumble.net/~jar/pseudoscheme/) because Common
Lisp and Scheme have drifted further apart as languages over the past
few decades, making some of the assumptions of past approaches no
longer viable.

Installing
----------

See [INSTALL.md](INSTALL.md) for installation instructions.

Contributing
------------

See [CONTRIBUTING.md](CONTRIBUTING.md) for contributing, terminology,
and style guidelines.

License
-------

MIT License. See [LICENSE.txt](LICENSE.txt) for the license text.

Using Airship Scheme
--------------------

This section will explain how to use Airship Scheme once it is
installed. This section is currently incomplete because the
programming language is not functional at the moment.

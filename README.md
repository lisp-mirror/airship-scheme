Airship Scheme
==============

A new r7rs Scheme implementation, designed to be run within a Common
Lisp (CL) environment.

Yes, [an "airship scheme" is a type of
scheme](https://en.wikipedia.org/wiki/Imperial_Airship_Scheme)!

Warning
-------

*This repository is currently a work-in-progress. Expect major changes
to happen rapidly and without warning.*

The basic language is currently **not** working because some of the
key functionality is incomplete.

The language will be usable, although still unstable, in [version
0.1](https://gitlab.com/mbabich/airship-scheme/-/milestones/4). Until
then, the versions will be of the form 0.0.n, which allows an
arbitrary number of unexpected versions to be inserted as new
complications arise.

Overview
--------

Airship Scheme is a new implementation of the Scheme programming
language, implemented in Common Lisp and designed to run within a
Common Lisp environment. This allows programs to run both Scheme and
Common Lisp code in the same runtime.

There are many existing implementations that compile Scheme to
languages like C or JavaScript. With the choice of Common Lisp, the
host language already contains many elements that Scheme requires.
This includes the cons cell (or cons pair) data structure and the
numeric tower.

On the other hand, the Scheme to CL compilation process is no longer
as trivial as it was in the days of
[Pseudoscheme](http://mumble.net/~jar/pseudoscheme/) because Common
Lisp and Scheme have drifted further apart as languages over the past
few decades, making some of the assumptions of past approaches no
longer viable. There are also syntactic incompatibilities, meaning
that a fully conforming Scheme cannot simply use the host CL's reader.

Why use this project?
---------------------

This project is primarily aimed at both Common Lispers and Schemers.

From the Common Lisp perspective, it offers Scheme as a convenient
library: it's no harder to install or use than any other CL library.
From the Scheme perspective, it offers the potential of a
high-performance implementation for "free" by taking advantage of
high-performance CL compilers like SBCL. From both perspectives, it
will allow someone to wrap libraries written for one of the languages
for the use in the other, which could potentially double the number of
libraries available. It also allows a level of integration between the
two languages that's not normally seen.

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

*This section is currently incomplete because the programming language
is not functional at the moment.*

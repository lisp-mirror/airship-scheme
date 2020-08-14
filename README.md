Airship Scheme
==============

A new r7rs Scheme implementation, designed to interoperate with a
Common Lisp (CL) environment.

Yes, [an "airship scheme" is a type of
scheme](https://en.wikipedia.org/wiki/Imperial_Airship_Scheme)!

Note
----

The primary repository is located
[here](https://gitlab.com/mbabich/airship-scheme). Progress towards
completion is measured under the [milestones
here](https://gitlab.com/mbabich/airship-scheme/-/milestones).

Warning
-------

*This repository is currently a work-in-progress. Expect major changes
to happen rapidly and without warning.*

The basic language is currently **not** working because some of the
key functionality is incomplete.

The language will be usable, although still unstable, in [version
0.1](https://gitlab.com/mbabich/airship-scheme/-/milestones/4). Until
then, the versions will be of the form 0.0.n. After that, the versions
will be of the form 0.n. That is, the first version is version 0.0.1,
which will be followed by 0.0.2, and so on. The first usable version
is version 0.1, which will be followed by 0.2, and so on.

Overview
--------

Airship Scheme is a new implementation of the Scheme programming
language, implemented in Common Lisp and designed to run within a
Common Lisp environment. The embedded nature of this architecture
allows programs to run both Scheme and Common Lisp code in the same
runtime.

There are many existing implementations that compile Scheme to
languages like C or JavaScript. With the choice of Common Lisp, the
host language already contains many elements that Scheme requires. The
host CL's data structures include the cons cell (or cons pair) data
structure and the numeric tower, both of which can be used in the
Scheme itself.

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

### For Common Lispers

It offers Scheme as a convenient library: it's no harder to install or
use than any other CL library. Most Scheme libraries will be usable in
Common Lisp, and a lot of core-or-SRFI Scheme functionality will be
provided in a CL-compatible form. Custom sequences not found in CL
will be implemented via `trivial-extensible-sequences`. Eventually, a
reader macro could be provided that provides most of the benefits of
Scheme's additional syntax while still using the CL reader.

### For Schemers

It offers the potential of getting a high-performance implementation
for "free" by taking advantage of high-performance CL compilers like
SBCL. Schemers will be able to wrap libraries written in CL, which
will more than double the number of libraries available. These wrapped
CL libraries will feel a lot "Lispier" than wrapped C libraries
because CL is a closely related programming language.

Installing
----------

See [INSTALL.md](INSTALL.md) for complete installation instructions.

Quickstart
----------

The fastest way to get a Common Lisp development environment for
someone who doesn't currently have one is to install
[Portacle](https://portacle.github.io/).

Then, inside of the Quicklisp `local-projects` directory:

```sh
git clone https://gitlab.com/zombie-raptor/zr-utils.git
git clone https://gitlab.com/mbabich/airship-scheme.git
```

Then you can run this at the Common Lisp REPL:

```lisp
(ql:quickload :airship-scheme)
```

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

Airship Scheme
==============

A new r7rs Scheme implementation, designed to run within a Common Lisp
environment.

On IRC
------

Airship Scheme now is on IRC! This project uses
[`irc.libera.chat`](https://libera.chat/) as its IRC network. The main
channel is `#airship`, but `#scheme` and `#lisp` also might be useful.

About the name
--------------

The name was inspired by [the "Imperial Airship Scheme" Wikipedia
article](https://en.wikipedia.org/wiki/Imperial_Airship_Scheme), via
[a comment someone else made on Hacker
News](https://news.ycombinator.com/item?id=13868549) in early 2017.
Prior to that, this project used the working title `cl-scheme`. The
old name is more descriptive, but considerably less interesting. The
`#scheme` channel on Freenode IRC loved the name, which was shortened
to "Airship Scheme" for simplicity.

Note
----

The primary repository is located
[here](https://gitlab.com/mbabich/airship-scheme). Progress towards
completion is measured under the [milestones
here](https://gitlab.com/mbabich/airship-scheme/-/milestones).

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

Status
------

This project depends on sharing a large amount of code initially
written for a Common Lisp first person 3D game engine called Zombie
Raptor. Parts of that effort were spun off into the `zr-utils` utility
library, which is a work in progress library with an unstable API. In
fact, that library was spun off precisely to be used as a shared
dependency between the game engine and this project. All of these are
still incomplete.

As of 2024, most of the recent work towards this project has been
written as general purpose programming language internals instead of
happening directly in this repository. In particular, there's also a
vector and shader language, ZRVL, which is of more direct importance
to the game engine. The smaller scope of this other language means
that it is a more suitable place to develop the shared internals of
programming languages that target Common Lisp implementations as a CPU
runtime. Additionally, ZRVL will also target SPIR-V to be used for
graphics shaders, but the semantics of Scheme would make it
challenging to do so without subsetting the language.

Work on this project will resume when the internals of ZRVL are spun
off as a useful intermediate representation that Airship Scheme can
also benefit from.

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
because CL is a closely related programming language. Additionally,
almost all of the Common Lisp standard library will be wrapped.

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

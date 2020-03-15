Installation
============

Airship Scheme requires a host Common Lisp implementation to run,
ideally SBCL. Other major implementations, especially CCL, will be
supported soon, but initial testing will only be done in SBCL until
the programming language actually works.

Using [Quicklisp](https://www.quicklisp.org/beta/) to handle the
dependencies is recommended, but Airship Scheme itself is currently
not available in Quicklisp. When Airship Scheme is available in
Quicklisp, these instructions will only be required to install the
up-to-date git version of Airship Scheme, which will override the
version in Quicklisp.

Instructions on how to make airship-scheme a Quicklisp local project
are available
[here](https://www.quicklisp.org/beta/faq.html#local-project) and
[here](http://blog.quicklisp.org/2018/01/the-quicklisp-local-projects-mechanism.html).

**Note:** You can create a symbolic link to the project directory in
the Quicklisp local-projects directory. Quicklisp will follow links in
most implementations, but it has been reported that this does not work
for CCL.

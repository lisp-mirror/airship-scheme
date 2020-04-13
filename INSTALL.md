Installation
============

Step 1: Install Common Lisp
---------------------------

The Airship Scheme installation process depends on the user having an
Common Lisp already installed. On Linux, your Linux distribution
probably has an old version of SBCL available, which can then be used
to compile a newer version of SBCL. On other OSes, you should be able
to directly install a binary online, such as [here for
SBCL](http://www.sbcl.org/platform-table.html), which will, again,
probably be an outdated starting point for compiling the latest
version. The latest version shouldn't be required, but this hasn't
been tested on every old version of SBCL.

There are also tools that exist to make this process more
beginner-friendly. The most popular is probably
**[Portacle](https://portacle.github.io/)**. Additionally,
[Roswell](https://github.com/roswell/roswell) is another option.

**SBCL is recommended.** CCL should also work. The more obscure the CL
implementation, the less likely it is for all of the features to be
available because Airship Scheme depends on third-party or
implementation-provided libraries in certain places and not every
implementation exposes all of the necessary functionality.

**Note: While the basic R7RS language is still incomplete, only SBCL
will be used for testing and development to keep things simple.**

Step 2: Install Quicklisp
-------------------------

In Common Lisp, **[Quicklisp](https://www.quicklisp.org/beta/) is the
recommended way to install libraries**, including Airship Scheme's
dependencies. All of the dependencies are either available in
Quicklisp or are easily added to `local-projects` in the same way as
Airship Scheme.

**Note:** Portacle and Roswell already come with Quicklisp bundled.

Step 3A: Manually Add Airship Scheme
------------------------------------

*Airship Scheme itself is currently not available in Quicklisp. When
Airship Scheme is available in Quicklisp, these instructions will only
be required to install the up-to-date git version of Airship Scheme,
which will override the version in Quicklisp.*

Download this git repository and make it recognizable to Quicklisp as
a "local project" in your local projects directory, which might be
located at `~/quicklisp/local-projects/`.

Instructions on how to make airship-scheme a Quicklisp local project
are available
[here](https://www.quicklisp.org/beta/faq.html#local-project) and
[here](http://blog.quicklisp.org/2018/01/the-quicklisp-local-projects-mechanism.html).

The dependency `zr-utils` also needs to be downloaded. Everything else
is in Quicklisp. The fastest way to do so is to go to your
`local-projects` directory and run:

```sh
git clone https://gitlab.com/zombie-raptor/zr-utils.git
git clone https://gitlab.com/mbabich/airship-scheme.git
```

**Note:** You can create a symbolic link to the project directory in
the Quicklisp local-projects directory. Quicklisp will follow links in
most implementations, but it has been reported that this does not work
for CCL. If this doesn't work, then manually editing
`~/quicklisp/local-projects/system-index.txt` should work.

Step 3B: Quickload Airship Scheme
---------------------------------

At this point, you should be able to run the following command in the
Common Lisp REPL:

```common-lisp
(ql:quickload :airship-scheme)
```

Now every dependency will be loaded and after that Airship Scheme
itself will load.

*At this point, a way to switch into the Airship Scheme REPL would be
a nice feature to add, when the REPL is functioning. Direct access to
Airship Scheme via SLIME and/or Geiser in Emacs would be nice features
to have, too.*

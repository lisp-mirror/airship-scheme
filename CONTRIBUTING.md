Contributing to Airship Scheme
==============================

For the Common Lisp code, Airship Scheme uses the same guidelines as
the Zombie Raptor game engine, where the guidelines still make sense.
Things specific to the game engine do not apply. Eventually, the
things in common will be spun off into a separate guide, containing
only the information that all Common Lisp projects share in common.
For more, see [its contributing
guide](https://gitlab.com/zombie-raptor/zombie-raptor/-/blob/master/CONTRIBUTING.md).

The special nature of being a project at the intersection of Common
Lisp and Scheme requires some special attention because of the
similarities and differences between the two languages, which are
closely related, but with divergences in key places.

The Scheme code will generally be written in a similar style to a
Common Lisp project, but fundamental differences exist between the two
languages making style concerns not entirely trivial. A separate
Airship Scheme style guide will have to be written in the future.

Complications and design solutions
----------------------------------

Common Lisp is, in its default behavior, effectively case-insensitive.
It does this by upcasing any lower case input. For example, `car`
becomes `CAR`. This was done for maximal backwards compatibility.
These days, Common Lisp is mostly written in lower case and relies on
this automatic conversion to upper case. Scheme, on the other hand, is
a case-sensitive programming language. Thus, `car` in Scheme remains
`car` and a Scheme program could define a separate `Car` or `CAR` that
needs to be treated separately. The easiest solution to this problem
is to *invert* the case of the Scheme implementation. In other words,
`car` becomes `CAR` internally and `Car` would become `cAR`
internally.

Common Lisp does not distinguish between false and the empty list,
treating both as `NIL`. Scheme, on the other hand, distinguishes
between the false value `#f` and the empty list `'()`. In order to
preserve the native cons cell data structure, Airship Scheme chooses
to have a separate false value for Scheme. This means that any wrapped
Common Lisp function must have its semantics distinguished as either a
regular procedure or a predicate. This is done by providing both the
`define-scheme-procedure` and `define-scheme-predicate` macros. In a
Scheme procedure, CL's `NIL` is treated as the empty list and not
converted. In a Scheme predicate, the CL call is wrapped by a simple
function that converts `NIL` to Scheme's `#f`. The other direction is
easier. Calling Scheme from CL will always turn `#f` into `NIL`,
although that presents the downside of some information potentially
being lost.

Common Lisp does not guarantee optimized tail recursion, even though
some implementations do offer it. Even when that is the case, tail
recursion semantics cannot be treated as necessarily identical to
Scheme's. Additionally, differing optimization levels can take code
that would normally have tail recursion and remove the tail recursion.
This is unacceptable from the perspective of a guest Scheme in the CL
environment because tail recursion is the idiomatic iteration in
Scheme and unexpected recursion limits would be bad. For this reason,
there needs to be a minimal runtime environment, where the guest
Scheme code does recursion through a
[trampoline](https://en.wikipedia.org/wiki/Trampoline_(computing)).
This guarantees tail recursion and in testing has shown to have no
noticeable performance loss over a CL implementation's native tail
recursion.

Although Common Lisp has lexical scope in addition to dynamic scope,
the standard Common Lisp global variable environment, via `defvar` or
`defparameter` is always dynamically scoped. This means that while
Common Lisp environments can normally be used, they cannot be used for
a mutable global Scheme environment for things like the REPL and CL's
interactive, SLIME-style development. This has not yet been completely
addressed. Additionally, CL is a Lisp-2 (having separate variable and
function namespaces) while Scheme is a Lisp-1 (having a combined
variable-and-function namespace), meaning that the Scheme code should
be able to create a second, function-namespace binding that the CL can
call into. This would be desirable, anyway, because CL calling into
Scheme needs to add a trampoline at the entry point. [Wikipedia
explains dynamic scope in
detail.](https://en.wikipedia.org/wiki/Scope_(computer_science)#Dynamic_scoping)

Common Lisp does not have Scheme-style continuations. This means that
even if Common Lisp `lambda`s are used in the compilation process, the
`lambda`s probably need to use something like Continuation Passing
Style in order to allow for things like `call/cc` to work.

Terminology differences
-----------------------

Even though Scheme and Common Lisp are both Lisps, there are quite a
few terminology differences between the two languages, making
describing the details of project like this more complicated than it
has to be.

In Scheme, "procedures" are the named or unnamed `lambda`s. In Common
Lisp, these are "functions". Scheme calls them "procedures" because
its "functions" are what Common Lisp would call "pure functions":
procedures without side effects.

There are quite a few names for [the cons data
structure](https://en.wikipedia.org/wiki/Cons). These include "cell",
"cons", "cons cell", "cons pair", "pair", etc. Scheme tends to prefer
"pair" instead of "cons", but that is a Scheme-ism. They are rarely,
if ever, called "pairs" in Common Lisp, where "cons" or "cons cell"
are more common.

When a linked list is built from conses, there are two possibilities.
Either the last `cdr` is `NIL` (or the empty list) or the last `cdr`
is not. In the latter case, it is represented syntactically as a
dotted list, like `(1 2 3 . 4)`, and is called an "improper list".
Otherwise, it is represented without any dots like `(1 2 3 4)` and is
called a "proper list". In Scheme, the word "list" without
qualifications implies that it is a "proper list". In Common Lisp,
this is not the case. This means, for example, that Scheme's `list?`
tests for proper lists, while Common Lisp's `listp` tests for proper
or improper lists. The Alexandria utility library offers an
`alexandria:proper-list-p` that is equivalent to Scheme's `list?`
procedure.

Differing conventions
---------------------

In Common Lisp, end-of-line comments start with `;`, and comments on
their own line start with `;;` if not at the top level (i.e. if
indented) and with `;;;` or `;;;;` if at the top level (i.e. if not
indented). Generally, `;;;;` is used for file headers and `;;;`
elsewhere, but not every Common Lisp project uses `;;;;`. Scheme
mostly shares the same styles for s-expressions and comments as Common
Lisp, but the common Scheme styles only distinguish between `;` for
end-of-line comments and `;;` for on-their-own-line comments. Whether
a Scheme uses `;;;` or `;;;;` varies more than with CL, where `;;;` is
extremely common. Because of its intended purpose of running in the CL
environment, Airship Scheme must use the Common Lisp comment
convention, including the use of `;;;` and `;;;;` where appropriate.

In Common Lisp, predicates end with `p`. The old convention is to end
with `p` if it is one word, such as `foop`, and end with `-p` if it is
more than one word, such as `foo-bar-p`. A newer convention is to
always end in `-p` so that it is always easy to tell if something is a
predicate. Scheme always ends its predicates with `?`, such as `foo?`.
Some Common Lisp code does this, but it is rare. This project will
mostly use the Scheme convention, but there are times where it makes
sense to make something look like it is a Common Lisp built-in, in
particular when writing Common Lisp functions for
`standard-procedures.lisp` to wrap.

Scheme procedures with side effects end in `!`, e.g. `foo!`, making it
easy to tell when something is pure or not. In Common Lisp, there is
no direct equivalent, but `nfoo` (where `n` stands for "nonconsing")
and `foof` (in the style of `setf` or `incf`) are often used. Both are
problematic because a function with side effects *can* still be
"consing" (i.e. heap-allocating) and `foof` is specifically for
dealing with "places". For that reason, Common Lisp code that must
distinguish side effects in a clear way should use `!`, but that's not
as necessary in Common Lisp as in Scheme. In this project, `!` should
be used.

Scheme uses `foo->bar` for conversion procedures while Common Lisp
tends to use `foo-to-bar`. Scheme uses `$foo` for constants while
Common Lisp uses `+foo+`. **Never** use `$foo` in Common Lisp code.

Common Lisp global variables use "earmuffs", like `*foo*`. However,
this is actually just a side effect of all portable Common Lisp global
variables being dynamic (special) variables. Portable Scheme does not
have dynamically scoped variables.

In Common Lisp, `deffoo` is usually used for single word defines
instead of `define-foo`, while `define-foo-bar` is always used for
hyphenated names. This is not always the case. The Common Lisp
standard itself uses the name `define-condition`. Scheme should always
use `define-foo`.

This traditional CL convention for `define`s is even more problematic
for tools than the `p` predicate convention, so this project **must**
always use the `define-` prefix in the Common Lisp portion of the
code, even for the case of `define-foo`. Tools probably want to do
interesting things with `define`s so machine-readability is more
important than with predicates. (An Airship Scheme wrapper of Common
Lisp code really could use a tool that automatically distinguishes
between predicates and non-predicates due to how the wrapping process
works, but Airship Scheme is a rare, special case in its needs.)

*Cliki (the Common Lisp wiki) mentions the [Common Lisp naming
conventions](https://www.cliki.net/Naming+conventions) in general and
covers a lot of the same ground, with an emphasis on Common Lisp
rather than an emphasis on contrasting CL with Scheme.*

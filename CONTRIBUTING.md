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

### Symbol case

Common Lisp is, in its default behavior, effectively case-insensitive
for its symbols. It does this by upcasing any lower case input. For
example, `car` becomes `CAR`. This was done for maximal backwards
compatibility. These days, Common Lisp is mostly written in lower case
and relies on this automatic conversion to upper case. Scheme, on the
other hand, is a case-sensitive programming language. Thus, `car` in
Scheme remains `car` and a Scheme program could define a separate
`Car` or `CAR` that needs to be treated separately. The easiest
solution to this problem is to *invert* the case of the Scheme
implementation. In other words, `car` becomes `CAR` internally and
`Car` would become `cAR` internally.

### `NIL` vs. `#f`

Common Lisp does not distinguish between false and the empty list,
treating both as `NIL`. Scheme, on the other hand, distinguishes
between the false value `#f` and the empty list `'()`. In order to
preserve the native cons cell data structure, Airship Scheme chooses
to have a separate false value for Scheme.

This means that any wrapped Common Lisp function must have its
semantics distinguished as either a regular procedure or a predicate.
This is done by providing both the `define-scheme-procedure` and
`define-scheme-predicate` macros. In a Scheme procedure, CL's `NIL` is
treated as the empty list and not converted. In a Scheme predicate,
the CL call is wrapped by a simple function that converts `NIL` to
Scheme's `#f`. The other direction is easier. Calling Scheme from CL
will always turn `#f` into `NIL`, although that means that some
information may be lost.

Internally, Scheme's `#f` is just `'%scheme-boolean:f`. There should
be no performance penalty because most Common Lisp implementations are
already comparing to `NIL` to find false. All that changes is the
exact symbol that's being used.

### Tail recursion

Common Lisp does not guarantee optimized tail recursion, even though
some implementations do offer it. Even when that is the case, tail
recursion semantics cannot be treated as necessarily identical to
Scheme's. Additionally, differing optimization levels can take code
that would normally have tail recursion and remove the tail recursion.
This is unacceptable from the perspective of a guest Scheme in the CL
environment because tail recursion is the idiomatic iteration in
Scheme and unexpected recursion limits would be bad.

For this reason, there needs to be a minimal runtime environment,
where the guest Scheme code does recursion through a
[trampoline](https://en.wikipedia.org/wiki/Trampoline_(computing)).
This guarantees tail recursion and in testing has shown to have no
noticeable performance loss over a CL implementation's native tail
recursion.

### Environments

Although Common Lisp has lexical scope in addition to dynamic scope,
the standard Common Lisp global variable environment, via `defvar` or
`defparameter` is always dynamically scoped. This means that while
Common Lisp environments can normally be used, they cannot be used for
a mutable global Scheme environment for things like the REPL and CL's
interactive, SLIME-style development. This has not yet been completely
addressed.

Additionally, CL is a Lisp-2 (having separate variable and function
namespaces) while Scheme is a Lisp-1 (having a combined
variable-and-function namespace), meaning that the Scheme code should
be able to create a second, function-namespace binding that the CL can
call into. This would be desirable, anyway, because CL calling into
Scheme needs to add a trampoline at the entry point. [Wikipedia
explains dynamic scope in
detail.](https://en.wikipedia.org/wiki/Scope_(computer_science)#Dynamic_scoping)

### Type system

Portable Scheme only has the predicates, such as `list?`. Scheme can
be thought of as being "predicatively" typed. Airship Scheme uses this
convention in its type names, making its Scheme type names end in `?`.

Technically, Common Lisp isn't just a "Lisp-2". It actually has more
than two namespaces. The third most popular namespace is the *type*
namespace, used by types defined by `defclass`, `deftype`, etc. One
possible solution is to introduce this type namespace to Scheme, but
standard Scheme is predicately typed. These could be seen as
`satisfies` types in CL, but those are inefficient. The simplest
efficient solution, then, is to have a `define-scheme-type` macro that
defines both a type and a predicate of the same name (e.g. `pair?`).

Thus, in Airship Scheme, properly defined Scheme types always define a
corresponding predicate of the same name, but are probably defined in
a more efficient way (such as `define-scheme-type` on the CL side or
`define-type` on the Scheme side).

This is generally done by automatically generating a trivially inline
procedure that calls `typep` (in the CL side) or `type?` (in the
Airship Scheme side). This means that this abstraction is a
potentially leaky abstraction since the procedure might be redefined
locally. However, `type?` will usually behave as expected because
there is no local way to define a type.

That is, technically speaking, this type namespace still exists and is
accessed through an exposed `type?` predicate on the Scheme side. For
example, `(type? foo 'pair?)` would be `#t` if `foo` is a `pair?` even
if `pair?` has been locally rebound in a `let`.

### Continuations

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

In general, Common Lisp has more consistent conventions than Scheme,
with a few notable exceptions, such as Scheme's `!` naming convention
for procedures with side effects. Where a clear convention exists in
Scheme, Airship Scheme will follow that Scheme convention. Otherwise,
Airship Scheme will borrow the convention from Common Lisp to ease
interoperability, even if this convention hasn't been seen in Scheme
before.

### Comments

In Common Lisp, end-of-line comments start with `;`. Comments on their
own line start with `;;` if not at the top level (i.e. if indented)
and with `;;;` or `;;;;` if at the top level (i.e. if not indented).
Generally, `;;;;` is used for file headers and `;;;` elsewhere, but
not every project uses `;;;;`. Scheme mostly shares the same styles
for s-expressions and comments as Common Lisp, but Schemers don't
always insist on using `;;;`. In Scheme, `;;;;` is also infrequently
seen.

Airship Scheme **must** use the `;;;;` comment convention in both the
Common Lisp and Scheme files. This means that comments on their own
line that are not indented use `;;;` except if they are a heading
and/or header (typically at the top of the file). Those use `;;;;`
instead.

#### Example

The following example code has *terrible* style because the comments
are unnecessary for the level of complexity, but the code demonstrates
the three different levels of `;`s:

```common-lisp
;;; Squares the input
(defun square (x) ; x is a number
  ;; Squares x
  (expt x 2))
```

```scheme
;;; Squares the input
(define (square x) ; x is a number
  ;; Squares x
  (expt x 2))
```

### Naming conventions

In Common Lisp, a slight variation of `foo` is usually called `foo*`.
Something that's for low-level or internal-use is often called `%foo`.
Airship Scheme uses this same convention.

Scheme uses `foo->bar` for conversion procedures while Common Lisp
tends to use `foo-to-bar`.

#### Predicates

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

#### Side effects

Most Scheme procedures with side effects end in `!`, e.g. `foo!`,
making it easy to tell when something is pure or not. In Common Lisp,
there is no direct equivalent, but `nfoo` (where `n` stands for
"nonconsing") and `foof` (in the style of `setf` or `incf`) are often
used. Both are problematic because a function with side effects *can*
still be "consing" (i.e. heap-allocating) and `foof` is specifically
for dealing with "places". For that reason, Common Lisp code that must
distinguish side effects in a clear way should use `!`, but that's not
as necessary in Common Lisp as in Scheme. In this project, `!` should
be used.

Some built-in Scheme procedures with side effects do not end in `!`,
such as `display`. This is for historical reasons. New Scheme
procedures with side effects should end in `!`, but I/O procedures in
the style of `display` might still seem more idiomatic to exclude it.

#### Global constants and variables

Common Lisp constants are surrounded in `+`s, like `+foo+`. Scheme
does not have a consistent convention for constants, but some Scheme
readers might not recognize tokens beginning with `+` that aren't the
symbol `+` itself (used for addition) as a symbol because `+` is also
a numeric prefix (e.g. `+42` is a number). Airship Scheme does not
have this limitation, and so will use the Common Lisp `+foo+` naming
convention for its constants.

Common Lisp global variables use "earmuffs", like `*foo*`. However,
this is actually just a side effect of all portable Common Lisp global
variables being dynamic (special) variables. Portable Scheme does not
have dynamically scoped variables.

#### Definitions

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

#### Type names

Common Lisp types are generally written like everything else, in lower
case, and often have name collisions (e.g. `list` the type and `list`
the function that creates a list) that aren't an issue because the
type namespace is a separate namespace (similar to how variables and
functions have different namespaces). Scheme types are inconsistent,
but many implementations capitalize the start of a type name, e.g.
`List`.

As mentioned earlier, Airship Scheme uses the predicate convention in
its type names, so all of its Scheme types should end in `?`. This is
enforced through `define-scheme-type` on the CL side or `define-type`
on the Scheme side.

There are some built-in ways to define types in ways that might
violate this, but it is still maintained by convention to have the
type and the predicate to test for the type have the same name. In
Airship Scheme it is conventional to use the same name for the `name`
and the `predicate` parts of `define-record-type`, although they can
be different.

Scheme rarely provides efficient versions of various types, preferring
the plainer definition, even if it requires a runtime predicate test.
`exact-integer?` instead of `integer?` is a good example of where
Scheme provides an efficient way, since `exact-integer?` will test for
the underlying integer type (e.g. `42`) while `integer?` will test for
the mathematical concept (including `42.0`).

When efficient versions don't exist, Airship Scheme will have to
define them, such as `%list?`, which permits improper lists and should
be preferred when working with lists.

*Cliki (the Common Lisp wiki) mentions the [Common Lisp naming
conventions](https://www.cliki.net/Naming+conventions) in general and
covers a lot of the same ground, with an emphasis on Common Lisp
rather than an emphasis on contrasting CL with Scheme.*

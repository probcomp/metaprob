# Interacting with metaprob-clojure

There are many ways to do development and other activities in Clojure.

## Using the Clojure REPL

The REPL can be started from the shell, or from emacs.  With emacs you
get many desirable tools such as automatic indentation, but the setup
is more complex and the learning curve is steep if you haven't used
emacs before.

### Using Clojure under the shell

To get an interactive read-eval-print loop at the shell:

    $ lein repl

This should be done with the working directory set to the directory
that contains `project.clj`, which in this setup would normally be the
clone of the `metaprob-clojure` repository.

### Using Clojure under Emacs

This section describes using a REPL in emacs.  The Emacs interface to
Clojure is called 'Cider'.  (Or at least the interface that I know about.)

Cider also knows to look at `project.clj`.

#### Starting a REPL under Emacs

Clojure runs outside of emacs, and emacs connects to it over a TCP
connection.  At a shell, in its own terminal window or tab (not
necessarily in emacs), go to the directory that contains
`project.clj`, and do:

    $ lein repl :headless

This takes a few seconds, then prints a TCP port number (it's different
every time).  In emacs, do:

    M-x cider-connect
    localhost
    {port} C-j

where `{port}` is the port number you saw when you did `lein repl
:headless`, M-x is meta-x, and C-j is control-J or linefeed.

(Of course the fact that the Clojure connection goes over TCP/IP means
you can put the server anywhere on the Internet that you want to.  The
best way to connect is with an ssh tunnel, but that is a story for
another day.)

#### Emacs commands

Evaluate an expression from a buffer with C-c C-E.  Cider knows how to
figure out which namespace to evaluate in (see below).

You can load a clojure file by visiting it in a buffer and doing C-c
C-k.  I think doing so will also load any needed dependencies as
inferred from the `ns` form at the top of the file.

C-h m will show you commands available in REPL mode.
There's more documentation of Cider somewhere.

### Working with namespaces

Exploration from the REPL is a little bit annoying due to the Clojure
namespace system.  Unlike in Common Lisp, which has its `::` syntax,
you can't even access a namespace unless you've previously 'required'
it.

Namespace names are typically long, so it's useful to define namespace
prefixes to abbreviate them.  I do this manually, e.g.

    > (require '[metaprob.builtin :as builtin])

at the REPL, so that I can say

    (builtin/trace-get x "foo")
    (builtin/pprint x)

and so on.  Alternatively, you can access binding without using a
prefix at all by using `:refer :all`:

    > (require '[metaprob.trace :refer :all])
    > (require '[metaprob.builtin-impl :refer :all])

    (trace-get x "foo")
    (metaprob-pprint x)

This seems better; I don't know why I don't it; maybe afraid of name
collisions or the possibility of stale values in the `user` namespace.
(I still don't fully understand namespaces.)

You'll need to `require` each module you want to use at the REPL.  See
below for modules.

At the REPL you can switch to a different namespace using `in-ns`, e.g.

    > (in-ns 'metaprob.distributions)

By doing this you can see proper `builtin` bindings at the REPL, and
the internals of whatever module you're working on, but personally I
have not found this to be as useful as sticking to `user` and doing a
bunch of `require`s from there.  Maybe I just don't understand how to
use all this stuff.

### Refreshing the state

There is a `refresh` function that reloads your project, giving an
alternative to manually visiting each changed buffer and doing C-c
C-k.  Make `refresh` available at the REPL's `user` namespace with

    > (require '[clojure.tools.namespace.repl :refer [refresh]])

and invoke it with

    > (refresh)

(I guess this `require` could be put in project.clj or
.lein/profiles.clj so that it happens every time you start Clojure?
Need to look into this.)

Often during development, if the namespaces or `deftype` types
(`basic_trace.clj`) change in some incompatible way, I find it necessary
to restart clojure (C-c C-q followed by killing the `lein repl
:headless` process).  It may also be helpful in such situations to
remove the `target` directory, which caches `.class` files.

This is a pain in the butt because it can take a minute or so to kill
any running clojure under emacs, restart the REPL, connect to the new
REPL, and reload the project.

## Testing

There is documentation on the test system and it should be consulted,
as it doesn't make sense to repeat all that information here.

You can run tests either from the shell or from inside Clojure.  From
the Clojure REPL:

    (require '[clojure.test :refer :all])

    (run-tests 'metaprob.trace-test)    ;single module

Because of the constant need to restart emacs to fix weird problems
that are hard to figure out, sometimes I lose patience with REPL-based
debugging and work exclusively from the shell, by writing and
debugging tests (in concert with writing and debugging the main code).
This is nice because all files are freshly loaded every time, so you
assured of a clean environment.  The downside is an overhead of maybe
3-5 seconds for every time you want to run a test, and slogging
through long backtraces to figure out what went wrong.

From the shell: tests for all modules in the project:

    lein test

Just one module at a time:

    lein test metaprob.trace-test

Don't forget the `-test` in the module names!  I have spent a long
time being confused, first because I hadn't realized the `-test` was
needed, and later because I just forgot it.

Tests are all in the `test/metaprob/` directory.  The tests for
`src/metaprob/x.clj` are in `test/metaprob/x_test.clj`.

I like for tests that reside in the test system to run quickly.  It's
good to run all the tests frequently, and if this were a slow
operation I would be put off and would run them less often.

## Running code directly from the shell using the `-main` feature

Rather than use the REPL or the test system I sometimes just put code
in the `-main` function in some file e.g. `main.clj` and invoke it
directly from the shell:

    lein run -m metaprob.oopsla.main

Any command line arguments become arguments to the `-main` function.


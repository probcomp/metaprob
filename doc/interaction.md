# Interacting with metaprob-clojure

There are many ways to work with Metaprob; they are the same as the
many ways one works with Clojure.

 * From a read-eval-print loop (REPL)
 * From the shell
 * From tests
 * Using files


## Using the Clojure REPL

The REPL can be started from the shell, or from emacs.  With emacs you
get many desirable tools such as automatic indentation and namespace
system integration, but the setup is more complex and the learning
curve is steep if you haven't used emacs before.

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

### Simple scratch namespace for playing around

Exploration from the REPL is a little bit annoying due to the Clojure
namespace system.  Unlike in Common Lisp, which has its `::` syntax,
you can't even access a namespace unless you've previously 'required'
it.

To get started quickly, you can just switch to the examples namespace,
after which you won't have to think about namespaces until you want to
create new Clojure/Metaprob modules.  Enter the following at the REPL:

    (in-ns 'metaprob.examples.all)

The `metaprob.examples.all` namespace imports all of the Metaprob namespaces,
meaning all bindings are directly available and you don't have to
worry about the Clojure namespace system.

You can then evaluate metaprob expressions directly, run examples, and so on:

    (define x (trace "foo" 17))
    (trace-get x "foo")
    (metaprob-pprint x)

### Interaction in other namespaces

The `user` namespace starts out knowing nothing about the metaprob
namespaces.  For any kind of access you need to use `require`.

    > (require '[metaprob.builtin])

after which

    > (metaprob.builtin/trace "foo" 17)

Namespace names are typically long, so it's useful to define namespace
prefixes to abbreviate them.  This is what `:as` is for:

    > (require '[metaprob.builtin :as builtin])

After which:

    > (builtin/trace-get x "foo")
    > (builtin/pprint x)

and so on.  Alternatively, you can access bindings without using a
prefix at all by 'requiring' a namespace with `:refer :all`:

    > (require '[metaprob.trace :refer :all])
    > (require '[metaprob.builtin-impl :refer :all])

After which:

    > (trace-get x "foo")
    > (metaprob-pprint x)

Note that with `:refer :all` it is `metaprob.builtin-impl` being
accessed, not `metaprob.builtin`.  The latter has name conflicts with
Clojure and if we tried to use it with `:refer :all` we would get
collisions.

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


## The REPL and files

Clojure talks about files and modules, but I'm not clear on the
difference.  I think they are in 1-1 correspondence.

The top of a typical metaprob file would look something like:

    (ns myproject.myfile
      (:refer-clojure :only [ns declare])
      (:require [metaprob.syntax :refer :all]
                [metaprob.builtin :refer :all]
                [metaprob.prelude :refer :all]
                [metaprob.distributions :refer :all]
                [metaprob.infer :refer :all]))

If one of these imported modules isn't needed it can be left out of
the list.

At the REPL you can switch into any file's namespace using `in-ns`, e.g.

    > (in-ns 'myproject.myfile)

This can be useful but I find I almost never do it.  Instead I usually
evaluate expressions from inside files using emacs, or I use the test
system, or the user environment or `metaprob.examples.all`.  Your
mileage may vary.

## Running code noninteractively from the shell using the `-main` feature

Rather than use the REPL or the test system I sometimes just put code
in the `-main` function in some file e.g. `main.clj` and invoke it
directly from the shell:

    $ lein run -m metaprob.examples.main

Any command line arguments become arguments to the `-main` function.


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

-----

From here you might want to look at [the examples](examples.md) or
[the language reference](language.md).
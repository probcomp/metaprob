# Using metaprob-in-clojure

This page is about the mechanics of using metaprob-in-clojure.  For
information about the Metaprob 'language' see [language.md](language.md).  For
information on probabilistic programming examples see [examples.md](examples.md).

There are many ways to work with metaprob-in-clojure.  They are the
same as the ways one works with Clojure.  Generally you alternate
writing functions with exploration (including testing).

 * From a Clojure read-eval-print loop (REPL)
     * running Clojure direct from the shell (command line)
     * running Clojure under emacs
 * From files
     * putting code in files
     * writing tests and trying them out
     * running code via 'main' programs

## Using the Clojure REPL

The REPL can be started from the shell, or from emacs.  With emacs you
get many desirable tools such as automatic indentation and namespace
system integration, but the setup is more complex and the learning
curve is steep if you haven't used emacs before.

### Using Clojure under the shell

To get an interactive read-eval-print loop at the shell:

    $ bin/lein repl

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

    $ bin/lein repl :headless

This takes a few seconds, then prints a TCP port number (it's different
every time).  In emacs, do:

    M-x cider-connect
    localhost
    {port} C-j

where `{port}` is the port number you saw when you did `bin/lein repl
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
create new Clojure/Metaprob modules.  Enter the following at the REPL
to load Metaprob:

    (require 'metaprob.examples.all)
    (in-ns 'metaprob.examples.all)

The `metaprob.examples.all` namespace imports all of the Metaprob
namespaces, meaning that all Metaprob bindings are directly available
and you don't have to worry about the Clojure namespace system after
this point.  (This is fine for experimentation, but for more durable
programming I recommend convnetional use of the Clojure namespace
system, requiring only those namespaces you use.)

You can then evaluate metaprob expressions directly, run examples, and so on:

    (define x (trace "foo" 17))
    (trace-get x "foo")
    (pprint x)

### Creating namespaces for use with metaprob 

Metaprob has some name conflicts with Clojure, so some care is
necessary when preparing files containing Metaprob cod.  The method is
described in [language.md](language.md).


### Interaction in other namespaces

It is possible to user metaprob functions and macros from any Clojure
namespace, if Metaprob namespaces are made accessible using `require`.

For example, the default Clojure namespace, `user`, starts out
knowing nothing about the metaprob namespaces.  For any kind of access
from `user`, you need to use `require`.  E.g.

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

See [this stack overflow discussion](https://stackoverflow.com/questions/7658981/how-to-reload-a-clojure-file-in-repl).

Often during development, if the namespaces or `deftype` types
(`basic_trace.clj`) change in some incompatible way, I find it necessary
to restart clojure (C-c C-q followed by killing the `bin/lein repl
:headless` process).  It may also be helpful in such situations to
remove the `target` directory, which caches `.class` files.

There are many other circumstances that require a complete Clojure restart.

This is a pain in the butt because it can take a minute or so to kill
any running clojure under emacs, restart the REPL, connect to the new
REPL, and reload the project.  Therefore other alternative interaction
modes (see below) may sometimes be preferable.


## Using files

### Files/modules in their own namespace

Clojure talks about files and modules, but I'm not clear on the
difference.  I think they are in 1-1 correspondence.  Each file has
its own "namespace", thus another 1-1 correspondence.

The top of a typical metaprob file, say `myproject/myfile.clj`, would
look something like:

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

### Unit tests

There is documentation on the test system and it should be consulted,
as it doesn't make sense to repeat all that information here.

Basically `test` tree in a given project directory is parallel to the
`src` tree.  Each `.clj` file in `test/` contains unit tests for the
corresponding file in `src/`.

It is possible to do most or all development and testing using unit
tests instead of the REPL.  The disadvantages of tests compared to the REPL are

    * tests are not so good for exploration (what does this do?  what
      is in this data structure?)
    * it takes 3-5 seconds to start tests going, whereas the REPL has no delay

You can run tests either from the shell or from inside Clojure.  From
the Clojure REPL: Run tests for all modules in the project:

    (require 'metaprob.examples.all)
    (require '[clojure.test :refer :all])
    (run-all-tests)

Single module test: the two `require`s above, plus:

    (require 'metaprob.trace-test)
    (run-tests 'metaprob.trace-test)

Or, from the shell: Run tests for all modules in the project:

    lein test

Single module test:

    lein test metaprob.trace-test

Don't forget the `-test` in the module names, and `_test` in the file
names!  I have spent a lot of time being confused, first because I
hadn't realized the `-test` was needed, and later because I just
forgot it.  No error is reported when you get this wrong.

I like for tests that reside in the test system to run quickly so that
I can run `lein test` frequently and not have to wait a long time.
It's good to run all the tests frequently, and if this were a slow
operation I would be put off, and would run them less often.

### Running code noninteractively from the shell using the `-main` feature

Rather than use the REPL or the test system I sometimes just put code
in the `-main` function in some file e.g. `main.clj` (could be any
file name) and invoke it directly from the shell:

    $ lein run -m metaprob.examples.main

Any command line arguments become arguments to the `-main` function.

# Using Metaprob

This page is about the mechanics of using Metaprob.  For
information about the Metaprob 'language' see [language.md](language.md).

There are many ways to work with Metaprob.  They are the
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

    $ lein repl

This should be done with the working directory set to the directory that
contains `deps.edn`, which in this setup would normally be the clone of the
`metaprob` repository.

One difference between running `lein repl` versus running `clj` is that `lein
repl` will set the classpath according to the project's configuration.  Thus,
if you simply use `clj`, you may get import errors when trying to import or run
code.

### Using Clojure under Emacs

This section describes using a REPL in emacs.  The Emacs interface to
Clojure is called 'Cider'.  (Or at least the interface that I know about.)

Cider also knows to look at `deps.edn`.

#### Starting a REPL under Emacs

In emacs, do:

    M-x cider-jack-in RET

(Alternatively, the fact that the Clojure connection goes over TCP/IP means you
can put the server anywhere on the Internet that you want to and connect with
`cider-connect`. The best way to connect is with an ssh tunnel, but that is a
story for another day.)

#### Emacs commands

Evaluate an expression from a buffer with `C-c C-E`.  Cider knows how to
figure out which namespace to evaluate in (see below).

You can load a clojure file by visiting it in a buffer and doing `C-c
C-k`.  I think doing so will also load any needed dependencies as
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

    (def x (trace-set-value {} "foo" 17))
    (trace-value x "foo")
    (pprint x)

### Creating namespaces for use with metaprob

Metaprob has some name conflicts with Clojure, so some care is
necessary when preparing files containing Metaprob code.  The method is
described in [language.md](language.md).


### Interaction in other namespaces

It is possible to user metaprob functions and macros from any Clojure
namespace, if Metaprob namespaces are made accessible using `require`.

For example, the default Clojure namespace, `user`, starts out
knowing nothing about the metaprob namespaces.  For any kind of access
from `user`, you need to use `require`.  E.g.

    > (require '[metaprob.trace])

after which

    > (metaprob.trace/trace-set-value {} "foo" 17)

Namespace names are typically long, so it's useful to define namespace
prefixes to abbreviate them.  This is what `:as` is for:

    > (require '[metaprob.trace :as trace])

After which:

    > (trace/trace-value x "foo")
    > (trace/addresses-of x)

and so on.  Alternatively, you can access bindings without using a
prefix at all by 'requiring' a namespace with `:refer :all`:

    > (require '[metaprob.trace :refer :all])

After which:

    > (trace-value x "foo")
    > (addresses-of x)

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

There are other circumstances that require a complete Clojure restart.

This is a pain because it can take a minute or so to kill
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
      (:refer-clojure :exclude [map replicate apply])
      (:require [metaprob.generative-functions :refer :all]
                [metaprob.prelude :refer :all]
                [metaprob.distributions :refer :all]
                [metaprob.trace :refer :all]
                [metaprob.inference :refer :all]))

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

    (require 'metaprob.all-tests)
    (require '[clojure.test :refer :all])
    (run-all-tests)

Single module test: the two `require`s above, plus:

    (require 'metaprob.trace-test)
    (run-tests 'metaprob.trace-test)

Or, from the shell: Run tests for all modules in the project:

    clojure -Atest

Single module test:

    clojure -Atest -n metaprob.trace-test

Don't forget the `-test` in the module names, and `_test` in the file
names!  I have spent a lot of time being confused, first because I
hadn't realized the `-test` was needed, and later because I just
forgot it.  No error is reported when you get this wrong.

I like for tests that reside in the test system to run quickly so that
I can run `clojure -Atest` frequently and not have to wait a long time.
It's good to run all the tests frequently, and if this were a slow
operation I would be put off, and would run them less often.

### Running code noninteractively from the shell using the `-main` feature

Rather than use the REPL or the test system I sometimes just put code
in the `-main` function in some file e.g. `main.clj` (could be any
file name) and invoke it directly from the shell:

    $ clojure -m metaprob.examples.main

Any command line arguments become arguments to the `-main` function.

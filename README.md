# metaprob-clojure

Metaprob implemented in Clojure

## Installation

### Install Java

You will need Java to run Leiningen and Clojure.  I use OpenJDK
version 8.  You need the full Java development environment, not just the JVM.

### Install Leiningen and Clojure

It is not necessary to separately install Clojure if Leiningen is
installed.  Just install Leiningen, and let it take care of installing
the right version of Clojure.

Instructions for installing Leiningen are [here](https://leiningen.org/#install), 
but for a quick install try the short script in the Makefile: `make lein`.
This script assumes that `~/bin` is on your `PATH`, and it will put the `lein` command there.

Leiningen keeps some state in the `~/.lein` directory.

I interace with Clojure either using a REPL under emacs, or from the
shell using the `lein` command.  Tests (meaning just about any kind
computation that one situates inside a test file) can be run either
from the REPL or from the shell.  To run particular Clojure code
directly from the shell, you can put that code in the `-main` function
in `main.clj`.  These options are described below.

### Clone the metaprob-clojure repository

There is no particular installation procedure for metaprob-clojure.
Just set the working directory to the root of a clone of the
metaprob-clojure repository.

### Emacs setup

It is possible to use metaprob-clojure exclusively from the shell, but
running a REPL under emacs can be more pleasant.  (or less.)  So this
setup is optional.  What I suggest here is not necessarily right or
best; it's just stuff I got off the Internet.  I am not an expert.

Put the following in your `.emacs` file:

    ;; From http://clojure-doc.org/articles/tutorials/emacs.html.
    (require 'package)
    (add-to-list 'package-archives
                 '("melpa-stable" . "http://stable.melpa.org/packages/")
                 t)
    ;; "Run M-x package-refresh-contents to pull in the package listing."
    (package-initialize)
    (defvar clojure-packages '(projectile
                               clojure-mode
                               cider))
    (dolist (p clojure-packages)
      (unless (package-installed-p p)
        (package-install p)))

Here is my `~/.lein/profiles.clj`; I'm not sure why it is as it is,
but it seems to be harmless:

    ; Sample profile: https://gist.github.com/jamesmacaulay/5603176
    {:repl {:dependencies [[org.clojure/tools.namespace "0.2.11"]]
            :injections [(require '(clojure.tools.namespace repl find))]
            :plugins [[cider/cider-nrepl "0.15.1"]]}}

## Using Clojure under Emacs

There are many ways to do development or other activities in Clojure.
This section describes using a REPL in emacs.  After this is
information about the test system and using Clojure from the shell.

### Startup

Clojure runs outside of emacs, and emacs connects to it over a TCP connection.
At a shell, in its own terminal window or tab (not necessarily in
emacs), go to the directory that contains `project.clj`, and do:

    $ lein repl :headless

This takes a few seconds, then prints a TCP port number (it's different
every time).  In emacs, do:

    M-x cider-connect
    localhost
    {port} C-j

where `{port}` is the port number you saw when you did `lein repl
:headless`, M-x is meta-x, and C-j is control-J or linefeed.

### Once it's going.  Dealing with namespaces

You can load a clojure file by visiting it in a buffer and doing C-c
C-k.  I think doing so will also load any needed dependencies as
inferred from the `ns` form at the top of the file.

Exploration from the REPL is a little bit annoying due to the Clojure
namespace system.  Unlike in Common Lisp, which has the `::` syntax, you can't even access a
namespace unless you've previously 'required' it.

Namespace names are typically long, so I typically set up namespace
prefixes.  I do this manually, e.g.

    > (require '[metaprob.builtin :as builtin])

at the REPL, so that I can say

    (builtin/trace-get x "foo")
    (builtin/pprint x)

and so on.  Alternatively, and more concisely:

    > (require '[metaprob.trace :refer :all])
    > (require '[metaprob.builtin-impl :refer :all])

    (trace-get x "foo")
    (metaprob-pprint x)

This seems better; I don't know why I don't it; maybe afraid of name
collisions or the possibility of stale values in the `user` namespace.
(I still don't fully understand namespaces.)

You'll need to `require` each module you want to use at the REPL.  See below for modules.

At the REPL you can switch to a different namespace using `in-ns`, e.g.

    > (in-ns 'metaprob.distributions)

By doing this you can see proper `builtin` bindings at the REPL, and
the internals of whatever module you're working on, but personally I
have not found this to be as useful as sticking to `user` and doing a
bunch of `require`s from there.  Maybe I just don't understand how to
use all this stuff.

### Refreshing the state

The `refresh` function reloads your project, giving an alternative
to manually visiting each changed buffer and doing C-c C-k.  Make `refresh`
available at the REPL's `user` namespace with

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

## Reference manual, not

A typical metaprob program `require`s the following:

  * `syntax` - defines macros like `gen`, `block`, `define`
  * `builtin` - defines primitive deterministic procedures like `trace-get`;
       this mainly re-exports procedures defined in `trace` and `builtin-impl`.
       If you're at the REPL you should require `trace` and/or `builtin-impl` since otherwise
       there will be collisions with `clojure.core` (`first`, `rest`, `nth`, and so on).
  * `prelude` - utility procedures like `map` that are written in metaprob ('user mode')
  * `infer` - the score- and trace-conscious interpreter, usually entered 
        via `infer-apply` (also 'user mode').  Also defines `inf`.
  * `distributions` - nondeterministic procedures like `flip` and `uniform` (also 'user mode')

The namespace name needs to be prefixed with `metaprob.`

Note that the `builtin` module has many name conflicts with Clojure so it's
not possible to do `(require '[metaprob.builtin :refer :all])` at the
REPL when in the `user` namespace.  Procedures with conflicting names,
e.g. `first`, are called `metaprob-first` in the `builtin-impl` module, but
simply `first` in `builtin`.

In a metaprob source file, in order to use `builtin`, you need to
suppress the usual clojure bindings, which you do with

    (ns (:refer-clojure :only [declare ns]) ...)

`distributions.clj` shows a typical `ns` form for use in files:

    (ns metaprob.distributions
      (:refer-clojure :only [declare ns])
      (:require [metaprob.syntax :refer :all])
      (:require [metaprob.builtin :refer :all])
      (:require [metaprob.prelude :refer :all])
      (:require [metaprob.infer :refer :all]))

Usually you would also want to add `distributions` to this list.

In lieu of a manual, you might look at
[`src/metaprob/builtin.clj`](src/metaprob/builtin.clj) for a long list
of exports from the `builtin` module, and the source code for the
other modules.

### Plotting histograms

`bin/gnuplot-hist` is a script for displaying a histogram using
gnuplot.  The histogram should be a file with one number per line, and
stored in the `results` directory (as is done by the
`binned-histogram` metaprob function).  E.g.

    bin/gnuplot-hist results/samples_from_the_prior.samples 
    open results/samples_from_the_prior.samples.png

(where `open` is the MacOS command for opening a file using the
appropriate Mac application).

Gnuplot must be installed in order for this to work.

-----

## Parsing metaprob [DEPRECATED]

Currently if you want to use the native "curly" metaprob syntax, you
will have to use the curly-metaprob parser written in Python to write
a file that can be read by Clojure.  This requires installing python
metaprob, which in turn requires Venture.

### Installing python-metaprob

Install metaprob if you want to be able to parse metaprob code expressed in
original metaprob syntax.

 * Clone the metaprob repository to `../metaprob`.
 * Follow instructions in [`../metaprob/README.md`](../metaprob/README.md), including 
   installation of dependencies such as Venture, graphviz, etc).
 * The binary distribution of Venture might be easier to deal with than the source distribution.

Note:

 * Building Venture depends on the packages listed in
   `../Venturecxx/install_requires.txt` .  Typically one would install these with `pip install`.
 * Metaprob will need Plex, but it should be able to get it from a copy
   found in Venture.


### Parse curly-metaprob, generate clojure-metaprob

This is the only part of the system that requires use of the original
Python version of metaprob.

To parse a single metaprob file:

    ../metaprob/pythenv.sh python python/transcribe.py -f {inpath}.vnts >{outpath}.trace

where `{inpath}.vnts` is a file containing metaprob code and
`{outpath}.trace` is where you want to put the file containing a representation 
of the metaprob parse tree (which happens to be a trace).

To parse all of the metaprob files in the repository:

    ./parse-all.sh   

or just

    make parse

This writes to a `parsings` directory which has a directory structure parallel to that of `src`.

## Converting a metaprob parse tree to Clojure [DEPRECATED]

A tiny bit of setup:

    make .lein_classpath

This only has to be done once.

To convert all the metaprob-repository metaprob files from traces (see
`make parse`) to clojure files:

    java -cp `cat .lein_classpath` metaprob.main {inpath}.trace {outpath}.clj

This can also be done at the Clojure read-eval-print loop or in a
Clojure program, something like:

    (in-ns 'metaprob.to_clojure)
    (convert "{inpath}.trace" "{outpath}.clj")

To convert *all* of the metaprob files from the metaprob repository,
do the following, which is analogous to `make parse`:

    make convert


# metaprob-clojure
Metaprob implemented in Clojure

## Install Java

You will of course need Java to run Leiningen and Clojure, e.g. OpenJDK
version 8 or later.  You need full Java, not just the JVM.

## Installing Leiningen and Clojure

It is not necessary to separately install Clojure if Leiningen is
installed.  Just install Leiningen, and let it take care of installing
the right version of Clojure.

Instructions for installing Leiningen are [here](https://leiningen.org/#install), 
but for a quick install use 
the short script in the Makefile: `make lein`.
This script assumes that `~/bin` is on your `PATH`.

Leiningen keeps some state in the `~/.lein` directory.

## Using Clojure under Emacs

There are many ways to do development or other activities in Clojure.
Here is what I do, which is not necessarily right or best:

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

At a shell, in its own terminal window or tab (not necessarily in
emacs), go to the directory that contains project.clj, and do:

    $ lein repl :headless

This takes a few seconds, then prints the TCP port number that you
need to enter in the next step.  In emacs, do:

    M-x cider-connect
    localhost
    <port> control-J

where `<port>` is the port you saw when you did `lein repl :headless`.

You can load a clojure file by visiting it in a buffer and doing C-c
C-k.  I think it will load dependencies as inferred from the `ns` form
at the top.

The `(refresh)` function reloads your project:

    (require '[clojure.tools.namespace.repl :refer [refresh]])

I guess this can be put in project.clj so that it happens every time
you start Clojure?  Need to look into this.

Exploration from the REPL is a little bit annoying due to the
namespace system.  I typically set up namespace prefixes manually, e.g.

    (require '[metaprob.builtin :as builtin])

so that I can say

    (builtin/trace-get x "foo")
    (builtin/pprint x)

and so on.  Alternatively, and more concisely:

    (require '[metaprob.trace :refer :all])
    (require '[metaprob.builtin-impl :refer :all])

    (trace-get x "foo")
    (metaprob-pprint x)

This seems better; I don't know why I don't it; maybe afraid of name collisions.

(Note the `builtin` module has many name conflicts with Clojure so it's not possible
to do `(require '[metaprob.builtin :refer :all])`.)

Often during development, if the namespaces or `deftype` types change
in some incompatible way, I find it necessary to restart clojure (C-c
C-q followed by killing the `lein repl :headless` process).  It may
also be helpful in such situations to remove the `target` directory.

This is a pain in the butt because it can take a minute or so ro kill
any running clojure under emacs, restart the REPL, connect to the new
REPL, and reload the project.

Sometimes I lose patience with this process and work exclusively from
the shell, by writing and debugging tests (in concert with writing and
debugging the main code).  This is nice because all files are freshly
loaded every time.  The downside is an overhead of a couple of seconds
for every time you want to run a test, and slogging through long
backtraces to figure out what went wrong.

## Testing

You can run tests either from the shell or from inside Clojure.  From Clojure:

    (require '[clojure.test :refer :all])

    (run-tests 'metaprob.trace-test)    ;single module

From the shell: tests for all modules in the project:

    lein test

Just one module at a time:

    lein test metaprob.trace-test

Don't forget the `-test` at the end!  I spent a long time being
confused because I hadn't realized it was needed.

Tests are all in the `test/metaprob/` directory.  The tests for `src/metaprob/x.clj` are in 
`test/metaprob/x_test.clj`.

-----

## Parsing metaprob [DEPRECATED]

Currently if you want to use the native metaprob syntax, you will have
to use the "curly-metaprob" parser written in Python to write a file that can
be read by Clojure.  This requires installing metaprob, which in turn
requires Venture.

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

To convert *all* of the metaprob files from the metaprob repository, following `make parse`:

    make convert


# metaprob-clojure
Metaprob implemented in Clojure

## Install Java

You will of course need Java to run Leinigen and Clojure, e.g. OpenJDK
version 8.  You need full Java, not just the VM.

## Installing Leinigen and Clojure

It is not necessary to separately install Clojure if Leinigen is
installed.  Just install Leinigen, and let it take care of installing
Clojure.

Instructions for installing Leinigen are [here](https://leiningen.org/#install), but for a quick install use
the short script in the Makefile: `make lein`.
This script assumes that `~/bin` is on your `PATH`.

Leinigen keeps some state in the `~/.lein` directory.

## Parsing metaprob

Currently if you want to use the native metaprob syntax, you will have to use the
metaprob parser written in python to write a file that can be read by
Clojure.  This requires installing metaprob, which in turn requires
Venture.

### Installing metaprob

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


### Parsing metaprob

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

## Converting a metaprob parse tree to Clojure

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

## Using Clojure

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

At a shell, in its own window or tab (not necessarily in emacs), do:

    $ lein repl :headless

This prints the TCP port to connect to in the next step.  In emacs, do:

    M-x cider-connect
    localhost
    <port> control-J

where `<port>` is the port you saw earlier.

Useful libraries: `clojure.test` and `clojure.tools.namespace.repl`, e.g.

    (require '[clojure.test :refer :all])
    (require '[clojure.tools.namespace.repl :refer [refresh]])

I guess these can be put somewhere so that they happen every time you
start Clojure this way?

Often during development, if the namespaces change in some
incompatible way, I find it necessary to restart clojure (C-c C-q
followed by killing the `lein repl :headless` process).  It is also
helpful in suchg situations to remove the `target` directory.


## Test

There are some tests of the Clojure code.  To run them all:

    lein test

(Apparently the `lein` command automagically compiles clojure to
java, as needed, placing the `.class` files in the `target` directory.)

You can also test individual files:

    lein test metaprob.to-clojure-test


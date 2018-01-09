# metaprob-clojure
Metaprob implemented in Clojure

## Installing Leinigen

It is not necessary to install Clojure if Leinigen is installed.
Usually you would just install Leinigen and let it take care of
getting Clojure.

Instructions for installing Leinigen are [here](https://leiningen.org/#install), but for a quick install use
the short script in the Makefile: `make lein`.
This script assumes that `~/bin` is on your `PATH`.

## Parsing metaprob

Currently if you use the native metaprob syntax you have to use the
metaprob parser written in python and write a file to be read by
clojure.  This requires installing metaprob, which in turn requires
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

    java -cp `cat .lein_classpath` dontknow.main {inpath}.trace {outpath}.clj

This can also be done at the Clojure read-eval-print loop, something like:

    (in-ns 'dontknow.to_clojure)
    (convert "{inpath}.clj" "{outpath}.clj")

## Test

There are some tests of the Clojure code.  To run them all:

    lein test

(Apparently the `lein` command automagically compiles clojure to
java, as needed, placing the `.class` files in the `target` directory.)
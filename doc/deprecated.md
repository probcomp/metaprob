# Interfacing with metaprob-in-python


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


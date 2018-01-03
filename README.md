# metaprob-clojure
Metaprob implemented in Clojure

## Install Leinigen

Instructions [here](https://leiningen.org/#install).

A short script for installin `lein` is available as `make lein`.

## Parsing metaprob

### Install metaprob

Install metaprob if you want to be able to parse metaprob code expressed in
original metaprob syntax.

 * Clone to ../metaprob
 * Follow instructions in [../metaprob/README.md](../metaprob/README.md) (install venture, graphviz, etc)

### Parse a single metaprob file

`../metaprob/pythenv.sh python python/transcribe.py -f` _inpath_`.vnts` `>`_outpath_`.trace`

### Parse all the metaprob files in the repository

`./parse-all.sh`

This writes to a `parsings` directory which has a directory structure parallel to that of `src`.

## Test

`lein test`

It seems that the `lein` command automagically compiles clojure to
java, as needed, placing the `.class` files in the `target` directory.
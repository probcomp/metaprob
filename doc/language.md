# Metaprob language reference manual

Well not really, but here are some notes.

Metaprob is really just Clojure with some macros and functions.  
You'll need some familiarity with Clojure in
order to use Metaprob.  Introducing Clojure is beyond the current
scope, so find some independent way to get started with Clojure.

You may want to create a project (a set of files depending on metaprob
and on one another).  This is done with `lein` which has its own
documentation.  (There are other project tools besides `lein`.  I talk
about `lein` only because it's the only one I'm familiar with.)


## The Clojure namespaces that implement Metaprob

Metaprob is provided as a set of Clojure namespaces that can
be used in the usual clojure way, with `require` and so on.

A typical Metaprob program `require`s the following namespaces:
(all namespace names begin with `metaprob.` , which I'll omit for readability)

  * `generative-functions` - provides functions for creating and using generative functions, and also the `gen` macro.
  * `prelude` - exposes utility procedures like eager versions of `map` and `replicate`
  * `trace` - implements the trace datatype and its methods
  * `distributions` - implements nondeterministic primitives like `flip` and `uniform` as generative functions
  * `inference` - provides a standard library for inference programming

In Clojure code, you can import these namespaces with `:as` or
`:refer` with an explicit list of names, but these namespaces provide
all the basic language primitives and it is a pain to write namespace
prefixes for them all the time in Metaprob source code, where they are
uniquitous.  Therefore, in a Metaprob source file, you generally
import the namespaces with `:refer :all`.  Because some names in
`prelude` conflict with the usual clojure bindings, you
need to suppress `clojure.core, which you do with

    (ns (:refer-clojure :exclude [map replicate apply]) ...)

So a typical Metaprob source file might start like this:

    (ns thisand.that
      (:refer-clojure :exclude [map replicate apply])
      (:require [metaprob.generative-functions :refer :all])
      (:require [metaprob.trace :refer :all])
      (:require [metaprob.prelude :refer :all])
      (:require [metaprob.inference :refer :all])
      (:require [metaprob.distributions :refer :all]))

## `generative-functions` namespace

([Source](../src/metaprob/generative_functions.clj))

* `(make-generative-function run-in-clj make-constrained-generator)` returns a custom generative function.
* `(gen {:annotation value, ...} [formal ...] body)` returns a generative function based on generative code.
* `(make-primitive sampler scorer)` returns a generative  function implementing a primitive distribution.
* `(make-constrained-generator f obs)` returns a constrained version of `f` given an observation trace.

### Addresses

A child of a trace is given by a key; each child of a trace has a
different key.  An 'address' is either a single key or a sequence of
keys specifying, for a given trace t, the descendent of t obtained by
following the path given by the key sequence.  An address that is a
key or a singleton specifies that child of a given trace, while longer
addresses specify deeper descendants, and an empty address specifies
the trace itself.

### Traces

In the following, 'A' is an address.

* `(trace-value t)`  - get the value at t
* `(trace-value t A)`  - get the value of the A subtrace of t
* `(trace-has-value? t)`  - is there a value at t?
* `(trace-has-value? t A)`  - does t's A subtrace have a value?
* `(trace-subtrace t A)`  - get the subtrace of t at A
* `(trace-has-subtrace? t A)` - does t have a subtrace at A?
* `(trace? x)`  - true if x is a trace

* `(trace-set-value t A x)` - return a new trace that's the same as t except at address A, where one will find the value x
* `(trace-set-subtrace t A u)` - a trace like t, in which u is at address A.

* `(trace-keys t)`  - list of keys for children
* `(addresses-of t)`  - the list of addresses, relative to t, of every descendant subtrace that has a value

* `(trace-merge t1 t2)`  - return a trace that is the merge of t1 and t2 (union of respective children, recursively)
* `(trace-clear t A)`  - remove the value at A
* `(trace-clear-subtrace t A)` - remove the subtrace at A
* `(merge-subtrace t A u)` - replace t's A subtrace with the result of merging it with u
* `(partition-trace t As)` - if As is a list of addresses, return a pair of traces `[u v]` where `u` contains all of 
the addresses in As, and `v` contains the others, such that merging `u` and `v` recovers `t`.

### Output

* `binned-histogram` - many options - writes out files that can be
  displayed using the `bin/gnuplot-hist` script.  A set of samples and a
  control file are written to the `results` directory (which must be
  created manually).

`bin/gnuplot-hist` is a shell script that uses gnuplot to create a
.png file from a samples file written by `binned-histogram`.  E.g.

    bin/gnuplot-hist results/samples_from_the_prior.samples 
    open results/samples_from_the_prior.samples.png

(where `open` is the MacOS command for opening a file using the
appropriate Mac application).

This is pretty much of a kludge right now.

Gnuplot must be installed in order for this to work.

## `prelude` namespace

([Source](../src/metaprob/prelude.clj))

documentation TBD

## `distributions` namespace

[Source](../src/metaprob/distributions.clj)

* `(flip w)`  - returns true with probability w, false otherwise
* `(uniform a b)`  - floating point number drawn from [a,b]
* `(uniform-discrete items)`  - one of the members of items (a list)
* `(categorical probabilities)`  - returns 0, 1, ... with probability of i proportional to probabilities[i]
* `(log-categorical scores)`  - returns 0, 1, ... with probability of i proportional to exp(score[i])
* `(gaussian mu sigma)` - samples from a normal distribution
* `(beta a b)` - samples from a beta distribution
* `(gamma shape scale)` - samples from a gamma distribution

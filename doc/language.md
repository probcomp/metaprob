# Metaprob reference manual

Well not really but here are some notes.

You'll need some familiarity with Clojure in order to get going with
metaprob.  That's beyond the current scope.

Metaprob is provided as a set of Clojure namespaces that can
be used in the usual clojure way, with `require` and so on.

A typical metaprob program `require`s the following namespaces:
(all namespace names begin with `metaprob.` , which I'll omit for readability)

  * `syntax` - structual macros like `gen`, `block`, `define`
  * `builtin` - primitive deterministic procedures like `trace-get`;
       this mainly re-exports procedures defined in `trace` and `builtin-impl`.
       If you're at the REPL you should require `trace` and/or `builtin-impl` since otherwise
       there will be collisions with `clojure.core` (`first`, `rest`, `nth`, and so on).
  * `prelude` - utility procedures like `map` that are written in metaprob ('user mode')
  * `infer` - the score- and trace-conscious interpreter, usually entered 
        via `infer-apply` (also 'user mode').  Also defines `inf`.
  * `distributions` - nondeterministic procedures like `flip` and `uniform` (also 'user mode')

In Clojure code, you can import these namespaces with `:as` or
`:refer` with an explicit list of names, but these namespaces provide
all the basic language primitives and it is a pain to write namespace
prefixes for them all the time in metaprob source code, where they are
uniquitous.  Therefore, in a metaprob source file, you generally
import the namespaces with `:refer :all`.  Because some names in
`builtin` and `syntax` conflict with the usual clojure bindings, you
need to suppress `clojure.core, which you do with

    (ns (:refer-clojure :only [declare ns]) ...)

So a typical Metaprob source file might start like this:

    (ns thisand.that
      (:refer-clojure :only [declare ns])
      (:require [metaprob.syntax :refer :all])
      (:require [metaprob.builtin :refer :all])
      (:require [metaprob.prelude :refer :all])
      (:require [metaprob.infer :refer :all])
      (:require [metaprob.distributions :refer :all]))

## `syntax` namespace

* `(get [x y] (add x y))` returns a generative procedure.
  Deterministic procedures are a special case, but in general a
  generative procedure will sample from some distribution.

## `builtin` namespace

The `builtin` module has many name conflicts with Clojure so it's not
ordinarily possible to do `(require '[metaprob.builtin :refer :all])`,
in particular at the REPL when in the `user` namespace.

In lieu of a manual, you might look at
[`../src/metaprob/builtin.clj`](src/metaprob/builtin.clj) for a long list
of exports from the `builtin` module, and the source code for the
other modules.

### Traces

The central facility is the so-called "trace" data type.  A trace is a
tree where each node has a labeled set of children that are all
traces, and an optional value.

A trace can be either mutable or immutable.  Most primitives that
create traces return mutable traces.

The following correspond to figure 7 in the July 2017 manuscript:

* `(empty-trace)` - create a new mutable trace, initially empty
* `(trace-set t "a" x)` - store x as the value at t's "a" child
* `(trace-set t (addr "a" "b") x)` - store x as the value at "b" child of "a" child of t.
  Repeated assignments overwrite.
* `(trace-subtrace t "a")`  - get the "a" subtrace (child trace) of t.
  t has a reference to the subtrace, not a copy, so changes to the subtrace are visible via t.
* `(trace-get t)`  - get the value at t
* `(trace-has? t)`  - is there a value at t?
* `(addresses-of t)`  - list of addresses, relative to t, that have values
* `(trace :value 1 "z" 2 "a" (** subtrace) "c" (** (trace "d" 8)))` - construct a trace.  See figure 7

Lists and tuples are special cases of traces.  A nonempty list is a
trace with a value and a `"rest"` child.  A tuple is a trace with no
value and children labeled 0, 1, ....  There are many procedures for
working with these, documentation TBD.

Immutable traces are represented as their natural Clojure data types:
immutable lists are sequences, immutable tuples are vectors, and other
immutable traces are maps.  There are a few procedures for controlling
mutability, documentation TBD.

### Opaque procedures

As a performance feature, and to reduce clutter in output traces, you
can prevent the interpretation of a procedure by applying a special
operator called `opaque`, which looks superficially like an identity
function.

* `(opaque name p)` - returns a procedure that acts the same as p when
  called outside the metaprob interpreter, and acts like a 'compiled'
  procedure when called within the metaprob interpreter.  That means
  its body won't be subject to intervention, scoring, output, etc.

name is just a comment and can be nil.

### Plotting histograms

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

Useful "user mode" functions like `map`.
[Source](../src/metaprob/prelude.clj)

## `infer` namespace - the metaprob interpreter

The Metaprob interpreter manages scoring, intervention, and
output tracing.
[Source](../src/metaprob/infer.clj)

* `(infer-apply proc inputs intervene target output)`  -
     apply proc to inputs using the metaprob interpreter, returning [answer score],
     respecting interventions, and recording to output.  Any or all of the three 
     traces may be nil meaning do not deal with that trace.
* `(inf name proc)`  - promote an ordinary (generative) procedure to an 
  inference procedure.  name is a comment and can be nil.

## `distributions` namespace

* `(flip w)`  - returns true with probability w, false otherwise
* `(uniform a b)`  - floating point number drawn from [a,b]
* `(uniform-sample items)`  - one of the members of items (a list)
* `(log-categorical scores)`  - returns 0, 1, ... with probabilities proportional to scores

[Source](../src/metaprob/distributions.clj)

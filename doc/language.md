# Metaprob reference manual

Well not really but here are some notes.

You'll need some familiarity with Clojure in order to get going with
Metaprob.  That's beyond the current scope.

## Metaprob values

A Metaprob value is either a scalar or a trace.  A scalar is one of:
  * a boolean
  * a number
  * a string
  * a Clojure keyword
  * nil
  * a 'foreign procedure'

All Metaprob-native compound data is represented using the so-called
"trace" data type.  A trace is a tree structure in which each node has
a labeled set of children that are all traces.  Labels are strings or
numbers.  In addition to, or instead of, children, a node may also
have a value.

A trace can be either mutable or immutable.  Most primitives that
create traces return mutable traces.

Trees should not contain cycles, although this is not enforced.

See below for operations on traces.


## The Clojure namespaces that implement Metaprob

Metaprob is provided as a set of Clojure namespaces that can
be used in the usual clojure way, with `require` and so on.

A typical Metaprob program `require`s the following namespaces:
(all namespace names begin with `metaprob.` , which I'll omit for readability)

  * `syntax` - structual macros like `gen`, `block`, `define`
  * `builtin` - primitive deterministic procedures like `trace-get`;
       this mainly re-exports procedures defined in `trace` and `builtin-impl`.
       If you're at the REPL you should require `trace` and/or `builtin-impl` since otherwise
       there will be collisions with `clojure.core` (`first`, `rest`, `nth`, and so on).
  * `prelude` - utility procedures like `map` that are written in Metaprob ('user mode')
  * `infer` - the score- and trace-conscious interpreter, usually entered 
        via `infer-apply` (also 'user mode').  Also defines `inf`.
  * `distributions` - nondeterministic procedures like `flip` and `uniform` (also 'user mode')

In Clojure code, you can import these namespaces with `:as` or
`:refer` with an explicit list of names, but these namespaces provide
all the basic language primitives and it is a pain to write namespace
prefixes for them all the time in Metaprob source code, where they are
uniquitous.  Therefore, in a Metaprob source file, you generally
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

([Source](../src/metaprob/syntax.clj))

* `(gen [formal ...] body)` returns a generative procedure.
  Deterministic procedures are a special case, but in general a
  generative procedure will sample from some distribution.
  If body is multiple forms, a `block` is added to wrap them.
* `(block f0 f1 ...)`  - evaluates forms f0 ... in sequence; a form 
  can be a `define` form in which case the lexical environment is extended
* `(define x v)`  - for use with `block` or at top level
* `and`, `or`, `case` - as in Clojure

## `builtin` namespace

([Source](../src/metaprob/builtin.clj))

The `builtin` module has many name conflicts with Clojure so it's not
ordinarily possible to do `(require '[metaprob.builtin :refer :all])`,
in particular at the REPL when in the `user` namespace.

In lieu of a manual, you might look at
[`../src/metaprob/builtin.clj`](src/metaprob/builtin.clj) for a long list
of exports from the `builtin` module, and the source code for the
other modules.

### Traces

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

An address (or path) is a sequence of keys giving a route from the
root of a trace to some child or descendant subtrace.  An address may
be either a single key or a list of keys.

* `(addr k0 k1 ...)`  - an address
* `(addresses-of t)`  - addresses of all subtraces of t that have values

There are a number of additional operations on traces, including:

* `(lookup t a)`  - subtrace of t at address a
* `(trace-delete t)`
* `(trace-keys t)`  - list of keys for children
* `(trace-subtrace t a)`   - child or other descendant trace at address a (which may be a key)
* `(trace-has-subtrace? t a)`

Lists and tuples are special cases of traces.  A nonempty list is a
trace with a value and a `"rest"` child that is a list.  A tuple is a
trace with no value and simple child traces labeled 0, 1, ....  (By
'simple' I mean a trace that has a value and no children.)  There are
many procedures for working with these.

* `(list x0 x1 ...)`
* `(tuple x0 x1 ...)` or `[x0 x1 ...]`
* `(nth s n)`  - nth element of sequence s, 0-based
* `(length s)`  - length of a sequence s
* `(list? x)`
* `(tuple? x)`
* `(to-list s)`  - convert a tuple to a list (lists are returned directly)
* `(to-tuple s)` - convert a list to a tuple (tuples are returned directly)
* `(append s1 s2)`  - concatenation of two sequences
* `(set-difference s1 s2)`
* `(range n)`

A nonempty list is called a _pair_:

* `(pair x y)` - returns a pair.  x is any value; y must be either a pair or the empty list `(list)`
* `(first z)`  - first element of a pair
* `(rest z)`   - second element of a pair
* `(pair? w)`  - true iff w is a pair
* `(last z)`  - last element of nonempty list z

Immutable traces are represented as their natural Clojure data types:
immutable lists are sequences, immutable tuples are vectors, and other
immutable traces are maps.  There are a few procedures for controlling
mutability:

* `(make-mutable t)`  - if t is immutable, returns a nonrecursive mutable copy of t, otherwise returns t
* `(make-immutable t)`  - if t is mutable, returns a nonrecursive immutable copy of t, otherwise returns t

### Procedures

The 'meta' in 'metaprob' comes from the ability to write Metaprob
programs that control probabilistic inference.

Metaprob has three basic kinds of procedure:

1. 'Native' generative procedure.  These are procedures that can be
   processed by the meta-circular interpreter (see `infer` namespace).
   Native generative procedures are the values of `gen` expressions.
1. 'Foreign' generative procedure.  These are procedures, usually
   written in Clojure, that are simply invoked to get a value, with
   no reference to interpretation.
1. Special inference procedures.  These are procedure that implement
   specialized inference methods, such as score computation for 
   distributions.
   Special inference procedures are created using the `inf` combinator.

* `(inf name p)` - promotes a generative procedure p to a procedure
  performing inference according to p.  p must be a procedure of four
  arguments: input, intervention trace, target trace, output trace,
  and it must return two values: a nominal value and a score.  When
  the procedure q = `(inf name p)` is called, say `(q x y)`, p is
  applied to the input sequence (x y) and the traces that the
  interpreter is maintaining in the interpretation of the call to q.
  The nominal value becomes the result of the call to q, and the
  accompanying score is used by the interpreter
* `(opaque name p)` - returns a procedure that acts the same as p when
  called outside the Metaprob interpreter, and acts like a 'compiled'
  procedure when called within the Metaprob interpreter.  That means
  its body won't be subject to intervention, scoring, output, etc.
  This is a performance feature, and also reduces clutter in output
  traces.  name is just a comment, and can be nil.
* `(trace-as-procedure t w)` - this is an internal utility used 
  in only a few places, documented here for completeness, not because 
  we expect new uses of it.  t should be a trace, and w should be a
  procedure.  This returns a procedure, say p.  When p is called using
  `infer-apply` or from within the interpreter, the interpreter
  obtains information needed to invoke p from properties of t.
  When p is called directly from Clojure, the procedure w is invoked.

### Output

* `(print x)`
* `(pprint x)`  - useful for displaying traces
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

Useful "user mode" functions like `map`, documentation TBD

## `infer` namespace - the metaprob interpreter

([Source](../src/metaprob/infer.clj))

The Metaprob interpreter manages scoring, intervention, and
output tracing.

* `(infer-apply proc inputs intervene target output)`  -
     apply proc to inputs using the Metaprob interpreter, returning [answer score],
     respecting interventions, and recording to output.  Any or all of the three 
     traces may be nil meaning do not deal with that trace.
* `(inf name proc)`  - promote an ordinary (generative) procedure to an 
  inference procedure.  name is a comment and can be nil.

## `distributions` namespace

[Source](../src/metaprob/distributions.clj)

* `(flip w)`  - returns true with probability w, false otherwise
* `(uniform a b)`  - floating point number drawn from [a,b]
* `(uniform-sample items)`  - one of the members of items (a list)
* `(log-categorical scores)`  - returns 0, 1, ... with probabilities proportional to scores


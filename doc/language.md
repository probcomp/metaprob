# Metaprob-in-Clojure language reference manual

Well not really, but here are some notes.

From here on I will simply say 'Metaprob' instead of 'Metaprob-in-Clojure'.

Metaprob is really just Clojure with some macros and functions and
some namespace trickery.  You'll need some familiarity with Clojure in
order to use Metaprob.  Introducing Clojure is beyond the current
scope, so find some independent way to get started with Clojure.

You may want to create a project (a set of files depending on metaprob
and on one another).  This is done with `lein` which has its own
documentation.  (There are other project tools besides `lein`.  I talk
about `lein` only because it's the only one I'm familiar with.)


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
a labeled set of children, and all the children are traces.  Labels
are strings or numbers.  In addition to, or instead of, children, a
node may also have a value.

A trace can be either mutable or immutable.  The language is in
transition between using mostly mutable traces to using mostly
immutable traces.  Most operators are generic and work with either
mutable or immutable traces, but you are likely to encounter some
inconsistencies.

A change to a mutable subtrace of a trace t will be visible via t,
since structure is shared between traces.

Traces should not contain cycles, i.e. a trace should not be a
descendant of itself, although this is not enforced.

See below for operations on traces.

A 'foreign procedure' or 'foreign generative procedure' is just a
'real' Clojure function, i.e. one that is not a seq, vector, map, etc.
Through use of the Clojure `meta` operator, some functions are also
made to look like traces.

Also see below for operators on procedures.


## The Clojure namespaces that implement Metaprob

Metaprob is provided as a set of Clojure namespaces that can
be used in the usual clojure way, with `require` and so on.

A typical Metaprob program `require`s the following namespaces:
(all namespace names begin with `metaprob.` , which I'll omit for readability)

  * `syntax` - structual macros like `gen`, `block`, `define`
  * `builtin` - primitive deterministic procedures like `trace-get`;
       this mainly re-exports procedures defined in `trace` and
       `builtin-impl`.
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

### Addresses

A child of a trace is given by a key; each child of a trace has a
different key.  An 'address' is either a single key or a sequence of
keys specifying, for a given trace t, the descendent of t obtained by
following the path given by the key sequence.  An address that is a
key or a singleton specifies that child of a given trace, while longer
addresses specify deeper descendants, and an empty address specifies
the trace itself.

The following syntax is used for address formation:

* `(addr k0 k1 ...)`  - an address

### Traces

In the following, 'A' is an address.

* `(trace-get t)`  - get the value at t
* `(trace-get t A)`  - get the value of the A subtrace of t
* `(trace-has? t)`  - is there a value at t?
* `(trace-has? t A)`  - does t's A subtrace have a value?
* `(trace-subtrace t A)`  - get the subtrace of t at A
* `(trace-has-subtrace? t A)`
* `(trace? x)`  - true if x is a trace

* `(trace-set t A x)` - return a new trace that's the same as t except at address A, where one will find the value x
* `(trace-set! t A x)` - store value x at address A under t.  t must be a mutable trace.
* `(trace-set-subtrace t A u)` - a trace like t, in which u is at address A.
* `(trace-set-subtrace! t A u)` - store trace u at address A under t.  u becomes a descendant of t.  t must be a mutable trace.
* `(empty-trace)` - create a new mutable trace, initially empty (use `(trace)` to get an immutable empty trace)
* `(trace :value 1 "z" 2 "a" (** subtrace) "c" (** (trace "d" 8)))` - construct an immutable trace.  The `**` marker indicates that something is to be made a subtrace, not the value of the subtrace.
* `(mutable-trace)` - new mutable trace, initially empty
* `(mutable-trace ...)` - like `(trace ...)` but returns a mutable trace

* `(trace-keys t)`  - list of keys for children
* `(trace-count t)`  - number of direct children
* `(addresses-of t)`  - the list of addresses, relative to t, of every descendant subtrace that has a value

* `(trace-merge t1 t2)`  - return a trace that is the merge of t1 and t2 (union of respective children, recursively)
* `(trace-delete! t A)`  - t must be mutable - remove the value at A
* `(lookup t a)`  - subtrace of t at address a - deprecated

### Lists and tuples

Lists and tuples (collectively: sequences, not to be confused with
Clojure seqs) are special cases of traces.  A nonempty list is a trace
with a value and a `"rest"` child that is a list.  A tuple is a trace
with no value and simple child traces labeled 0, 1, ....  (By 'simple'
I mean a trace that has a value and no children.)  There are many
procedures for working with these.

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

* `(to-mutable t)`  - if t is immutable, returns a nonrecursive mutable copy of t, otherwise returns t
* `(to-immutable t)`  - if t is mutable, returns a nonrecursive immutable copy of t, otherwise returns t

### Procedures

The 'meta' in 'metaprob' comes from the ability to write Metaprob
programs that control probabilistic inference.

Metaprob has three basic kinds of procedure:

1. 'Native' generative procedure.  These are procedures that are
   written in Metaprob and can therefore be
   processed by the meta-circular interpreter (see `infer` namespace).
   Native generative procedures are the values of `gen` expressions.
1. 'Foreign' generative procedure.  These are procedures, usually
   written in Clojure, that are simply invoked to get a value, with
   no reference to interpretation.
1. Special inference procedures.  These are procedure that implement
   specialized inference methods, such as score computation for
   distributions.
   Special inference procedures are created using the `inf` combinator.

* `(infer-apply p arg ...)` - after a dispatch, this typically invokes
  the interpreter (or a configured interpreter, if there is more than one).
* `(inf name p)` - promotes a generative procedure p to a procedure
  performing inference according to p.  p must be a procedure of four
  arguments: input, intervention trace, target trace, output trace,
  and it must return two values: a nominal value and a score.  When
  the procedure q = `(inf name p)` is called, say `(q x y)`, p is
  applied to the input sequence (x y) and the traces that the
  interpreter is maintaining in the interpretation of the call to q.
  The nominal value becomes the result of the call to q, and the
  accompanying score is used by the interpreter
  - note jmt, `inf` seems to have changed signature to be 3-arity:
    `(defn inf [name model implementation] ...)` where
    `implementation` is a procedure
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
* `(categorical probabilities)`  - returns 0, 1, ... with probability of i equal to probabilities[i]
* `(log-categorical scores)`  - returns 0, 1, ... with probability of i proportional to exp(score[i])

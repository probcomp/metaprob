# Metaprob [![Build Status](https://travis-ci.org/probcomp/metaprob.svg?branch=master)](https://travis-ci.org/probcomp/metaprob)

A language for probabilistic programming and metaprogramming, embedded in [Clojure](https://clojure.org/).

Metaprob is currently alpha software, and everything about it is subject to change.

## Key features

* Models can be represented via generative code, i.e. ordinary code that makes stochastic choices
* Models can also be represented via approximations, e.g. importance samplers with nontrivial weights
* Custom inference algorithms can be written in user-space code, via reflective language constructs for:
  * tracing program executions
  * using partial traces to specify interventions and constraints
* Generic inference algorithms are provided via user-space code in a standard library; adding new algorithms does not require modifying the language implementation
* All Inference algorithms are ordinary generative code and can be traced and treated as models
* New probability distributions and inference algorithms are first-class citizens that can be created dynamically during program execution

## Motivations

* Lightweight embeddings of probabilistic programming and inference metaprogramming
  * Interactive, browser-based data analysis tools (via [ClojureScript](https://clojurescript.org/))
  * Smart data pipelines suitable for enterprise deployment (via Clojure on the JVM)
* “Small core” language potentially suitable for formal specification and verification
* Teaching
  * Undergraduates and graduate students interested in implementing their own minimal PPL
  * Software engineers and data engineers interested in probabilistic modeling and inference
* Research in artificial intelligence and cognitive science
  * Combining symbolic and probabilistic reasoning, e.g. via integration with Clojure’s [core.logic](https://github.com/clojure/core.logic)
  * “Theory of mind” models, where an agent’s reasoning is modeled as an inference metaprogram acting on a generative model
  * Reinforcement learning and other “nested” applications of modeling and approximate inference
  * Causal reasoning, via a notion of interventions that extends Pearl's “do” operator
* Research in probabilistic meta-programming, e.g. synthesis, reflection, runtime code generation

## Modeling and tracing

Generative models are represented as ordinary functions that make stochastic choices.

```clojure
;; Flip a fair coin n times
(define fair-coin-model
 (gen [n]
   (replicate n
     (gen [] (flip 0.5))))))
;; Flip a possibly weighted coin n times
(define biased-coin-model
 (gen [n]
   (define p (uniform 0 1))
   (replicate n (gen [] (flip p)))))
;; The coin “tries” to ensure a balanced sequence
(define gamblers-fallacy-model
 (gen [n]
   (define add-flip
     (gen [so-far]
       (cons (flip
         (+ 0.02 (* 0.96 (- 1 (avg so-far))))) so-far)))
   ((compose-n add-flip (- n 1)) (list (flip 0.5)))))
```

Execution traces of models, which record the random choices they make, are first-class values that inference algorithms can manipulate.

We obtain scored traces using `infer`, which invokes a “tracing interpreter” that is itself a Metaprob program.

```clojure
(define [val trace score] (infer :procedure fair-coin-model, :inputs [3]))
```

## User-space generic inference algorithms

Generic inference algorithms like rejection sampling, sampling/importance resampling, and single-site Metropolis Hastings can be implemented as short, user-space higher-order functions in Metaprob. (They can even be viewed as—and are indistinguishable from—models, for example in theory-of-mind applications.)

```clojure
;; Rejection sampling with an arbitrary predicate on the trace.
(define rejection-sample
 (gen [model predicate]
   ;; generate an execution trace using infer
   (define [_ t _] (infer :procedure model))
   ;; try again if we don't satisfy the predicate
   (if (predicate t) t (rejection-sample model predicate))))

;; Sampling/importance resampling, with the condition specified as a partial
;; execution trace
(define importance-resample
 (gen [model condition n-particles]
   (define get-weighted-sample
     (gen []
     (define [_ t score]
      (infer :procedure model, :target-trace condition)
      [t score])))
(define particles (replicate n-particles get-weighted-sample))
(nth (map first particles) (log-categorical (map second particles)))))
```

## Documentation

  * [Installation instructions](INSTALL.md)
  * [Using metaprob-in-clojure](doc/interaction.md)
  * [Language reference](doc/language.md)
  * [Probabilistic inference examples](doc/examples.md)

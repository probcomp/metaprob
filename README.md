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
(def fair-coin-model
 (gen {:tracing-with t} [n]
   (map (fn [i] (t i flip [0.5])) (range n))))
;; Flip a possibly weighted coin n times
(def biased-coin-model
 (gen {:tracing-with t} [n]
   (let [p (t "p" uniform [0 1]))]
     (map (fn [i] (t i flip [p])) (range n)))))
```

Execution traces of models, which record the random choices they make, are first-class values that inference algorithms can manipulate.

We obtain scored traces using `infer-and-score`, which invokes a “tracing interpreter” that is itself a Metaprob program.

```clojure
(infer-and-score :procedure fair-coin-model, :inputs [3])
```

## Documentation

  * [Contributor installation instructions](INSTALL.md)
  * [Using Metaprob](doc/interaction.md)
  * [Language reference](doc/language.md)

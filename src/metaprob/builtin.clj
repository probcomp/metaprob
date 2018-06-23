;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin
  (:refer-clojure :exclude
                  [newline not
                   and or case cond
                   assert print apply
                   list list? first rest last nth range sort])
  (:require [metaprob.trace :as trace])
  (:require [metaprob.sequence :as sequence])
  (:require [metaprob.builtin-impl :as impl]))

(defmacro define-foreign-procedure [mp-name generate-fn]
  (let [namestring (if (symbol? mp-name) (str mp-name) mp-name)]
    `(do (declare ~mp-name)
         (def ~mp-name
           (impl/make-foreign-procedure ~namestring
                                        ~generate-fn)))))

;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

;; General
(define-foreign-procedure eq =)
(define-foreign-procedure neq impl/neq)
(define-foreign-procedure assert impl/metaprob-assert)
(define-foreign-procedure error impl/error)
(define-foreign-procedure generate-foreign impl/generate-foreign)
(define-foreign-procedure procedure-name impl/procedure-name)
(define-foreign-procedure trace-name impl/trace-name)
(define-foreign-procedure trace-as-procedure trace/trace-as-procedure)

;; Logical
(define-foreign-procedure not clojure.core/not)

;; Numeric
(define-foreign-procedure gt >)
(define-foreign-procedure gte >=)
(define-foreign-procedure lte <=)
(define-foreign-procedure lt <)
(define-foreign-procedure add impl/add)
(define-foreign-procedure sub -)
(define-foreign-procedure mul *)
(define-foreign-procedure div /)
(define-foreign-procedure log impl/log)
(define-foreign-procedure cos impl/cos)
(define-foreign-procedure sin impl/sin)
(define-foreign-procedure log1p impl/log1p)
(define-foreign-procedure exp impl/exp)
(define-foreign-procedure sqrt impl/sqrt)
(define-foreign-procedure normal impl/normal)
(define-foreign-procedure floor impl/floor)
(define-foreign-procedure round impl/round)

;; Sample from uniform distribution, with RNG as hidden state
(define-foreign-procedure sample-uniform impl/sample-uniform)

;; Traces
(define-foreign-procedure ok-key? trace/ok-key?)
(define-foreign-procedure empty-trace trace/empty-trace)
(define-foreign-procedure empty-trace? trace/empty-trace?)
(define-foreign-procedure trace-has? trace/trace-has?)
(define-foreign-procedure trace-get trace/trace-get)
(define-foreign-procedure lookup trace/lookup)
(define-foreign-procedure trace-subtrace trace/trace-subtrace)
(define-foreign-procedure trace-keys trace/trace-keys)
(define-foreign-procedure trace-count trace/trace-count)
(define-foreign-procedure trace? trace/trace?)
(define-foreign-procedure trace-has-subtrace? trace/trace-has-subtrace?)
(define-foreign-procedure trace-subtrace trace/trace-subtrace)
(define-foreign-procedure trace trace/trace)    ;constructor
(define-foreign-procedure mutable-trace trace/mutable-trace)    ;constructor
(define-foreign-procedure immutable-trace trace/immutable-trace)    ;constructor
(define-foreign-procedure trace-copy trace/trace-copy)
(define-foreign-procedure to-mutable trace/to-mutable)
(define-foreign-procedure to-immutable trace/to-immutable)
(define-foreign-procedure mutable-trace? trace/mutable-trace?)
(define-foreign-procedure immutable-trace? trace/immutable-trace?)
(define-foreign-procedure ** trace/**)          ;for (trace ... (** ...) ...)

(define-foreign-procedure trace-set! trace/trace-set!)
(define-foreign-procedure trace-delete! trace/trace-delete!)
(define-foreign-procedure trace-set-subtrace! trace/trace-set-subtrace!)
(define-foreign-procedure trace-update! trace/trace-update!)

(define-foreign-procedure trace-update trace/trace-update)
(define-foreign-procedure trace-set trace/trace-set)
(define-foreign-procedure trace-set-subtrace trace/trace-set-subtrace)
(define-foreign-procedure trace-copy trace/trace-copy)

(define-foreign-procedure addresses-of impl/addresses-of)
(define-foreign-procedure addr impl/addr)

;; Lists
(define-foreign-procedure pair sequence/pair)
(define-foreign-procedure pair? sequence/metaprob-pair?)
(define-foreign-procedure first sequence/metaprob-first)
(define-foreign-procedure rest sequence/metaprob-rest)
(define-foreign-procedure list sequence/metaprob-list)
(define-foreign-procedure list? sequence/metaprob-list?)
(define-foreign-procedure last sequence/metaprob-last)

;; Tuples
(define-foreign-procedure tuple sequence/tuple)
(define-foreign-procedure tuple? sequence/metaprob-tuple?)

;; Sequences
(define-foreign-procedure length sequence/length)
(define-foreign-procedure sequence-to-seq sequence/sequence-to-seq)
(define-foreign-procedure to-immutable-list sequence/sequence-to-seq)
(define-foreign-procedure nth sequence/metaprob-nth)
(define-foreign-procedure range sequence/metaprob-range)
(define-foreign-procedure append sequence/append)
(define-foreign-procedure set-difference sequence/set-difference)
(define-foreign-procedure sort sequence/metaprob-sort)
(define-foreign-procedure to-list sequence/to-list)
(define-foreign-procedure to-tuple sequence/to-tuple)

;; Environments
(define-foreign-procedure top-level-lookup impl/top-level-lookup)
(define-foreign-procedure top-level-environment? trace/top-level-environment?)

;; Printing
(define-foreign-procedure print impl/metaprob-print)
(define-foreign-procedure newline trace/metaprob-newline)
(define-foreign-procedure pprint trace/metaprob-pprint)
(define-foreign-procedure binned-histogram impl/binned-histogram)

;; -----------------------------------------------------------------------------
;; Work in progress

(define-foreign-procedure generate-foreign impl/generate-foreign)
(define-foreign-procedure make-foreign-procedure impl/make-foreign-procedure)
(define-foreign-procedure foreign-procedure? trace/foreign-procedure?)

(def positive-infinity Double/POSITIVE_INFINITY)
(def negative-infinity Double/NEGATIVE_INFINITY)
(define-foreign-procedure same-trace-states? trace/same-trace-states?)

(defmacro and [& forms] `(clojure.core/and ~@forms))
(defmacro or [& forms] `(clojure.core/or ~@forms))
(defmacro cond [& forms] `(clojure.core/cond ~@forms))
(defmacro case [& forms] `(clojure.core/case ~@forms))

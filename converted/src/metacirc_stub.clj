;; This file was automatically generated

(ns metaprob.src.metacirc-stub
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare interpret trace_choices propose)

(define
  interpret
  (program
    [sp args intervene]
    (define [v _] (py_propose sp args intervene (mk_nil) (mk_nil)))
    v))

(define
  trace_choices
  (program
    [sp args intervene output]
    (define [v _] (py_propose sp args intervene (mk_nil) output))
    v))

(define propose_and_trace_choices py_propose)

(define
  propose
  (program
    [sp args intervene target]
    (py_propose sp args intervene target (mk_nil))))


;; This file was automatically generated

(ns metaprob.src.mem
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare mem)

(define
  mem
  (program
    [f]
    (define root this)
    (define cache (mk_nil))
    (program
      [arg]
      (define addr (list arg))
      (if (trace_has (lookup cache addr))
        (trace_get (lookup cache addr))
        (block
          (define
            val
            (with-address (add (list root "cache") addr) (f arg)))
          (trace_set (lookup cache addr) val)
          val)))))

(trace_set (lookup mem (list "name")) "mem")


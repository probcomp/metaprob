;; This file was automatically generated

(ns metaprob.examples.normal-normal
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare proposer)

(define
  proposer
  (program
    [args intervention target output]
    (define mu (trace_get (lookup args (list 0))))
    (define sig1 (trace_get (lookup args (list 1))))
    (define sig2 (trace_get (lookup args (list 2))))
    (if (and
          (trace_has target)
          (not (trace_has (lookup target (list "x")))))
      (block
        (define prec1 (div 1 (pow sig1 2)))
        (define prec2 (div 1 (pow sig2 2)))
        (define
          post_mu
          (div
            (add (mul mu prec1) (mul (trace_get target) prec2))
            (add prec1 prec2)))
        (define post_prec (add prec1 prec2))
        (define post_sig (sqrt (div 1 post_prec)))
        (define post_sample (normal post_mu post_sig))
        (trace_set (lookup output (list "x")) post_sample)
        (py_propose
          normal
          (tuple mu (sqrt (add (pow sig1 2) (pow sig2 2))))
          intervention
          target
          output))
      (block
        (define
          [val score]
          (py_propose
            normal
            (tuple mu sig1)
            (lookup intervention (list "x"))
            (lookup target (list "x"))
            (lookup output (list "x"))))
        (define
          [val2 score2]
          (py_propose
            normal
            (tuple val sig2)
            intervention
            target
            output))
        (tuple val2 (add score score2))))))

(define normal_normal (sp "normal_normal" proposer))


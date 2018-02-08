;; See doc/about-the-prelude.md

(ns metaprob.prelude
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all]))

(declare
  drop
  reverse
  _reverse
  iterate
  replicate
  repeat
  map
  _map
  _imap
  imap
  zipmap
  for_each
  for_each2
  _i_for_each2
  i_for_each2
  filter
  concat
  trace_of
  lookup_chain
  lookup_chain_with_exactly
  sp
  proposer_of)

(define addresses_of trace_sites)

(define uniform_sample uniform_categorical)

(define uniform uniform_continuous)

(trace_set (lookup uniform (list "name")) "uniform")

(trace_set (lookup flip (list "support")) (array_to_list (tuple true false)))

(define
  drop
  (program
    [lst index]
    (block (if (gt index 0) (drop (rest lst) (sub index 1)) lst))))

(define reverse (program [lst] (_reverse lst (mk_nil))))

(define
  _reverse
  (program
    [lst res]
    (if (is_pair lst)
      (_reverse (rest lst) (pair (first lst) res))
      res)))

(define
  iterate
  (program
    [n f a]
    (if (lte n 0) a (block (iterate (sub n 1) f (f a))))))

(define
  replicate
  (program
    [n f]
    (define root this)
    (map (program [i] (with-address (list root i) (f))) (range n))))

(trace_set (lookup replicate (list "name")) "replicate")

(define
  repeat
  (program
    [times program-noncolliding]
    (if (gt times 0)
      (block
        (program-noncolliding)
        (repeat (sub times 1) program-noncolliding))
      "ok")))

(define
  map
  (program
    [f l]
    (define root this)
    (define
      ans
      (if (is_metaprob_array l)
        (list_to_array (_map f (array_to_list l) 0 root))
        (block (_map f l 0 root))))
    (dereify_tag root)
    ans))

(define
  _map
  (program
    [f l i root]
    (block
      (if (is_pair l)
        (block
          (define val (with-address (list root i) (f (first l))))
          (pair val (_map f (rest l) (add i 1) root)))
        (mk_nil)))))

(define
  _imap
  (program
    [f i l]
    (if (is_pair l)
      (pair (f i (first l)) (_imap f (add i 1) (rest l)))
      (mk_nil))))

(define
  imap
  (program
    [f l]
    (if (is_metaprob_array l)
      (list_to_array (_imap f 0 (array_to_list l)))
      (block (_imap f 0 l)))))

(define
  zipmap
  (program
    [f l1 l2]
    (if (and (is_pair l1) (is_pair l2))
      (pair (f (first l1) (first l2)) (zipmap f (rest l1) (rest l2)))
      (mk_nil))))

(define
  for_each
  (program
    [l f]
    (if (is_pair l)
      (block (f (first l)) (for_each (rest l) f))
      "done")))

(define
  for_each2
  (program
    [f l1 l2]
    (if (and (is_pair l1) (is_pair l2))
      (block
        (f (first l1) (first l2))
        (for_each2 f (rest l1) (rest l2)))
      "done")))

(define
  _i_for_each2
  (program
    [f i l1 l2]
    (if (and (is_pair l1) (is_pair l2))
      (block
        (f i (first l1) (first l2))
        (_i_for_each2 f (add i 1) (rest l1) (rest l2)))
      "done")))

(define i_for_each2 (program [f l1 l2] (_i_for_each2 f 0 l1 l2)))

(define
  filter
  (program
    [pred l]
    (if (is_pair l)
      (if (pred (first l))
        (pair (first l) (filter pred (rest l)))
        (block (filter pred (rest l))))
      (mk_nil))))

(define
  concat
  (program
    [ll]
    (if (is_pair ll) (append (first ll) (concat (rest ll))) (mk_nil))))

(define
  lookup_chain
  (program
    [coll key]
    (if (is_pair key)
      (lookup_chain (lookup coll (first key)) (rest key))
      coll)))

(define
  lookup_chain_with_exactly
  (program
    [coll key]
    (if (is_pair key)
      (lookup_chain_with_exactly (lookup coll (first key)) (rest key))
      (block (exactly coll)))))

(define
  sp
  (program
    [name proposer]
    (define
      interpreter
      (program
        [args intervene]
        (define [v _] (proposer args intervene (mk_nil) (mk_nil)))
        v))
    (define
      tracer
      (program
        [args intervene output]
        (define [v _] (proposer args intervene (mk_nil) output))
        v))
    (define
      non_tracing_proposer
      (program
        [args intervene target]
        (proposer args intervene target (mk_nil))))
    (block
      (define __trace_0__ (mk_nil))
      (trace_set __trace_0__ "prob prog")
      (trace_set (lookup __trace_0__ (list "name")) name)
      (trace_set
        (lookup __trace_0__ (list "custom_interpreter"))
        interpreter)
      (trace_set
        (lookup __trace_0__ (list "custom_choice_tracer"))
        tracer)
      (trace_set
        (lookup __trace_0__ (list "custom_proposer"))
        non_tracing_proposer)
      (trace_set
        (lookup __trace_0__ (list "custom_choice_tracing_proposer"))
        proposer)
      __trace_0__)))

(define tracing_proposer_to_prob_prog sp)

(define
  proposer_of
  (program
    [the_sp]
    (trace_get
      (lookup the_sp (list "custom_choice_tracing_proposer")))))

(define
  factor
  (sp
    "factor"
    (program
      [args t1 t2 t3]
      (define score (trace_get (lookup args (list 0))))
      (tuple (mk_nil) score))))

;; Manual edit: moved from interpret.clj

(define
  name_for_definiens
  (program
    [pattern]
    (block
      (if (eq (trace_get pattern) "variable")
        (block
          (if (neq (trace_get (lookup pattern (list "name"))) "_")
            (block (list (trace_get (lookup pattern (list "name")))))
            (block (list "definiens"))))
        (block (list "definiens"))))))


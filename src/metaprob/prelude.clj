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

(define
  drop
  (program
    [lst index]
    (block (if (gt index 0) (drop (rest lst) (sub index 1)) lst))))

(define reverse (program [lst] (_reverse lst (empty-trace))))

(define
  _reverse
  (program
    [lst res]
    (if (is-pair lst)
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

(trace-set (lookup replicate (list "name")) "replicate")

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
      (if (is-array l)
        (list-to-array (_map f (array-to-list l) 0 root))
        (block (_map f l 0 root))))
    ;; (dereify_tag root)
    ans))

(define
  _map
  (program
    [f l i root]
    (block
      (if (is-pair l)
        (block
          (define val (with-address (list root i) (f (first l))))
          (pair val (_map f (rest l) (add i 1) root)))
        (empty-trace)))))

(define
  _imap
  (program
    [f i l]
    (if (is-pair l)
      (pair (f i (first l)) (_imap f (add i 1) (rest l)))
      (empty-trace))))

(define
  imap
  (program
    [f l]
    (if (is-array l)
      (list-to-array (_imap f 0 (array-to-list l)))
      (block (_imap f 0 l)))))

(define
  zipmap
  (program
    [f l1 l2]
    (if (and (is-pair l1) (is-pair l2))
      (pair (f (first l1) (first l2)) (zipmap f (rest l1) (rest l2)))
      (empty-trace))))

(define
  for_each
  (program
    [l f]
    (if (is-pair l)
      (block (f (first l)) (for_each (rest l) f))
      "done")))

(define
  for_each2
  (program
    [f l1 l2]
    (if (and (is-pair l1) (is-pair l2))
      (block
        (f (first l1) (first l2))
        (for_each2 f (rest l1) (rest l2)))
      "done")))

(define
  _i_for_each2
  (program
    [f i l1 l2]
    (if (and (is-pair l1) (is-pair l2))
      (block
        (f i (first l1) (first l2))
        (_i_for_each2 f (add i 1) (rest l1) (rest l2)))
      "done")))

(define i_for_each2 (program [f l1 l2] (_i_for_each2 f 0 l1 l2)))

(define
  filter
  (program
    [pred l]
    (if (is-pair l)
      (if (pred (first l))
        (pair (first l) (filter pred (rest l)))
        (block (filter pred (rest l))))
      (empty-trace))))

(define
  concat
  (program
    [ll]
    (if (is-pair ll) (append (first ll) (concat (rest ll))) (empty-trace))))

(define
  lookup_chain
  (program
    [coll key]
    (if (is-pair key)
      (lookup_chain (lookup coll (first key)) (rest key))
      coll)))

(define
  lookup_chain_with_exactly
  (program
    [coll key]
    (if (is-pair key)
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
        (define [v _] (proposer args intervene (empty-trace) (empty-trace)))
        v))
    (define
      tracer
      (program
        [args intervene output]
        (define [v _] (proposer args intervene (empty-trace) output))
        v))
    (define
      non_tracing_proposer
      (program
        [args intervene target]
        (proposer args intervene target (empty-trace))))
    (block
      (define __trace_0__ (empty-trace))
      (trace-set __trace_0__ "prob prog")
      (trace-set (lookup __trace_0__ (list "name")) name)
      (trace-set
        (lookup __trace_0__ (list "custom_interpreter"))
        interpreter)
      (trace-set
        (lookup __trace_0__ (list "custom_choice_tracer"))
        tracer)
      (trace-set
        (lookup __trace_0__ (list "custom_proposer"))
        non_tracing_proposer)
      (trace-set
        (lookup __trace_0__ (list "custom_choice_tracing_proposer"))
        proposer)
      __trace_0__)))

(define tracing_proposer_to_prob_prog sp)

(define
  proposer_of
  (program
    [the_sp]
    (trace-get
      (lookup the_sp (list "custom_choice_tracing_proposer")))))

(define
  factor
  (sp
    "factor"
    (program
      [args t1 t2 t3]
      (define score (trace-get (lookup args (list 0))))
      (tuple (empty-trace) score))))

;; Manual edit: moved from interpret.clj

(define
  name_for_definiens
  (program
    [pattern]
    (block
      (if (eq (trace-get pattern) "variable")
        (block
          (if (neq (trace-get (lookup pattern (list "name"))) "_")
            (block (list (trace-get (lookup pattern (list "name")))))
            (block (list "definiens"))))
        (block (list "definiens"))))))

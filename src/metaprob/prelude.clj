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
  concat)

(define
  drop
  (gen
    [lst index]
    (block (if (gt index 0) (drop (rest lst) (sub index 1)) lst))))

(define reverse (gen [lst] (_reverse lst (empty-trace))))

(define
  _reverse
  (gen
    [lst res]
    (if (is-pair lst)
      (_reverse (rest lst) (pair (first lst) res))
      res)))

(define
  iterate
  (gen
    [n f a]
    (if (lte n 0) a (block (iterate (sub n 1) f (f a))))))

(define replicate
  (gen [n f]
    (define root this)
    (map (gen [i] (with-address (list root i) (f))) (range n))))

(define
  repeat
  (gen
    [times pp]
    (if (gt times 0)
      (block
        (pp)
        (repeat (sub times 1) pp))
      "ok")))

(define map
  (gen [f l]
    (define root this)
    (define
      ans
      (if (tuple? l)
        (to-tuple (_map f (to-list l) 0 root))
        (block (_map f l 0 root))))
    ;; (dereify_tag root)
    ans))

(define _map
  (gen [f l i root]
    (block
      (if (is-pair l)
        (block
          (define val (with-address (list root i) (f (first l))))
          (pair val (_map f (rest l) (add i 1) root)))
        (empty-trace)))))

(define
  _imap
  (gen
    [f i l]
    (if (is-pair l)
      (pair (f i (first l)) (_imap f (add i 1) (rest l)))
      (empty-trace))))

(define
  imap
  (gen
    [f l]
    (if (tuple? l)
      (to-tuple (_imap f 0 (to-list l)))
      (block (_imap f 0 l)))))

(define
  zipmap
  (gen
    [f l1 l2]
    (if (and (is-pair l1) (is-pair l2))
      (pair (f (first l1) (first l2)) (zipmap f (rest l1) (rest l2)))
      (empty-trace))))

(define
  for-each
  (gen
    [l f]
    (if (is-pair l)
      (block (f (first l)) (for-each (rest l) f))
      "done")))

(define
  for-each2
  (gen
    [f l1 l2]
    (if (and (is-pair l1) (is-pair l2))
      (block
        (f (first l1) (first l2))
        (for-each2 f (rest l1) (rest l2)))
      "done")))

(define
  _i_for_each2
  (gen
    [f i l1 l2]
    (if (and (is-pair l1) (is-pair l2))
      (block
        (f i (first l1) (first l2))
        (_i_for_each2 f (add i 1) (rest l1) (rest l2)))
      "done")))

(define i_for_each2 (gen [f l1 l2] (_i_for_each2 f 0 l1 l2)))

(define
  filter
  (gen
    [pred l]
    (if (is-pair l)
      (if (pred (first l))
        (pair (first l) (filter pred (rest l)))
        (block (filter pred (rest l))))
      (empty-trace))))

(define
  concat
  (gen
    [ll]
    (if (is-pair ll) (append (first ll) (concat (rest ll))) (empty-trace))))

;; Manual edit: moved from interpret.clj

(define
  name_for_definiens
  (gen
    [pattern]
    (block
      (if (eq (trace-get pattern) "variable")
        (block
          (if (neq (trace-get (lookup pattern (list "name"))) "_")
            (block (list (trace-get (lookup pattern (list "name")))))
            (block (list "definiens"))))
        (block (list "definiens"))))))

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
  ;; replicate
  repeat
  ;; map
  _map
  _imap
  imap
  zipmap
  for-each
  for-each2
  _i_for_each2
  i_for_each2
  filter
  concat)

(define drop
  (gen [lst index]
    (block (if (gt index 0) (drop (rest lst) (sub index 1)) lst))))

(define reverse
  (gen [lst] (_reverse lst (if (mutable-trace? lst)
                             (empty-trace)
                             (list)))))

(define _reverse
  (gen [lst res]
    (if (pair? lst)
      (_reverse (rest lst) (pair (first lst) res))
      res)))

(define iterate
  (gen [n f a]
    (if (lte n 0) a (block (iterate (sub n 1) f (f a))))))

(define repeat
  (gen [times pp]
    (if (gt times 0)
      (block
        (pp)
        (repeat (sub times 1) pp))
      "ok")))

;; Similar to `lookup` but does not create locatives

(define maybe-subtrace
  (gen [tr adr]
    (if (and tr (trace-has-subtrace? tr adr))
      (trace-subtrace tr adr)
      nil)))

(define _imap
  (gen [f i l]
    (if (pair? l)
      (pair (f i (first l)) (_imap f (add i 1) (rest l)))
      l)))

(define imap
  (gen [f l]
    (if (tuple? l)
      (to-tuple (_imap f 0 (to-list l)))
      (block (_imap f 0 l)))))

(define zipmap
  (gen [f l1 l2]
    (if (and (pair? l1) (pair? l2))
      (pair (f (first l1) (first l2)) (zipmap f (rest l1) (rest l2)))
      (list))))

(define for-each
  (gen [l f]
    (if (pair? l)
      (block (f (first l)) (for-each (rest l) f))
      "done")))

(define for-each2
  (gen [f l1 l2]
    (if (and (pair? l1) (pair? l2))
      (block
        (f (first l1) (first l2))
        (for-each2 f (rest l1) (rest l2)))
      "done")))

(define _i_for_each2
  (gen [f i l1 l2]
    (if (and (pair? l1) (pair? l2))
      (block
        (f i (first l1) (first l2))
        (_i_for_each2 f (add i 1) (rest l1) (rest l2)))
      "done")))

(define i_for_each2 (gen [f l1 l2] (_i_for_each2 f 0 l1 l2)))

(define filter
  (gen [pred l]
    (if (pair? l)
      (if (pred (first l))
        (pair (first l) (filter pred (rest l)))
        (block (filter pred (rest l))))
      l)))

(define concat
  (gen [ll]
    (if (pair? ll)
      (append (first ll) (concat (rest ll)))
      ll)))

;; Moved to infer.clj

(define map-original
  (gen [f l]
    (define root (&this))
    (if (tuple? l)
        (to-tuple (_map f (to-list l) 0 root))
        (block (_map f l 0 root)))))

(define _map
  (gen [f l i root]
    (if (pair? l)
      (block (define val (with-address (list root i) (f (first l))))
             (pair val (_map f (rest l) (add i 1) root)))
      l)))

; (define map map-original)

(define replicate-original
  (gen [n f]
    (define root (&this))
    ;; Should be (map (gen [i] (f)) (range n))
    ;; because map already sets the correct address.
    (map-original (gen [i] (with-address (list root i) (f))) (range n))))


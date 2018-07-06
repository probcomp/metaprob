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
    (block (if (> index 0) (drop (rest lst) (- index 1)) lst))))

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
    (if (<= n 0) a (block (iterate (- n 1) f (f a))))))

(define repeat
  (gen [times pp]
    (if (> times 0)
      (block
        (pp)
        (repeat (- times 1) pp))
      "ok")))

(define _imap
  (gen [f i l]
    (if (pair? l)
      (pair (f i (first l)) (_imap f (+ i 1) (rest l)))
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
        (_i_for_each2 f (+ i 1) (rest l1) (rest l2)))
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

;; -----------------------------------------------------------------------------
;; Interpreter utilities

(define maybe-subtrace
  (gen [tr adr]
    (if (trace-has-subtrace? tr adr)
      (trace-subtrace tr adr)
      (trace))))

(define maybe-set-subtrace
  (gen [output key suboutput]
    (assert (trace? output) "bad output")
    (if suboutput
      (block (assert (trace? suboutput) ["bad suboutput" key suboutput])
             (if (empty-trace? suboutput)
               output
               (trace-set-subtrace output key suboutput)))
      output)))

;; Utilities for interpreter

;; This is used to compute the key under which to store the value that
;; is the binding of a definition.

(define name-for-definiens
  (gen [pattern]
    (if (eq (trace-get pattern) "variable")
      (block (define name (trace-get pattern "name"))
             (if (or (eq name "_")           ;Cf. from-clojure-definition in syntax.clj
                     (eq name "pattern"))
               "definiens"
               (trace-get pattern "name")))
      "definiens")))

;; Get the key to use for storing the result of a procedure call.

(define application-result-key
  (gen [exp]
    (define key (if (eq (trace-get exp) "variable")
                  (trace-get exp "name")
                  "call"))
    (assert (ok-key? key) key)
    key))


;; -----------------------------------------------------------------------------
;; Inf-based utilities.

(define opaque
  (gen [name proc]
    (inf name
         proc   ;model (generative procedure)
         (gen [inputs intervene target output?]
           ;; Ignore the traces.
           (infer-apply proc inputs (trace) (trace) false)))))

(define apply
  (trace-as-procedure
   (inf "apply"
        clojure.core/apply   ;model (generative procedure)
        (gen [inputs intervene target output?]
          (infer-apply (first inputs) (rest inputs) intervene target output?)))
   ;; Kludge
   (gen [proc inputs]
     (clojure.core/apply proc (to-immutable-list inputs)))))

;; map defined using inf (instead of with-address)

(define map-issue-20
  (inf "map"
       nil                              ;no model
       (gen [[fun sequ] intervene target output?]
         (block (define re
                  (gen [i l]
                    (if (pair? l)
                      (block (define [value suboutput subscore]
                               (infer-apply fun
                                            [(first l)]
                                            ;; advance traces by address i
                                            (maybe-subtrace intervene i)
                                            (maybe-subtrace target i)
                                            output?))

                             ;; Recur over rest of list
                             (define [values output score]
                               (re (+ i 1)
                                   (rest l)))
                             [(pair value values)
                              (maybe-set-subtrace output i suboutput)
                              (+ subscore score)])

                      ;; End of list
                      [(if (tuple? sequ) (to-tuple l) l)
                       (trace)
                       0])))

                ;; Fire it up
                (re 0 (to-list sequ))))))

(define map map-issue-20)

(define replicate
  (gen [n f]
    (map (gen [i] (f))
         (range n))))

(ns metaprob.context
  (:refer-clojure :only [declare ns instance? deref update swap!])
  (:require [metaprob.syntax :refer :all]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.prelude :refer :all])
  (:import (clojure.lang Atom)))

(clojure.core/defn cell? [x] (instance? Atom x))
(clojure.core/defn cell [x] (Atom. x))

(define single-key? (gen [adr] (not (clojure.core/seq? adr))))
(define compound-adr (gen [adr] (if (single-key? adr) `(~adr) adr)))

; trace is either an execution trace or a vector of an atom and current address.
(define make-top-level-tracing-context
  (gen [intervention-trace target-trace]
    {:tracing-context? true,
     :interpretation-id (clojure.core/gensym),
     :trace  intervention-trace,
     :current-address '(),
     :target target-trace,
     :active? true}))

(define direct-subcontext
  (gen [ctx key]
    (define subtrace (get (get ctx :trace) key))
    (define target-subtrace (get (get ctx :target) key))
    (if (or (cell? subtrace) (not (cell? (get ctx :trace))))
      (assoc ctx :trace subtrace, :current-address '(), :target target-subtrace)
      (assoc ctx :current-address (concat (get ctx :current-address) `(~key)) :target target-subtrace))))

(define subcontext
  (gen [ctx adr]
    (cond
      (not (get ctx :active?)) ctx
      (= adr '()) ctx
      (single-key? adr) (direct-subcontext ctx adr)
      true (subcontext (direct-subcontext ctx (first adr)) (rest adr)))))


; Returns [new-ctx t], where t is the captured context,
; and new-ctx is an updated (inactive) current context.
(define capture-tracing-context
  (gen [ctx]
    (cond
      (not (get ctx :active?)) (block (pprint ["WAS NOT ACTIVE" ctx]) [ctx ctx])
      (not (cell? (get ctx :trace)))
      (block (pprint ["WAS NOT A CELL" (unbox ctx)])
      [(assoc ctx :active? false)
       (assoc ctx :trace (cell (get ctx :trace)))])
      true ; it is a cell
      (block
         (define new-trace (cell (trace-subtrace (get ctx :trace) (get ctx :current-address))))
         (swap! (get ctx :trace) trace-set-subtrace (get ctx :current-address) new-trace)
         (pprint ["CREATED INSIDE ATOM:" (unbox ctx)])
         (define new-ctx (assoc ctx :trace new-trace :current-address '()))
         [(assoc new-ctx :active? false) new-ctx]))))

; Note: MUST be called when manager is at the proper context.
; Returns a version of the current manager in which this tracing context
; does not exist.
(define release-tracing-context
  (gen [ctx]
    (assert (= (get ctx :current-address) '()) "Cannot call close-context on a sub-context.")
    (pprint ["RELEASING" (unbox ctx)])
    (assoc ctx :trace (unbox (get ctx :trace)))))

(define targeted?
  (gen [ctx adr]
    (trace-has-value? (get ctx :target) adr)))

(define target-value
  (gen [ctx adr]
    (trace-value (get ctx :target) adr)))

(define direct-recorded-value
  (gen [ctx]
    (trace-value (get ctx :trace) (get ctx :current-address))))

(define recorded-value
  (gen [ctx adr]
    (direct-recorded-value (subcontext ctx adr))))

(define direct-record-or-use-constrained!
  (gen [ctx val]
    (define value (or-nil? (trace-value (get ctx :trace) (get ctx :current-address))
                      (trace-value (get ctx :target))
                      val))
    (if (cell? (get ctx :trace))
      (block (swap! (get ctx :trace) trace-set-value (get ctx :current-address) value) ctx)
      (assoc ctx :trace (trace-set-value (get ctx :trace) value)))))

(define incorporate-subctx
  (gen [ctx adr subctx]
    (assert (not= adr '()) "Must provide a non-empty address to incorporate-subctx")
    ; (assert (not (cell? (:trace subctx))) "Cannot incorporate a captured subcontext.")
    (cond
      (not (get ctx :active?)) ctx
      ; If I am a cell, there are two cases:
      ;   1. subctx is the same cell, and I have already incorporated the changes
      ;   2. subctx is a different cell—an error.
      (and (cell? (get ctx :trace))
           (clojure.core/identical? (get ctx :trace) (get subctx :trace))) ctx
      (cell? (get ctx :trace))
      (block
        (pprint ["INCORPORATING" adr (unbox (get ctx :trace)) "=======" (unbox (get subctx :trace))])

        (swap! (get ctx :trace)
               (gen [tr]
                 (if (empty? (get subctx :trace))
                   (trace-clear-subtrace tr (concat (get ctx :current-address) (compound-adr adr)))
                   (trace-set-subtrace tr (concat (get ctx :current-address) (compound-adr adr)) (get subctx :trace)))))
        ctx)
      true
      (block
        (pprint ["INCORPORATING" adr (unbox (get ctx :trace)) "=======" (unbox (get subctx :trace))])

        (assert (not (cell? (get subctx :trace))) "Cannot incorporate a captured subcontext.")
        (assoc ctx :trace (maybe-set-subtrace (get ctx :trace) adr (get subctx :trace)))))))


(define record-or-use-constrained!
  (gen [ctx adr val]
    (incorporate-subctx ctx adr (direct-record-or-use-constrained! (subcontext ctx adr) val))))

(define current-trace
  (gen [ctx]
    (trace-subtrace (get ctx :trace) (get ctx :current-address))))
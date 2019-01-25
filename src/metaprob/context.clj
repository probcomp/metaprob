(ns metaprob.context
  (:refer-clojure :only [declare ns instance? nil? deref update swap!
                         atom])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.prelude :refer :all])
  (:import [clojure.lang Atom]))

(clojure.core/defn cell? [x] (instance? Atom x))
(clojure.core/defn cell [x] (atom x))

;; (define single-key? (gen [adr] (not (clojure.core/seq? adr))))
;; (define compound-adr (gen [adr] (if (single-key? adr) `(~adr) adr)))

(define make-top-level-tracing-context
  (gen [intervention-trace target-trace]
    {:interpretation-id (clojure.core/gensym)
     :active? true
     :intervene intervention-trace
     :target target-trace}))

(define active-ctx?
  (gen [ctx] (get ctx :active?)))

(define captured-ctx?
  (gen [ctx] (not (nil? (get ctx :captured-state)))))

;; trace is either an execution trace or a vector of an atom and current address.
;(define make-top-level-tracing-context
;  (gen [intervention-trace target-trace]
;    {:tracing-context? true,
;     :interpretation-id (clojure.core/gensym),
;     :trace  intervention-trace,
;     :current-address '(),
;     :target target-trace,
;     :active? true}))

(define subcontext
  (gen [ctx adr]
    (assert (not (captured-ctx? ctx)) "Cannot take subcontext of a captured context")
    (if (active-ctx? ctx)
      (assoc ctx :intervene (maybe-subtrace (get ctx :intervene) adr),
             :target (maybe-subtrace (get ctx :target) adr))
      ctx)))

;(define direct-subcontext
;  (gen [ctx key]
;    (define subtrace (get (get ctx :trace) key))
;    (define target-subtrace (get (get ctx :target) key))
;    (if (or (cell? subtrace) (not (cell? (get ctx :trace))))
;      (assoc ctx :trace subtrace, :current-address '(), :target target-subtrace)
;      (assoc ctx :current-address (concat (get ctx :current-address) `(~key)) :target target-subtrace))))
;
;(define subcontext
;  (gen [ctx adr]
;    (cond
;      (not (get ctx :active?)) ctx
;      (= adr '()) ctx
;      (single-key? adr) (direct-subcontext ctx adr)
;      true (subcontext (direct-subcontext ctx (first adr)) (rest adr)))))

(define attach-clojure-implementation
  (gen [ctx]
    (define ctx' (unbox ctx))
    (if (active-ctx? ctx')
      (clojure.core/with-meta
        (clojure.core/fn [adr proc & args]
          (define [out-atom score-atom applicator]
            (get ctx' :captured-state))
          (define modified-tc (dissoc ctx' :captured-state))
          (define [v o s] (applicator
                           proc
                           args
                           (subcontext modified-tc adr)))
          (swap! out-atom trace-merge (maybe-set-subtrace {} adr o))
          (swap! score-atom + s)
          v)
        ctx')
      (clojure.core/with-meta
        (clojure.core/fn [adr proc & args] (apply proc args))
        ctx'))))


;; Returns a new version t' of a captured tracing context t in which a
;; certain address has been intervened on. Note that (t ... proc ...)
;; will run proc un-intervened, while (t' ... proc ...) will run
;; intervened.
(define intervene-on-captured-context
  (gen [ctx new-intervene]
    (define ctx' (assoc ctx :intervene new-intervene))
    (if (captured-ctx? ctx')
      (attach-clojure-implementation ctx')
      ctx')))

;(gen []
;  (with-explicit-tracer t
;    (clojure.core/map (gen [x] (t x flip x))
;                      '(0.1 0.5 0.9))))

; returns captured-ctx
(define capture-tracing-context
  (gen [ctx applicator]
    (attach-clojure-implementation
      (if (active-ctx? ctx)
        (assoc ctx :captured-state [(cell {}) (cell 0) applicator])
        ctx))))

(define release-tracing-context
  (gen [ctx]
    (if (captured-ctx? ctx)
      (block
       (define [out-atom score-atom _] (get ctx :captured-state))
       [(deref out-atom) (deref score-atom)])
      [{} 0])))

;; Returns [new-ctx t], where t is the captured context,
;; and new-ctx is an updated (inactive) current context.
;(define capture-tracing-context
;  (gen [ctx]
;    (cond
;      (not (get ctx :active?)) (block (pprint ["WAS NOT ACTIVE" ctx]) [ctx ctx])
;      (not (cell? (get ctx :trace)))
;      (block (pprint ["WAS NOT A CELL" (unbox ctx)])
;      [(assoc ctx :active? false)
;       (assoc ctx :trace (cell (get ctx :trace)))])
;      true ; it is a cell
;      (block
;         (define new-trace (cell (trace-subtrace (get ctx :trace) (get ctx :current-address))))
;         (swap! (get ctx :trace) trace-set-subtrace (get ctx :current-address) new-trace)
;         (pprint ["CREATED INSIDE ATOM:" (unbox ctx)])
;         (define new-ctx (assoc ctx :trace new-trace :current-address '()))
;         [(assoc new-ctx :active? false) new-ctx]))))

(define targeted?
  (gen [ctx adr]
    (trace-has-value? (get ctx :target) adr)))

(define target-value
  (gen [ctx adr]
    (trace-value (get ctx :target) adr)))

(define intervened?
  (gen [ctx adr]
    (trace-has-value? (get ctx :intervene) adr)))

(define intervene-value
  (gen [ctx adr]
    (trace-value (get ctx :intervene) adr)))

(define constrained?
  (gen [ctx adr]
    (or (targeted? ctx adr)
        (intervened? ctx adr))))

(define constrained-value
  (gen [ctx adr]
    (or-nil? (target-value ctx adr) (intervene-value ctx adr))))

;(define direct-recorded-value
;  (gen [ctx]
;    (trace-value (get ctx :trace) (get ctx :current-address))))
;
;(define recorded-value
;  (gen [ctx adr]
;    (direct-recorded-value (subcontext ctx adr))))
;;
;(define direct-record-or-use-constrained!
;  (gen [ctx val]
;    (define value (or-nil? (trace-value (get ctx :trace) (get ctx :current-address))
;                      (trace-value (get ctx :target))
;                      val))
;    (if (cell? (get ctx :trace))
;      (block (swap! (get ctx :trace) trace-set-value (get ctx :current-address) value) ctx)
;      (assoc ctx :trace (trace-set-value (get ctx :trace) value)))))
;
;
;(define incorporate-subctx
;  (gen [ctx adr subctx]
;    (assert (not= adr '()) "Must provide a non-empty address to incorporate-subctx")
;    ; (assert (not (cell? (:trace subctx))) "Cannot incorporate a captured subcontext.")
;    (cond
;      (not (get ctx :active?)) ctx
;      ; If I am a cell, there are two cases:
;      ;   1. subctx is the same cell, and I have already incorporated the changes
;      ;   2. subctx is a different cell—an error.
;      (and (cell? (get ctx :trace))
;           (clojure.core/identical? (get ctx :trace) (get subctx :trace))) ctx
;      (cell? (get ctx :trace))
;      (block
;        (pprint ["INCORPORATING" adr (unbox (get ctx :trace)) "=======" (unbox (get subctx :trace))])
;
;        (swap! (get ctx :trace)
;               (gen [tr]
;                 (if (empty? (get subctx :trace))
;                   (trace-clear-subtrace tr (concat (get ctx :current-address) (compound-adr adr)))
;                   (trace-set-subtrace tr (concat (get ctx :current-address) (compound-adr adr)) (get subctx :trace)))))
;        ctx)
;      true
;      (block
;        (pprint ["INCORPORATING" adr (unbox (get ctx :trace)) "=======" (unbox (get subctx :trace))])
;
;        (assert (not (cell? (get subctx :trace))) "Cannot incorporate a captured subcontext.")
;        (assoc ctx :trace (maybe-set-subtrace (get ctx :trace) adr (get subctx :trace)))))))
;
;
;(define record-or-use-constrained!
;  (gen [ctx adr val]
;    (incorporate-subctx ctx adr (direct-record-or-use-constrained! (subcontext ctx adr) val))))
;
;(define current-trace
;  (gen [ctx]
;    (trace-subtrace (get ctx :trace) (get ctx :current-address))))

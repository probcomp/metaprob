;; A meta-circular Metaprob interpreter.

(ns metaprob.compositional
  (:refer-clojure :only [declare ns name])
  (:require [metaprob.syntax :refer :all]
            [clojure.pprint :as pp]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.codeutils :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.context :refer :all]
            [metaprob.frame :refer :all]))

;; ----------------------------------------------------------------------------
(define debug? false)

(declare infer-apply-native infer-apply-foreign infer-apply-tc infer-apply-proc
         infer-apply infer-eval infer-eval-sequence)

; This used to take in a context
(define compile-mp-proc
  (clojure.core/fn [proc]
    (clojure.core/with-meta
      (clojure.core/fn [& args] (nth (infer-apply proc (clojure.core/vec args) {} {} false) 0))
      (unbox proc))))

;; Main entry point: an `apply` that respects interventions
;; and constraints, records choices made, and computes scores.

;; Output-is-not-an-input version
(define third  (gen [l] (first (rest (rest l)))))
(define fourth (gen [l] (first (rest (rest (rest l))))))

; Top Level Entry Point
(define infer-apply
  ;(gen [proc inputs intervene target out?]
  ;  (assert (trace? intervene) ["bad intervene" intervene])
  ;  (assert (trace? target) ["target" target])
  ;
  ;  (define ctx (assoc (make-top-level-tracing-context intervene target) :active? out?))
  ;
  ;  (define [v ctx' s] (infer-apply-proc proc inputs ctx))
  ;  [v (get ctx' :trace) s]))

  (inf
    "infer-apply"
    (gen [proc inputs intervene target out?]
      (assert (trace? intervene) ["bad intervene" intervene])
      (assert (trace? target) ["target" target])

      (define ctx (assoc (make-top-level-tracing-context intervene target) :active? out?))

      (define [v ctx' s] (infer-apply-proc proc inputs ctx))
      [v (current-trace ctx') s])

    (gen [[proc inputs intervene target out?] interpreter-ctx]
      (define ctx
        (clojure.core/reduce
          (gen [ctx-so-far new-address]
            (record-or-use-constrained! ctx-so-far new-address (trace-value intervene new-address)))
          (assoc interpreter-ctx :target (trace-merge target (get interpreter-ctx :target)) :active? (or out? (get interpreter-ctx :active?)))
          (addresses-of intervene)))

      (define [v ctx' s] (infer-apply-proc proc inputs ctx))
      [[v (if out? (current-trace ctx') {}) s] ctx' s])))

; Apply a procedure with a trace manager.
(define infer-apply-proc
  (gen [proc inputs ctx]
    (clojure.core/when debug? (pprint ["APPLYING PROC:" (unbox proc)]))
    (cond
      ;(and (not (:active? ctx)) (fn? proc))
      ;[(apply proc inputs) ctx 0] ; TODO: should this be clojure.core/apply?

      (contains? proc :implementation)
      (block (clojure.core/when debug? (pprint ["CALLING OUT TO IMP" proc inputs ctx]))
             ((get proc :implementation) inputs ctx))

      (get proc :tracing-context?)
      (infer-apply-tc proc inputs ctx)

      (native-procedure? proc)
      (infer-apply-native proc inputs ctx)

      (get proc :apply?)
      (infer-apply-proc (first inputs) (second inputs) ctx)

      (fn? proc)
      (infer-apply-foreign proc inputs ctx)

      true (error "infer-apply: not a procedure" proc))))


; Apply a procedure with a trace manager.
(define infer-apply-proc-mh
  (gen [proc inputs ctx]
    (clojure.core/when debug? (pprint ["APPLYING PROC:" (unbox proc)]))
    (cond
      ;(and (not (:active? ctx)) (fn? proc))
      ;[(apply proc inputs) ctx 0] ; TODO: should this be clojure.core/apply?

      (and (contains? proc :implementation) (get proc :no-custom-proposal-logic?))
      (block (clojure.core/when debug? (pprint ["CALLING OUT TO IMP" proc inputs ctx]))
             ((get proc :implementation) inputs ctx))

      (get proc :tracing-context?)
      (infer-apply-tc proc inputs ctx)

      (native-procedure? proc)
      (infer-apply-native proc inputs ctx)

      (get proc :apply?)
      (infer-apply-proc (first inputs) (second inputs) ctx)

      (fn? proc)
      (infer-apply-foreign proc inputs ctx)

      true (error "infer-apply: not a procedure" proc))))



;; Invoke a 'foreign' generative procedure, i.e. one written in
;; clojure (or Java)

(define infer-apply-foreign
  (gen [proc inputs ctx]
    ;; 'Foreign' generative procedure
    (define value (generate-foreign proc inputs))
    (define ivalue (or-nil? (direct-recorded-value ctx) value))
    [ivalue ctx (if (not= (or-nil? (target-value ctx '()) ivalue) ivalue) negative-infinity 0)]))

;; Invoke a 'native' generative procedure, i.e. one written in
;; Metaprob, with inference mechanics (traces and scores).
(define infer-apply-native
  (gen [proc inputs ctx]
    (define source (get proc :generative-source))
    (define body (gen-body source))
    (define environment (get proc :environment))
    (define new-env (make-env environment))
    ;; Extend the enclosing environment by binding formals to actuals
    (match-bind! (gen-pattern source) ; pattern. (source is of form '(gen [...] ...)
                 inputs
                 new-env)
    (infer-eval (if (empty? (rest body)) (first body) (cons 'block body)) ; body with implicit `block`
                new-env
                ctx)))

(define infer-apply-tc
  (gen [tc [sub-adr proc & ins] old-ctx]
    (if (not= (get old-ctx :interpretation-id) (get tc :interpretation-id))
      ; This is not "our" tracing context; whoever is interpreting us will know
      ; what to do.
      [(apply tc (cons sub-adr (cons proc ins))) old-ctx 0]
      (block
        (clojure.core/when debug? (pprint ["ABOUT TO CALL INFER-APPLY-PROC IN TC:" (unbox tc) proc ins]))
        (define [v _ s] (infer-apply-proc proc ins (subcontext tc sub-adr)))
        (clojure.core/when debug? (pprint ["modified tc" (unbox tc)]))
        [v old-ctx s]))))

(define infer-subeval
  (gen [sub-exp adr env ctx]
    (clojure.core/when debug? (pprint ["INFER-SUBEVAL will call infer-eval and subcontext"]))
    (define [v subctx s] (infer-eval sub-exp env (subcontext ctx adr)))
    (clojure.core/when debug? (pprint ["INFER-EVAL returned within sub-eval call, going to incorporate" (unbox ctx) adr (unbox subctx)]))
    [v (incorporate-subctx ctx adr subctx) s]))

(define infer-eval-expressions
  (gen [exp env ctx]
    (second
      (clojure.core/doall (clojure.core/reduce
        (gen [[i [v prev-ctx prev-s]] next]
          (clojure.core/when debug? (pprint ["INFER-EVAL-EXPRESSIONS will call infer-subeval" next i env prev-ctx]))
          (define [next-v new-ctx s]
            (infer-subeval next i env prev-ctx))
          [(+ i 1) [(clojure.core/conj v next-v) new-ctx (+ s prev-s)]])
        [0 [[] ctx 0]]
        exp)))))

; Question:
; In the code {(uniform-sample ["key1" "key2" "key3"]) (flip 0.5)}, does resampling the key change the "control flow"?
; Or can we reuse the flip even after the key changes?
(define infer-eval-map
  (gen [exp env ctx]
    (define [_ answer]
      (clojure.core/doall (clojure.core/reduce-kv
        (gen [[i [v prev-ctx prev-s]] next-key next-val]
          (define [key-v ctx-with-key s-key]
            (infer-subeval next-key (str "key" i) env prev-ctx))
          (define [val-v ctx-with-val s-val]
            (infer-subeval next-val (str key-v) env ctx-with-key)) ; TODO: or "vali" for address
          [(+ i 1)
           [(clojure.core/conj v [key-v val-v]) ctx-with-val (+ prev-s s-key s-val)]])
        [0 [{} ctx 0]]
        exp)))
    answer))


;; Evaluate a subexpression (by reduction)
(define infer-eval
  (gen [exp env ctx]
    ; (assert (trace? exp) ["bad expression - eval" exp])
    (assert (environment? env) ["bad env - eval" env])
    (clojure.core/when debug? (pprint ["EVAL:" exp env (unbox ctx)]))
    (define [v final-ctx s]
      (cond
        (variable? exp)
        [(env-lookup env exp) ctx 0]

        (vector? exp)
        (infer-eval-expressions exp env ctx)

        (map? exp)
        (infer-eval-map exp env ctx)

        (or (not (clojure.core/seq? exp)) (= '() exp))
        [exp ctx 0]

        (quote-expr? exp)
        [(quote-quoted exp) ctx 0]

        (with-explicit-tracer-expr? exp)
        (block
          (define [inactive-ctx captured-ctx] (capture-tracing-context ctx))
          (define context-id (clojure.core/gensym)) ; just for debugging
          (pprint ["CAPTURED" context-id (unbox captured-ctx)])
          (define score-acc (cell 0))
          (define augmented-ctx
            (clojure.core/with-meta
              (clojure.core/fn [& args]
                (clojure.core/let [[v _ s] (infer-apply-tc captured-ctx args inactive-ctx)]
                (clojure.core/swap! score-acc + s) v))
              (unbox captured-ctx)))
          (define new-env (make-env env))
          (match-bind! (explicit-tracer-var-name exp) augmented-ctx new-env)
          (define [values _ s] (infer-eval-expressions (explicit-tracer-body exp) new-env inactive-ctx))
          (pprint ["ABOUT TO RELEASE" context-id (unbox captured-ctx)])
          [(clojure.core/last values) (release-tracing-context captured-ctx) (+ s (clojure.core/deref score-acc))])

        (if-expr? exp)
        (block
          (define [predicate-value ctx-with-pred pred-s]
            (infer-subeval (if-predicate exp) "predicate" env ctx))
          (define [final-value final-ctx clause-s]
            (infer-subeval
              (if predicate-value (if-then-clause exp) (if-else-clause exp))
              (if predicate-value "then" "else")
              env ctx-with-pred))
          [final-value final-ctx (+ pred-s clause-s)])

        (definition? exp)
        (block
          (define [rhs-value ctx' s]
            (infer-subeval (definition-rhs exp) (name-for-definiens (definition-pattern exp)) env ctx))
          [(match-bind! (definition-pattern exp) rhs-value env) ctx' s])

        (block-expr? exp)
        (block
          (define new-env (make-env env))
          (define [values ctx' s]
            (infer-eval-expressions (block-body exp) new-env ctx))
          [(clojure.core/last values) ctx' s])

        (gen-expr? exp)
        [(compile-mp-proc
          {:name (trace-name exp)
           :generative-source exp, ; (cons 'gen (cons (second exp) (map mp-expand (rest (rest exp)))))
           :environment env}) ctx 0]

        ; It's an application:
        true
        (block
          (clojure.core/when debug? (pprint ["IT'S AN APPLICATION" exp]))
          (define [evaluated ctx' s] (infer-eval-expressions exp env ctx))
          (clojure.core/when debug? (pprint ["EVALUATED SUB-EXPS:" evaluated]))
          (define key (application-result-key (first exp)))
          (clojure.core/when debug? (pprint ["CALLING INFER-APPLY-PROC:" evaluated]))
          (define [v app-ctx app-s] (infer-apply-proc (first evaluated) (rest evaluated) (subcontext ctx' key)))
          (clojure.core/when debug? (pprint ["INFER-APPLY-PROC FINISHED:" v app-ctx app-s]))
          [v (incorporate-subctx ctx' key app-ctx) (+ s app-s)])))

    ; These may be nil, if no such value exists.
    (define ivalue (direct-recorded-value final-ctx))
    (define tvalue (target-value final-ctx '()))

    (clojure.core/when debug? (pprint ["DONE EVAL-ING:" exp final-ctx v]))

    (cond
      ; intervention with no disagreeing target
      (and (not (clojure.core/nil? ivalue)) (or (clojure.core/nil? tvalue) (= ivalue tvalue)))
      [(or-nil? ivalue v) final-ctx 0]

      ; target and value (from intervention or execution) disagree
      (and tvalue (not= (or-nil? ivalue v) tvalue))
      (assert false (str "Unsatisfiable target constraint (target=" tvalue ", expected=", (or-nil? ivalue v) ")"))

      ; in all other cases, the existing values work fine:
      true
      [(or-nil? ivalue v) final-ctx s])))
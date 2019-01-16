;; A meta-circular Metaprob interpreter.

(ns metaprob.compositional
  (:refer-clojure :only [declare ns name])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.codeutils :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.context :refer :all]
            [metaprob.frame :refer :all]))

;; ----------------------------------------------------------------------------
(declare infer-apply-native infer-apply-foreign infer-apply-tc
         infer-apply infer-eval infer-eval-sequence)

;; infer-apply itself is an inf, because it has custom
;; tracing/proposal behavior.  This is the model of infer-apply's
;; behavior.
(define model-of-infer-apply
  (gen [proc inputs ctx]
    (cond
      (contains? proc :implementation)
      ((get proc :implementation) inputs ctx)

      (native-procedure? proc)
      (infer-apply-native proc inputs ctx)

      (captured-ctx? proc)
      (infer-apply-tc proc inputs ctx)

      (get proc :apply?)
      (infer-apply (first inputs) (second inputs) ctx)

      (fn? proc)
      (infer-apply-foreign proc inputs ctx)

      true
      (error "infer-apply: not a procedure" proc))))

;; infer-apply with custom tracing: when a primitive is invoked by
;; interpreted code, we pretend it is also invoked by the interpreter.
(define infer-apply
  (inf
   "infer-apply"
   model-of-infer-apply
   (gen [[proc ins ctx] ctx']
     ;; (clojure.core/println "infer-apply gen" proc ins ctx)
     (cond
       (not (active-ctx? ctx'))
       [(model-of-infer-apply proc ins ctx) {} 0]

       (get proc :primitive?)
       (if (constrained? ctx '())
         ;; Case 1: we have a constraint, so the interpreter (being
         ;; traced) needs to generate no randomness.
         ;; TODO: What if ctx is inactive?
         [((get proc :implementation) ins ctx) {} 0]

         ;; Case 2: the interpreter needs to generate randomness,
         ;; because proc's execution is unconstrained.
         ;; Score is 0 at proc level.
         (block
          (define [v o s] ((get proc :implementation) ins ctx'))
          [[v o 0] o s]))

       ;; Otherwise, use default tracing
       true
       (infer-apply model-of-infer-apply [proc ins ctx] ctx')))))

;; Lambdas created by the interpreter as it evaluates `(gen ...)` expressions should
;; be lambdas in the host language (Clojure) as well. Ideally, they would be efficient
;; compiled versions of the generative code, just like when a `(gen ...)` expression
;; is evaluated outside the interpreter. But there are thorny issues surrounding mutual
;; recursion / forward references that make that difficult, so for now, the "compiled" version
;; simply invokes the tracing interpreter and throws away the trace.
(define compile-mp-proc
  (clojure.core/fn [proc]
    (clojure.core/with-meta
      (clojure.core/fn [& args]
        (nth (infer-apply
              proc (clojure.core/vec args)
              (assoc (make-top-level-tracing-context {} {}) :active? false)) 0))
      (unbox proc))))

                                        ; Invoke a "foreign" (i.e., Clojure) procedure, handling intervention and target traces.
(define infer-apply-foreign
  (gen [proc inputs ctx]
    ;; 'Foreign' generative procedure
    (define value (generate-foreign proc inputs))
    (define ivalue (if (intervened? ctx '()) (intervene-value ctx '()) value))
    [ivalue {} (if (and (targeted? ctx '()) (not= (target-value ctx '()) ivalue)) negative-infinity 0)]))

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
    (infer-eval (if (empty? (rest body))
                  (first body)
                  (cons 'block body)) ; body with implicit `block`
                new-env
                ctx)))

;; Apply proc at sub-adr in the tracing context tc, given that we are
;; currently in old-ctx.
(define infer-apply-tc
  (gen [tc [sub-adr proc & ins] old-ctx]
    (assert (captured-ctx? tc) "Using apply-tc on a non-captured tc.")
    (if (not= (get old-ctx :interpretation-id) (get tc :interpretation-id))
      [(apply tc (cons sub-adr (cons proc ins))) old-ctx 0]
      (block
       (define [out-atom score-atom applicator] (get tc :captured-state))
       (define modified-tc (subcontext (dissoc tc :captured-state) sub-adr))
       (define [v o s] (applicator proc ins modified-tc))
       (clojure.core/swap! out-atom trace-merge (maybe-set-subtrace {} sub-adr o)) ; TODO: Check this has the right behavior
       [v {} s]))))

(define infer-subeval
  (gen [sub-exp adr env ctx]
    (define [v sub-o s] (infer-eval sub-exp env (subcontext ctx adr)))
    [v (maybe-set-subtrace {} adr sub-o) s]))

(define infer-eval-expressions
  (gen [exp env ctx]
    (second
     (reduce
      (gen [[i [v o prev-s]] next]
        (define [next-v sub-o s]
          (infer-eval next env (subcontext ctx i)))
        [(+ i 1) [(clojure.core/conj v next-v) (maybe-set-subtrace o i sub-o) (+ s prev-s)]])
      [0 [[] {} 0]]
      exp))))

(define infer-eval-map
  (gen [exp env ctx]
    (define [_ answer]
      (clojure.core/doall (clojure.core/reduce-kv
                           (gen [[i [v o prev-s]] next-key next-val]
                             (define key-adr (str "key" i))
                             (define val-adr i)
                             (define [key-v key-o key-s]
                               (infer-eval next-key env (subcontext ctx key-adr)))
                             (define [val-v val-o val-s]
                               (infer-eval next-val env (subcontext ctx val-adr))) ; TODO: or "vali" for address
                             [(+ i 1)
                              [(clojure.core/conj v [key-v val-v])
                               (maybe-set-subtrace (maybe-set-subtrace {} key-adr key-o) val-adr val-o)
                               (+ prev-s key-s val-s)]])
                           [0 [{} {} 0]]
                           exp)))
    answer))


(define infer-eval
  (gen [exp env ctx]
    (assert (environment? env) ["bad env - eval" env])

    (define [v o s]
      (cond
        (variable? exp)
        [(env-lookup env exp) {} 0]

        (vector? exp)
        (infer-eval-expressions exp env ctx)

        (map? exp)
        (infer-eval-map exp env ctx)

        (or (not (clojure.core/seq? exp)) (= '() exp))
        [exp {} 0]

        (quote-expr? exp)
        [(quote-quoted exp) {} 0]

        (with-explicit-tracer-expr? exp)
        (block (define captured-ctx (capture-tracing-context
                                     ctx infer-apply))

               ;; Create an environment that has this captured context in it
               (define inactive-ctx (assoc ctx :active? false))

               (define new-env (make-env env))
               (match-bind! (explicit-tracer-var-name exp)
                            captured-ctx
                            new-env)

               ;; Evaluate the body
               (define [values _ s]
                 (infer-eval-expressions
                  (explicit-tracer-body exp)
                  new-env                    ;; has custom tracer
                  inactive-ctx))

               (define [o-acc score-acc] (release-tracing-context captured-ctx))
               [(clojure.core/last values) o-acc (+ s score-acc)])

        (if-expr? exp)
        (block (define [pred-value pred-trace pred-s]
                 (infer-eval (if-predicate exp) env (subcontext ctx "predicate")))
               (define clause-adr (if pred-value "then" "else"))
               (define [final-value clause-trace clause-s]
                 (infer-eval
                  (if pred-value (if-then-clause exp) (if-else-clause exp))
                  env (subcontext ctx clause-adr)))
               (define output-trace
                 (maybe-set-subtrace (maybe-set-subtrace {} clause-adr clause-trace) "predicate" pred-trace))
               [final-value output-trace (+ pred-s clause-s)])

        (definition? exp)
        (block (define [rhs-value out s]
                 (infer-subeval (definition-rhs exp) (name-for-definiens (definition-pattern exp)) env ctx))
               [(match-bind! (definition-pattern exp) rhs-value env) out s])

        (block-expr? exp)
        (block (define new-env (make-env env))
               (define [values o s]
                 (infer-eval-expressions (block-body exp) new-env ctx))
               [(clojure.core/last values) o s])

        (gen-expr? exp)
        [(compile-mp-proc
          {:name (trace-name exp)
           :generative-source exp, ; (cons 'gen (cons (second exp) (map mp-expand (rest (rest exp)))))
           :environment env}) {} 0]

        ;; It's an application:
        true
        (block (define key (application-result-key (first exp)))
               (define [evaluated o s] (infer-eval-expressions exp env ctx))

               (define [v app-o app-s] (infer-apply (first evaluated)
                                                    (rest evaluated)
                                                    (subcontext ctx key)))
               [v (maybe-set-subtrace o key app-o) (+ s app-s)])))

    ;; These may be nil, if no such value exists.
    (define ivalue (intervene-value ctx '()))
    (define tvalue (target-value ctx '()))

    (cond
      ;; intervention with no disagreeing target
      (and (intervened? ctx '())
           (or (not (targeted? ctx '()))
               (= ivalue tvalue)))
      [(or-nil? ivalue v) o 0]

      ;; target and value (from intervention or execution) disagree
      (and (targeted? ctx '()) (not= (or-nil? ivalue v) tvalue))
      (assert false (str "Unsatisfiable target constraint (target=" tvalue ", expected=", (or-nil? ivalue v) ")"))

      ;; in all other cases, the existing values work fine:
      true
      [(or-nil? ivalue v) o s])))

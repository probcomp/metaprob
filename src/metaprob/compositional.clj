(ns metaprob.compositional
  "A meta-circular Metaprob interpreter."
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [clojure.pprint :as pp]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.prelude :refer :all]
            [metaprob.frame :refer :all]))

;; ----------------------------------------------------------------------------

(declare infer-apply-native infer-apply-foreign
         infer-apply infer-eval infer-eval-sequence)

(define infer-apply
  "Main entry point: an `apply` that respects interventions and
  constraints, records choices made, and computes scores."

  (gen [proc inputs intervene target output?]
    (assert (or (list? inputs) (tuple? inputs))
            ["inputs neither list nor tuple" inputs])
    (assert (trace? intervene) ["bad intervene" intervene])
    (assert (trace? target) ["target" target])
    (assert (boolean? output?) output?)

    (if (and (trace? proc) (trace-has? proc "implementation"))
      ;; Proc is a special inference procedure returned by `inf`.
      ;; Return the value+output+score that the implementation computes.
      (block
        (define imp (trace-get proc "implementation"))
        (imp inputs intervene target output?))

      (cond
        ;; Bypass interpreter when there is no need to use it.  Was once
        ;; necessary in order for map and apply to work properly; it might
        ;; be possible to remove this check now that we have
        ;; *ambient-interpreter*.
        (and (foreign-procedure? proc)
             (empty-trace? intervene)
             (empty-trace? target)
             (not output?))
        (infer-apply-foreign proc inputs intervene target output?)

        ;; Native (interpreted) generative procedure.
        (native-procedure? proc)
        (infer-apply-native proc inputs intervene target output?)

        ;; Foreign (opaque, compiled) procedure.
        (foreign-procedure? proc)
        (infer-apply-foreign proc inputs intervene target output?)

        ;; Otherwise, this is not a procedure we can interpret.
        true
        (block (pprint proc)
               (error "infer-apply: not a procedure" proc))))))

(define infer-apply-foreign
  "Invoke a 'foreign' generative procedure, i.e. one written in
  clojure (or Java)"
  (gen [proc inputs intervene target output?]
    ;; 'Foreign' generative procedure
    (define value (generate-foreign proc inputs))
    (define ivalue (if (trace-has? intervene)
                     (trace-get intervene)
                     value))
    (if (and (trace-has? target)
             (not (same-states? (trace-get target) ivalue)))
      [(trace-get target)
       (trace-set (trace) (trace-get target))
       negative-infinity]
      [ivalue (trace) 0])))

(define infer-apply-native
  "Invoke a 'native' generative procedure, i.e. one written in
  metaprob, with inference mechanics (traces and scores)."
  (gen [proc inputs intervene target output?]
    (define source (trace-subtrace proc "generative-source"))
    (define environment (trace-get proc "environment"))
    (define new-env (make-env environment))
    ;; Extend the enclosing environment by binding formals to actuals
    (match-bind! (trace-subtrace source "pattern")
                 inputs
                 new-env)
    (infer-eval (trace-subtrace source "body")
                new-env
                intervene
                target
                output?)))

(define infer-subeval
  "Evaluate a subexpression (a subproblem)"
  (gen [exp key env intervene target output?]
    (define [value output score]
      (infer-eval (trace-subtrace exp key)
                  env
                  (maybe-subtrace intervene key)
                  (maybe-subtrace target key)
                  output?))
    [value (maybe-set-subtrace (trace) key output) score]))

(define infer-eval
  "Evaluate a subexpression (by reduction)"
  (gen [exp env intervene target output?]
    (assert (trace? exp) ["bad expression - eval" exp])
    (assert (environment? env) ["bad env - eval" env])
    (assert (trace? intervene) ["bad intervene" intervene])
    (assert (trace? target) ["bad target" target])
    (assert (boolean? output?) output?)

    (define [v output score]
      ;; Dispatch on type of expression
      (case (trace-get exp)

        ;; Application of a procedure to inputs (call)
        "application"
        (block (define [values output score]
                 (infer-eval-sequence exp env intervene target output?))

               (define result-key
                 (application-result-key (trace-subtrace exp 0)))

               (define [result suboutput subscore]
                 (infer-apply (first values)
                              (rest values)
                              (maybe-subtrace intervene result-key)
                              (maybe-subtrace target result-key)
                              output?))
               [result
                (if output?
                  (maybe-set-subtrace output result-key suboutput)
                  output)
                (+ subscore score)])

        "variable"
        [(env-lookup env (trace-get exp "name")) (trace) 0]

        "literal"
        [(trace-get exp "value") (trace) 0]

        ;; Gen-expression yields a generative procedure
        "gen"
        [(mutable-trace :value "prob prog"
                        "name" (trace-name exp)
                        "generative-source" (** exp)
                        "environment" env)
         (trace)
         0]

        ;; Conditional
        "if"
        (block
         (define [pred-val pred-output pred-score]
           (infer-subeval exp "predicate"
                          env intervene target output?))
         (define key
           (if pred-val "then" "else"))
         (define [val output score]
           (infer-subeval exp key
                          env intervene target output?))

         [val
          (trace-merge pred-output output)
          (+ pred-score score)])

        ;; Sequence of expressions and definitions
        "block"
        (block (define [values output score]
                 (infer-eval-sequence exp (make-env env) intervene target output?))

               [(last values) output score])

        ;; Definition: bind a variable to some value
        "definition"
        (block (define key
                 (name-for-definiens (trace-subtrace exp "pattern")))
               (define [val output score]
                 (infer-subeval exp key env intervene target output?))
               [(match-bind! (trace-subtrace exp "pattern") val env)
                output
                score])

        (block (pprint exp)
               (error "Not a code expression"))))

    (assert (trace? output) output)

    (define ivalue (if (trace-has? intervene) (trace-get intervene) v))
    (define tvalue (if (trace-has? target) (trace-get target) v))

    (cond
      ;; intervention with no disagreeing target
      (and (trace-has? intervene)
           (or (not (trace-has? target))
               (same-states? ivalue tvalue)))
      [ivalue
       (if (empty-trace? output) output (trace-set (trace) ivalue))
       0]

      ;; target and value (from intervention or execution) disagree
      (and (trace-has? target)
           (not (same-states? ivalue tvalue)))
      [tvalue
       (trace-set output tvalue)
       negative-infinity]

      ;; in all other cases, the existing values work fine:
      true
      [v output score])))

;; XXX jmt unused?
;; (define z (gen [n v]
;;   (assert (tuple? v) ["tuple" v])
;;   (assert (= (length v) n) ["tuple length" n v])
;;   v))

(define infer-eval-sequence
  "Evaluate a sequence of expressions."
  (gen [exp env intervene target output?]
    (assert (trace? exp) exp)
    (assert (gte (trace-count exp) 1) exp)

    (define luup
      (gen [i values output score]
        (if (trace-has? exp i)
          (block (define [val suboutput subscore]
                   (infer-subeval exp
                                  i
                                  env
                                  intervene
                                  target
                                  output?))
                 (luup (+ i 1)
                       (pair val values)
                       (trace-merge output suboutput)
                       (+ score subscore)))
          [(reverse values) output score])))
    (luup 0 (list) (trace) 0)))

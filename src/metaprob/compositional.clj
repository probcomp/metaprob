;; A meta-circular Metaprob interpreter.

(ns metaprob.compositional
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [clojure.pprint :as pp]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.prelude :refer :all]
            [metaprob.frame :refer :all]))

;; ----------------------------------------------------------------------------

(declare infer-apply-native infer-apply infer-eval infer-eval-sequence)

;; Main entry point: an `apply` that respects interventions
;; and constraints, records choices made, and computes scores.

;; Output-in-not-an-input version

(define infer-apply
  (gen [proc inputs intervene target output?]
    (assert (or (list? inputs) (tuple? inputs))
            ["inputs neither list nor tuple" inputs])
    (assert (trace? intervene) ["bad intervene" intervene])
    (assert (trace? target) ["target" target])
    (assert (boolean? output?) output?)
    (if (and (trace? proc) (trace-has? proc "implementation"))
      ;; Proc is a special inference procedure returned by `inf`.
      ;; Return the value+score that the implementation computes.
      
      (do (define imp (trace-get proc "implementation"))
          (assert (procedure? imp) ["what" imp])
          (imp inputs intervene target output?))
      (if (and (foreign-procedure? proc)
               (empty-trace? intervene)
               (empty-trace? target)
               (not output?))
        ;; Bypass interpreter when there is no need to use it.
        ;; -- This already gets done in builtin/infer-apply - remove this check.
        [(generate-foreign proc inputs) (trace) 0]
        (block
         ;; Proc is a generative procedure, either 'foreign' (opaque, compiled)
         ;; or 'native' (interpreted).
         ;; First call the procedure.  We can't skip the call when there
         ;; is an intervention, because the call might have side effects.
         (define [value output score]
           (if (native-procedure? proc)
             ;; 'Native' generative procedure.  Intervention happens therein.
             (infer-apply-native proc inputs intervene target output?)
             (if (foreign-procedure? proc)
               ;; 'Foreign' generative procedure
               (block (define value (generate-foreign proc inputs))
                      (define ivalue (if (trace-has? intervene) (trace-get intervene) value))
                      [ivalue
                       (if output?
                         (trace :value ivalue)
                         (trace))
                       0])
               (block (pprint proc)
                      (error "infer-apply: not a procedure" proc)))))
         ;; Apply target trace to get modified value and score
         (if (trace-has? target)
           [(trace-get target)
            ;; Note: Alexey's version only sets output for primitives.
            ;; This might matter!
            (if output?
              (trace-set output (trace-get target))
              output)
            (if (trace-has? intervene)
              ;; Score goes infinitely bad if there is both an
              ;; intervention and a constraint, and they differ
              (if (same-trace-states? (trace-get target) value)
                score
                (do (print ["value mismatch!"
                            (trace-get target)
                            value])
                    negative-infinity))
              score)]
           [value output score]))))))

;; Invoke a 'native' generative procedure, i.e. one written in
;; metaprob, with inference mechanics (traces and scores).

(define infer-apply-native
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

;; Evaluate a subexpression (a subproblem)

(define infer-subeval
  (gen [exp key env intervene target output?]
    (define [value output score]
      (infer-eval (trace-subtrace exp key)
                  env
                  (maybe-subtrace intervene key)
                  (maybe-subtrace target key)
                  output?))
    [value (maybe-set-subtrace (trace) key output) score]))

;; Evaluate a subexpression (by reduction)

(define infer-eval
  (gen [exp env intervene target output?]
    (assert (trace? exp) ["bad expression - eval" exp])
    (assert (environment? env) ["bad env - eval" env])
    (assert (trace? intervene) ["bad intervene" intervene])
    (assert (trace? target) ["bad target" target])
    (assert (boolean? output?) output?)

    (define [v output score]
      ;; Dispatch on type of expression
      (case (trace-get exp)

        ;; Application of a procedure to arguments (call)
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
                  (trace-set-subtrace output result-key suboutput)
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

    (define v (if (trace-has? intervene)
                (trace-get intervene)
                v))

    [v
     ;; Alexey's version doesn't do this - remove?
     (if (and output? (not (empty-trace? output)))
       (trace-set output v)
       output)
     score]))

(define z (gen [n v]
  (assert (tuple? v) ["tuple" v])
  (assert (= (length v) n) ["tuple length" n v])
  v))

(define infer-eval-sequence
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

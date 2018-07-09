;; A meta-circular Metaprob interpreter.

;; Although this file will almost always run as clojure code, it
;; is a goal to retain the ability to run it as metaprob
;; (i.e. to make it self-applicable!).  This feature has not yet been
;; tested.

(ns metaprob.infer
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [clojure.pprint :as pp]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.prelude :refer :all]
            [metaprob.frame :refer :all]))

;; Similar to `lookup` but does not create locatives

(define subtrace-maybe
  (gen [tr adr]
    (if (and tr (trace-has-subtrace? tr adr))
      (trace-subtrace tr adr)
      nil)))

;; Extend an address.

(define extend-addr
  (gen [adr key]
    (add adr (addr key))))

;; ----------------------------------------------------------------------------

(declare infer-apply-native infer-eval)

;; Useful invariant: if output is non-nil, then on returning [value score],
;; we have value = (trace-ref output).

;; Main entry point: a version of `apply` that respects interventions
;; and constraints, records choices, and computes scores.

;; Returns [value score]

(define infer-apply-locally
  (gen [proc inputs intervene target output]
    (assert (or (list? inputs) (tuple? inputs))
            ["inputs neither list nor tuple" inputs])
    (assert (or (eq output nil)
                (mutable-trace? output))
            output)
    (if (and (trace? proc) (trace-has? proc "implementation"))
      ;; Proc is a special inference procedure returned by `inf`, called
      ;; using 'standard' protocol.  So we need to adapt the inputs 
      ;; going in, and the results coming out, to implement the change
      ;; of protocol.
      ;; Return the value+score that the implementation computes.
      (block (define impl (trace-get proc "implementation"))
             (assert (procedure? impl) ["what" impl])
             (define [value out score]
               ;; Maybe not the right thing here.  Really we're
               ;; calling the interpreter at the next outer level.
               (impl inputs
                     (if (= intervene nil) (trace) intervene)
                     (if (= target nil) (trace) target)
                     (not (= output nil))))
             (if (and output out)
               (trace-merge! output out))
             [value score])
      (if (and (foreign-procedure? proc)
               (not (or intervene target output)))
        ;; Bypass interpreter when there is no need to use it.
        ;; -- This already gets done in builtin/infer-apply - remove this check.
        [(generate-foreign proc inputs) 0]
        (block
         ;; Proc is a generative procedure, either 'foreign' (opaque, compiled)
         ;; or 'native' (interpreted).
         ;; First call the procedure.  We can't skip the call when there
         ;; is an intervention, because the call might have side effects.
         (define [value score]
           (if (native-procedure? proc)
             ;; 'Native' generative procedure
             (infer-apply-native proc inputs intervene target output)
             (if (foreign-procedure? proc)
               ;; 'Foreign' generative procedure - store result in
               ;; output trace

               (block (define value (generate-foreign proc inputs))    ;Side effects
                      ;; For native procedures, interventions are handled in eval
                      (define ivalue
                        (if (and intervene (trace-has? intervene))
                          (trace-get intervene)
                          value))
                      (if output
                        (trace-set! output ivalue))
                      [ivalue 0])

               (block (pprint proc)
                      (error "infer-apply: not a procedure" proc)))))
         ;; Apply intervention trace to get modified value
         (define intervention? (and intervene (trace-has? intervene)))
         (define post-intervention-value
           (if intervention?
             (trace-get intervene)
             value))
         ;; Apply target trace to get modified value and score
         (if (and target (trace-has? target))
           (block (if output
                    (trace-set! output (trace-get target)))
                  [(trace-get target)
                   (if intervention?
                     ;; Score goes infinitely bad if there is both an
                     ;; intervention and a constraint, and they differ
                     (if (same-trace-states? (trace-get target) post-intervention-value)
                       score
                       (do (print ["value mismatch!"
                                   (trace-get target)
                                   post-intervention-value])
                           negative-infinity))
                     score)])
           ;; Else
           [post-intervention-value score]))))))

;; Invoke a 'native' generative procedure, i.e. one written in
;; metaprob, with inference mechanics (traces and scores).

(define infer-apply-native
  (gen [proc
        inputs
        intervene
        target
        output]
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
                output)))

;; Evaluate the body of a 'native' procedure by recursive descent.

(define infer-eval
  (gen [exp env intervene target output]
    (assert (or (eq output nil)
                (mutable-trace? output))
            output)
    (define walk
      (gen [exp env address]
        (assert (trace? exp) ["bad trace - eval" exp address])
        ;; (print ["eval" (trace-get exp) address])
        ;; (pprint exp)
        (define [v score]
          ;; Dispatch on type of expression
          (case (trace-get exp)

            ;; Application of a procedure to arguments (call)
            "application"
            (block (define subscore (empty-trace 0))
                   ;; Evaluate all subexpressions, including the procedure
                   ;; position
                   (define values
                     (map (gen [i]
                            (define [v s]
                              (walk (trace-subtrace exp i)
                                    env
                                    (extend-addr address i)))
                            (trace-set! subscore (+ (trace-get subscore) s))
                            v)
                          (range (trace-count exp))))
                   (define new-addr
                     (extend-addr address
                                  (application-result-key (trace-subtrace exp 0))))
                   (define [val score]
                     (infer-apply-locally (first values)
                                          (rest values)
                                          (subtrace-maybe intervene new-addr)
                                          (subtrace-maybe target new-addr)
                                          (lookup output new-addr)))
                   [val (+ (trace-get subscore) score)])

            "variable"
            [(env-lookup env (trace-get exp "name")) 0]

            "literal"
            [(trace-get exp "value") 0]

            ;; Gen-expression yields a generative procedure
            "gen"
            [(mutable-trace :value "prob prog"
                            "name" (trace-name exp)
                            "generative-source" (** exp)
                            "environment" env)
             0]

            ;; Conditional
            "if"
            (block
             (define [pred pred-score]
               (walk (trace-subtrace exp "predicate") env
                     (extend-addr address "predicate")))
             (if pred
               (block (define [val score]
                        (walk (trace-subtrace exp "then") env
                              (extend-addr address "then")))
                      [val (+ pred-score score)])
               (block (define [val score]
                        (walk (trace-subtrace exp "else") env
                              (extend-addr address "else")))
                      [val (+ pred-score score)])))

            ;; Sequence of expressions and definitions
            "block"
            (block (define new-env (make-env env))
                   (define subscore (empty-trace 0))
                   (define values
                     ;; This assumes that map is left to right!
                     (map (gen [i]
                            (define [v s]
                              (walk (trace-subtrace exp i) new-env
                                    (extend-addr address i)))
                            (trace-set! subscore
                                        (+ (trace-get subscore) s))
                            v)
                          (range (trace-count exp))))
                   (if (gt (length values) 0)
                     [(last values) (trace-get subscore)]
                     [(empty-trace) (trace-get subscore)]))

            ;; Definition: bind a variable to some value
            "definition"
            (block (define key
                     (name-for-definiens
                      (trace-subtrace exp "pattern")))
                   ;; (print ["definiens =" key])
                   (define [val score]
                     (walk (trace-subtrace exp key) env
                           (extend-addr address key)))
                   [(match-bind! (trace-subtrace exp "pattern")
                                 val
                                 env)
                    score])

            (block (pprint exp)
                   (error "Not a code expression"))))

        ;; Compare above code for foreign generative call
        (if (and intervene (trace-has? intervene address))
          [(trace-get intervene address) score]
          [v score])))
    (walk exp env (addr))))

;; Variant using 'standard' interface

(define infer-apply
  (gen [proc inputs intervene target output?]
    (define output (if output? (empty-trace) nil))
    (define [value score]
      (infer-apply-locally proc inputs
                           (if (empty-trace? intervene) nil intervene)
                           (if (empty-trace? target) nil target)
                           output))
    (assert (number? score) score)
    [value output score]))

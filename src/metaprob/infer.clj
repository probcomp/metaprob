;; The meta-circular Metaprob interpreter.

;; Although this file will almost always run as clojure code, it
;; is a goal to retain the ability to run it as metaprob
;; (i.e. to make it self-applicable!).  This feature has not yet been
;; tested.

(ns metaprob.infer
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [clojure.pprint :as pp]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare map)

;; ----------------------------------------------------------------------------
;; Lexical environments, needed by gen macro.
;; TBD: Move to prelude?

(define frame?
  (gen [obj]
    (if (trace? obj)
      (trace-has-subtrace? obj "*parent*")
      false)))

(define environment?
  (gen [obj]
    (or (frame? obj)
        (top-level-environment? obj))))

(define frame-parent
  (gen [frame]
    (trace-get frame "*parent*")))

(define make-env
  (gen [parent]
    (assert (environment? parent) parent)
    (mutable-trace "*parent*" (** (trace :value parent)))))

(define env-lookup
  (gen [env name]
    (if (frame? env)
      (if (trace-has? env name)
        (trace-get env name)
        (env-lookup (frame-parent env) name))
      ;; Top level environment
      (top-level-lookup env name))))

(define env-bind!
  (gen [env name val]
    (if (frame? env)
      (trace-set! env name val)
      (assert false "bad env-bind!"))))

;; match-bind! - overrides original prelude.
;; Liberal in what it accepts: the input can be either a list or a
;; tuple, at any level.

(define match-bind!
  ;; pattern is a parse-tree trace (variable or tuple expression) - not a tuple.
  ;; input is anything.
  (gen [pattern input env]
    (case (trace-get pattern)
      "variable"
      (env-bind! env (trace-get pattern "name") input)
      "tuple"
      (block (define count (trace-count pattern))

             (define loup
               (gen [i cursor]
                 (cond (eq i count)
                       ;; We've reached the end of the patterns
                       (assert (empty-trace? cursor)
                               ["too many inputs"
                                (length input)
                                count
                                (clojure.core/map to-immutable
                                                  (to-immutable input))
                                pattern
                                env])

                       ;; The pattern [& x] matches anything
                       (and (eq i (sub count 2))
                            (eq (trace-get pattern i) "&"))
                       (match-bind! (trace-subtrace pattern (+ i 1))
                                    cursor
                                    env)

                       ;; Ensure that an input remains to match remaining pattern
                       (empty-trace? cursor)
                       (assert false
                               ["too few inputs"
                                (length input)
                                count
                                (clojure.core/map to-immutable
                                                  (to-immutable input))
                                pattern
                                env])

                       ;; Bind pattern to input, and continue
                       true
                       (block (match-bind! (trace-subtrace pattern i) (first cursor) env)
                              (loup (+ i 1) (rest cursor))))))

             (loup 0 (to-list input)))
      (do (pprint pattern)
          (assert false ["bad pattern" pattern input])))
    "return value of match-bind!"))

;; Similar to `lookup` but does not create locatives

(define maybe-subtrace
  (gen [tr adr]
    (if (and tr (trace-has-subtrace? tr adr))
      (trace-subtrace tr adr)
      nil)))

;; -----------------------------------------------------------------------------
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

(define maybe-subtrace
  (gen [tr adr]
    (if (trace-has-subtrace? tr adr)
      (trace-subtrace tr adr)
      (trace))))

(define maybe-set-subtrace
  (gen [output key suboutput]
    (assert (trace? output) "output")
    (assert (trace? suboutput) "suboutput")
    (if (empty-trace? suboutput)
      output
      (trace-set-subtrace output key suboutput))))

;; ----------------------------------------------------------------------------

(declare infer-apply-native infer-apply-neuf infer-eval infer-eval-sequence)

;; Main entry point: an `apply` that respects interventions
;; and constraints, records choices made, and computes scores.

;; Backward compatible version

(define infer-apply
  (gen [proc inputs intervene target output-place]
    (define [val output score]
      (infer-apply-neuf proc inputs
                        (or intervene (trace))
                        (or target (trace))
                        (if output-place true false)))
    (if output-place
      (trace-update! output-place output))
    [val score]))

;; Output-not-an-input version

(define infer-apply-neuf
  (gen [proc inputs intervene target output?]
    (assert (or (list? inputs) (tuple? inputs))
            ["inputs neither list nor tuple" inputs])
    (assert (trace? intervene) ["intervene" intervene])
    (assert (trace? target) ["target" target])
    (assert (boolean? output?) output?)
    (if (and (trace? proc) (trace-has? proc "infer-method"))
      ;; Proc is a special inference procedure returned by `inf`.
      ;; Return the value+score that the infer-method computes.
      ((trace-get proc "infer-method") inputs intervene target output?)
      (if (and (foreign-procedure? proc)
               (empty-trace? intervene)
               (empty-trace? target)
               (not output?))
        ;; Bypass interpreter when there is no need to use it.
        [(generate-foreign proc inputs) (trace) 0]
        (block
         ;; Proc is a generative procedure, either 'foreign' (opaque, compiled)
         ;; or 'native' (interpreted).
         ;; First call the procedure.  We can't skip the call when there
         ;; is an intervention, because the call might have side effects.
         (define [value output score]
           (if (and (trace? proc)
                    (trace-has? proc "generative-source")
                    (trace-has? proc "environment"))
             ;; 'Native' generative procedure
             (infer-apply-native proc inputs intervene target output?)
             (if (foreign-procedure? proc)
               ;; 'Foreign' generative procedure
               (block (define value (generate-foreign proc inputs))
                      [(if (trace-has? intervene) (trace-get intervene) value)
                       intervene
                       0])
               (block (pprint proc)
                      (error "infer-apply: not a procedure" proc)))))
         ;; Apply target trace to get modified value and score
         (define [post-target-value score2]
           (if (trace-has? target)
             [(trace-get target)
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
             [value score]))
         (assert (trace? output) ["lose" output? output])
         [post-target-value
          (if output?
            (trace-set output post-target-value)
            output)
          score2])))))

;; Invoke a 'native' generative procedure, i.e. one written in
;; metaprob, with inference mechanics (traces and scores).

(define infer-apply-native
  (gen [proc
        inputs
        intervene
        target
        output?]
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
                 (infer-apply-neuf (first values)
                                   (rest values)
                                   (maybe-subtrace intervene result-key)
                                   (maybe-subtrace target result-key)
                                   output?))
               [result
                (if output?
                  (trace-set output result-key suboutput)
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
                       (trace-merge suboutput output)
                       (+ score subscore)))
          [(reverse values) output score])))
    (luup 0 (list) (trace) 0)))
>>>>>>> tests run with new eval protocol


;; -----------------------------------------------------------------------------

(define inf-neuf
  (gen [name infer-method]
    (assert (procedure? infer-method) infer-method)
    (trace-as-procedure (mutable-trace "name" (add "inf-" (procedure-name infer-method))
                                       "infer-method" infer-method)
                        ;; When called from Clojure:
                        (gen [& inputs]
                          (nth (infer-method inputs (trace) (trace) false)
                               0)))))

(define inf
  (gen [name infer-method-classic]
    (assert (procedure? infer-method-classic) infer-method-classic)
    (inf-neuf name
              ;; 2nd arg to inf-neuf is new-style infer method, which is a 
              ;;  deterministic procedure
              (gen [inputs intervene target output?]
                (define output (mutable-trace))
                (define [value score]
                  ;; Call old-style infer method using old protocol
                  (infer-method-classic inputs
                                        (if (empty-trace? intervene)
                                          nil
                                          intervene)
                                        (if (empty-trace? target)
                                          nil
                                          target)
                                        (if output?
                                          output
                                          nil)))
                [value output score]))))

;; Experimental

(define opaque
  (gen [name proc]
    (inf name
         (gen [inputs intervene target output]
           ;; Ignore the traces.
           (infer-apply proc inputs nil nil nil)))))

(define apply
  (trace-as-procedure
   (inf-neuf "apply"
             (gen [inputs intervene target output?]
               (infer-apply-neuf (first inputs) (rest inputs) intervene target output?)))
   ;; Kludge
   (gen [proc inputs] (clojure.core/apply proc (to-immutable-list inputs)))))


;; map defined using inf (instead of with-address)

(define map
  (inf-neuf "map"
            (gen [[fun sequ] intervene target output?]
              (block (define re
                       (gen [i l]
                         (if (pair? l)
                           (block (define [value suboutput subscore]
                                    (infer-apply-neuf fun
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

(define replicate
  (gen [n f]
    (map (gen [i] (f))
         (range n))))

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
    (mutable-trace "*parent*" parent)))

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

(define infer-apply
  (gen [proc inputs intervene target output]
    (assert (or (list? inputs) (tuple? inputs))
            ["inputs neither list nor tuple" inputs])
    (assert (or (eq output nil)
                (mutable-trace? output))
            output)
    (if (and (trace? proc) (trace-has? proc "infer-method"))
      ;; Proc is a special inference procedure returned by `inf`.
      ;; Return the value+score that the infer-method computes.
      ((trace-get proc "infer-method") inputs intervene target output)
      (if (and (foreign-procedure? proc)
               (not (or intervene target output)))
        [(generate-foreign proc inputs) 0]
        (block
         ;; Proc is a generative procedure, either 'foreign' (opaque, compiled)
         ;; or 'native' (interpreted).
         ;; First call the procedure.  We can't skip the call when there
         ;; is an intervention, because the call might have side effects.
         (define [value score]
           (if (and (trace? proc)
                    (trace-has? proc "generative-source")
                    (trace-has? proc "environment"))
             ;; 'Native' generative procedure
             (infer-apply-native proc inputs intervene target output)
             (if (foreign-procedure? proc)
               ;; 'Foreign' generative procedure
               [(generate-foreign proc inputs) 0]
               (block (pprint proc)
                      (error "infer-apply: not a procedure" proc)))))
         ;; Apply intervention trace to get modified value
         (define intervention? (and intervene (trace-has? intervene)))
         (define post-intervention-value
           (if intervention?
             (trace-get intervene)
             value))
         ;; Apply target trace to get modified value and score
         (define [post-target-value score2]
           (if (and target (trace-has? target))
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
                score)]
             [post-intervention-value score]))
         ;; Store value in output trace
         (if output
           (trace-set! output post-target-value))
         [post-target-value score2])))))

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
                     (infer-apply (first values)
                                  (rest values)
                                  (maybe-subtrace intervene new-addr)
                                  (maybe-subtrace target new-addr)
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
        (if (and intervene (trace-has? intervene address))
          [(trace-get intervene address) score]
          [v score])))
    (walk exp env (addr))))

(define inf
  (gen [name infer-method]
    (trace-as-procedure (mutable-trace "name" (add "inf-" (procedure-name infer-method))
                                       "infer-method" infer-method)
                        ;; When called from Clojure:
                        (gen [& inputs]
                          (nth (infer-method inputs nil nil nil)
                               0)))))

(define apply
  (trace-as-procedure
   (inf "apply"
        (gen [inputs intervene target output]
          (infer-apply (first inputs) (rest inputs) intervene target output)))
   ;; Kludge
   (gen [proc inputs] (clojure.core/apply proc (to-immutable-list inputs)))))


;; map defined using inf (instead of with-address)

(define map-issue-20
  (inf "map"
       (gen [[fun sequ] intervene target output]
         (block (define re
                  (gen [l i]
                    (if (pair? l)
                      (block (define suboutput (if output (lookup output i) nil))
                             (define [valu subscore]
                               (infer-apply fun
                                            [(first l)]
                                            ;; advance traces by address i
                                            (maybe-subtrace intervene i)
                                            (maybe-subtrace target i)
                                            suboutput))
                             (if (and output suboutput)
                               (trace-set! output i suboutput))
                             (define [more-valu more-score]
                               (re (rest l) (+ i 1)))
                             [(pair valu more-valu)
                              (+ subscore more-score)])
                      [l 0])))
                (if (tuple? sequ)
                  (to-tuple (re (to-list sequ) 0))
                  (re sequ 0))))))

(define map map-issue-20)

(define replicate
  (gen [n f]
    (map (gen [i] (f))
         (range n))))


;; Experimental

(define opaque
  (gen [name proc]
    (inf name
         (gen [inputs intervene target output]
           ;; Ignore the traces.
           (infer-apply proc inputs nil nil nil)))))

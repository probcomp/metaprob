;; The meta-circular Metaprob interpreter.

;; Although this file will almost always run as clojure code, it
;; is a goal to retain the ability to run it as metaprob
;; (i.e. to make it self-applicable!).  This feature has not yet been
;; tested.

(ns metaprob.infer
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

;; ----------------------------------------------------------------------------
;; Lexical environments, needed by gen macro.
;; TBD: Move to prelude?

(define frame?
  (gen [obj]
    (if (trace? obj)
      (trace-has-subtrace? obj "parent")
      false)))

(define frame-parent
  (gen [frame]
    (trace-get frame "parent")))

(define env-lookup
  (gen [env name]
    (if (frame? env)
      (if (trace-has? env name)
        (trace-get env name)
        (env-lookup (frame-parent env) name))
      ;; Top level environment
      (top-level-lookup env name))))

;; make-env - overrides original prelude

(define make-env
  (gen [parent]
    (define env (empty-trace))
    (trace-set env "parent" parent)
    env))

(define env-bind!
  (gen [env name val]
    (if (frame? env)
      (trace-set env name val)
      (assert false "bad env-bind!"))))

;; match-bind - overrides original prelude.
;; Liberal in what it accepts: the input can be either a list or a
;; tuple, at any level.

(define match-bind
  ;; pattern is a parse-tree trace (variable or tuple expression) - not a tuple.
  ;; inputs is a seq, I think
  (gen [pattern input env]
    (if (eq (trace-get pattern) "variable")
      (env-bind! env (trace-get pattern "name") input)
      (if (eq (trace-get pattern) "tuple")
        (block (define inputs (to-list input))
               (define subpatterns
                 ;; Clojure map always returns a seq.
                 (map (gen [i]
                        (lookup pattern i))
                      ;; Ugh.  Gotta be a better way to do this
                      (range (length (trace-keys pattern)))))
               (if (not (eq (length subpatterns) (length inputs)))
                 (assert false
                         ["number of subpatterns differs from number of input parts"
                          (make-immutable pattern)
                          (length (trace-keys pattern))
                          (clojure.core/map make-immutable
                               (make-immutable inputs))
                          (length inputs)
                          env]))
               ;; TBD: handle [x & y] properly
               (for-each2 (gen [p i]
                            (assert (not (eq p "&")) "NYI")
                            (match-bind p i env))
                          subpatterns
                          inputs))
        (assert false ["bad pattern" pattern input])))
    "return value of match-bind"))

;; -----------------------------------------------------------------------------
;; Utilities for interpreter

;; This is used to compute the key under which to store the value that
;; is the binding of a definition.

(define name-for-definiens
  (gen [pattern]
    (if (eq (trace-get pattern) "variable")
      (if (neq (trace-get pattern "name") "_")
        (addr (trace-get pattern "name"))
        (addr "definiens"))
      (addr "definiens"))))

;; The tag address business, needed for the implementation of 'this'
;; and 'with-address'

;; capture-tag-address - used in interpretation of `this`.
;; Combine the return value with an address to make a 'quasi-address'
;; to pass to resolve-tag-address, e.g.
;;   (pair (capture-tag-address ...) (addr ...))
;; This is rather hacky; there ought to be a better way to do this.

(define capture-tag-address
  (gen [intervene target output]
    ;; Cannot freeze, freezing is hereditary
    ;; Ergo, these things can't go into addresses (addr)
    (immutable-trace "intervention-trace" intervene
                     "target-trace" target
                     "output-trace" output
                     :value "captured tag address")))

;; resolve-tag-address
;; Convert a quasi-address, whose first element was returned by 
;; capture-tag-address, into the appropriate trace

(define resolve-tag-address
  (gen [quasi-addr]
    (define captured (first quasi-addr))
    (define more (rest quasi-addr))
    (define intervene (trace-get captured "intervention-trace"))
    (define target (trace-get captured "target-trace"))
    (define output (trace-get captured "output-trace"))
    [(if intervene (lookup intervene more) nil)
     (if target (lookup target more) nil)
     (if output (lookup output more) nil)]))

;; Get the key to use for storing the result of a procedure call.

(define procedure-key
  (gen [exp]
    (if (eq (trace-get exp) "variable")
      (trace-get exp "name")
      "call")))

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
    (if (and (trace? proc) (trace-has? proc "infer-method"))
      ;; Proc is a special inference procedure returned by `inf`.
      ;; Return the value+score that the infer-method computes.
      ((trace-get proc "infer-method")
       inputs intervene target output)
      (block
       ;; Proc is a generative procedure, either 'foreign' (opaque, compiled)
       ;; or 'native' (interpreted).
       ;; First call the procedure.  We can't skip the call when there
       ;; is an intervention, because the call might have side effects.
       (define [value score]
         (if (and (trace? proc) (trace-has? proc "generative-source"))
           ;; 'Native' procedure
           (infer-apply-native proc inputs intervene target output)
           (if (foreign-procedure? proc)
             ;; 'Foreign' procedure
             [(generate-foreign proc inputs) 0]
             (block (pprint proc)
                    (error "infer-apply: not a procedure" proc)))))
       ;; Potentially modify value based on intervention trace
       (define intervention? (and intervene (trace-has? intervene)))
       (define intervention-value
         (if intervention?
           (trace-get intervene)
           value))
       ;; Potentially modify value (and score) based on target trace
       (define [value2 score2]
         (if (and target (trace-has? target))
           [(trace-get target)
            (if intervention?
              ;; Score goes infinitely bad if there is both an
              ;; intervention and a constraint, and they differ
              (if (same-trace-states? (trace-get target) intervention-value)
                score
                (do (print ["value mismatch!" (trace-get target) intervention-value])
                    negative-infinity))
              score)]
           [intervention-value score]))
       ;; Store value in output trace
       (if output
         (trace-set output value2))
       [value2 score2]))))

;; Invoke a 'native' generative procedure, i.e. one written in
;; metaprob, with inference mechanics (traces and scores).

(define infer-apply-native
  (gen [proc
        inputs
        intervene
        target
        output]
    (define source (lookup proc "generative-source"))
    (define environment (trace-get proc "environment"))
    (define new-env (make-env environment))
    ;; Extend the enclosing environment by binding formals to actuals
    (match-bind (lookup source "pattern")
                inputs
                new-env)
    (infer-eval (lookup source "body")
                new-env
                intervene
                target
                output)))

;; Evaluate the body of a 'native' procedure by recursive descent.

(define infer-eval
  (gen [exp env intervene target output]
    (define walk
      (gen [exp env address]
        (define [v score]
          ;; Dispatch on type of expression
          (case (trace-get exp)

            ;; Application of a procedure to arguments (call)
            "application"
            (block (define n (length (trace-keys exp)))
                   (define subscore (empty-trace))
                   (trace-set subscore 0)
                   ;; Evaluate all subexpressions, including the procedure
                   ;; position
                   (define values
                     (map (gen [i]
                            (define [v s]
                              (walk (trace-subtrace exp i)
                                    env
                                    (add address (addr i))))
                            (trace-set subscore (add (trace-get subscore) s))
                            v)
                          (range n)))
                   (define new-addr
                     (add address (addr (procedure-key (trace-subtrace exp 0)))))
                   (define [val score]
                     (infer-apply (first values)
                                  (rest values)
                                  (and intervene (lookup intervene new-addr))
                                  (and target (lookup target new-addr))
                                  (and output (lookup output new-addr))))
                   [val (add (trace-get subscore) score)])

            "variable"
            [(env-lookup env (trace-get exp "name")) 0]

            "literal"
            [(trace-get exp "value") 0]

            ;; Gen-expression yields a generative procedure
            "gen"
            (block (define proc
                     (trace :value "prob prog"
                            "name" (trace-name exp)
                            "generative-source" (** exp)
                            "environment" env))
                   [(trace-as-procedure proc
                                        ;; What to do when called directly
                                        ;; as a clojure function (this
                                        ;; should happen rarely if ever)
                                        (gen [& inputs]
                                          (nth (infer-apply proc inputs
                                                            nil nil nil)
                                               0)))
                    0])

            ;; Conditional
            "if"
            (block
             (define [pred pred-score]
               (walk
                (lookup exp "predicate") env
                (add address (addr "predicate"))))
             (if pred
               (block
                (define [val score]
                  (walk (lookup exp "then") env
                        (add address (addr "then"))))
                [val (add pred-score score)])
               (block
                (define [val score]
                  (walk (lookup exp "else") env
                        (add address (addr "else"))))
                [val (add pred-score score)])))

            ;; Sequence of expressions and definitions
            "block"
            (block (define n (length (trace-keys exp)))
                   (define new-env (make-env env))
                   (define subscore (empty-trace))
                   (trace-set subscore 0)
                   (define values
                     (map          ;; How do we know map is left to right?
                      (gen [i]
                        (define [v s]
                          (walk (lookup exp i) new-env
                                (add address (addr i))))
                        (trace-set
                         subscore
                         (add (trace-get subscore) s))
                        v)
                      (range n)))
                   (if (gt (length values) 0)
                     [(last values) (trace-get subscore)]
                     [(empty-trace) (trace-get subscore)]))

            ;; Definition: bind a variable to some value
            "definition"
            (block (define subaddr
                     (name-for-definiens
                      (lookup exp "pattern")))
                   (define [val score]
                     (walk (lookup exp subaddr) env
                           (add address subaddr)))
                   [(match-bind
                     (lookup exp "pattern")
                     val
                     env)
                    score])

            ;; `this` is the current location in the traces
            "this"
            [(capture-tag-address intervene target output)
             0]

            ;; `with-address` makes use of a location previously
            ;; captured by `this`
            "with-address"
            (block (define [tag-addr tag-score]
                     (walk (lookup exp "tag") env
                           (add address (addr "tag"))))
                   ;; tag-addr is actually a quasi-address 
                   ;; (captured . address) - not an address.
                   ;; Must be constructed using (pair x (addr ...))
                   (define [new-intervene new-target new-output]
                     (resolve-tag-address tag-addr))
                   (define [val score]
                     (infer-eval (lookup exp "expression")
                                 env
                                 new-intervene
                                 new-target
                                 new-output))
                   [val (add tag-score score)])

            (block (pprint exp)
                   (error "Not a code expression"))))
        (if (and intervene (trace-has? intervene address))
          [(trace-get intervene address) score]
          [v score])))
    (walk exp env (addr))))

(define inf
  (gen [name infer-method]
    (define tr (empty-trace))
    (trace-set tr "name" (add "inf-" (procedure-name infer-method)))
    (trace-set tr "infer-method" infer-method)
    (trace-as-procedure tr
                        ;; When callsed from Clojure:
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


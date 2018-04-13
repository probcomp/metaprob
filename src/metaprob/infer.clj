;; This file was automatically generated, then edited

;; Although this will almost alway run as clojure code, it
;; is a goal to retain the ability to run it as metaprob
;; (i.e. to make it self-applicable!).

(ns metaprob.infer
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

;; ----------------------------------------------------------------------------
;; Lexical environments, needed by gen macro.
;; TBD: Move to prelude

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

;; Manual edit: moved from interpret.clj

(define name_for_definiens
  (gen [pattern]
    (if (eq (trace-get pattern) "variable")
      (if (neq (trace-get pattern "name") "_")
        (addr (trace-get pattern "name"))
        (addr "definiens"))
      (addr "definiens"))))

;; -----------------------------------------------------------------------------
;; The tag address business

;; capture-tag-address - overrides original prelude - but definition is the same.
;; Used in interpretation of `this`.
;; Use as first element of an address, e.g. (addr (capture-tag-address ...) ...).

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
  (gen [quasi_addr]
    (define captured (first quasi_addr))
    (define more (rest quasi_addr))
    (define intervene (trace-get captured "intervention-trace"))
    (define target (trace-get captured "target-trace"))
    (define output (trace-get captured "output-trace"))
    [(if intervene (lookup intervene more) nil)
     (if target (lookup target more) nil)
     (if output (lookup output more) nil)]))

(define procedure-key
  (gen [exp]
    (if (eq (trace-get exp) "variable")
      (trace-get exp "name")
      "call")))

;; ----------------------------------------------------------------------------

(declare infer-apply-native infer-eval)

;; Useful invariant: if output is non-nil, then on returning [value score],
;; we have value = (trace-ref output).

;; infer
(define infer-apply
  (gen [prog inputs intervene target output]
    (assert (or (list? inputs) (tuple? inputs)) ["inputs neither list nor tuple" inputs])
    (if (if (trace? prog) (trace-has? prog "infer-method") false)
      ;; return the value+score that the infer-method computes
      ((trace-get prog "infer-method")
       inputs intervene target output)
      (block
       ;; Call the procedure (n.b. it might have side effects)
       (define [value score]
         (if (and (trace? prog) (trace-has? prog "generative-source"))
           (infer-apply-native prog inputs intervene target output)
           (if (foreign-procedure? prog)
             [(generate-foreign prog inputs) 0]
             (block (pprint prog)
                    (error "infer-apply: not a procedure" prog)))))
       ;; Potentially modify value based on intervention trace
       (define intervention? (and intervene (trace-has? intervene)))
       (define intervention-value
         (if intervention?
           (trace-get intervene)
           value))
       ;; Potentially modify value (and score) based on target trace
       (define [value2 score2]
         (if (and target (trace-has? target))
           (if intervention?
             [(trace-get target)
              (if (same-trace-states? (trace-get target) intervention-value)
                score
                (do (print ["mismatch!" (trace-get target) intervention-value])
                    negative-infinity))]
             [(trace-get target) score])
           [intervention-value score]))
       ;; Store value in output trace
       (if output
         (trace-set output value2))
       [value2 score2]))))

;; Query a 'native' generator (i.e. one written in metaprob and interpreted).

(define infer-apply-native
  (gen [self
        inputs
        intervene
        target
        output]
    (define source (lookup self "generative-source"))
    (define environment (trace-get self "environment"))
    (define new_env (make-env environment))
    (match-bind (lookup source "pattern")
                inputs
                new_env)
    (infer-eval (lookup source "body")
                new_env
                ;; Do not let the interpreter affect any of the traces.
                ;; Any changes to the traces needs to be made by the code
                ;; itself.
                intervene
                target
                output)))

(define infer-eval
  (gen [exp env intervene target output]
    (define walk
      (gen [exp env address]
        (define [v score]
          (case (trace-get exp)
            "application"
            (block (define n (length (trace-keys exp)))
                   (define subscore (empty-trace))
                   (trace-set subscore 0)
                   (define values
                     (map (gen [i]
                            (define [v s]
                              (walk (trace-subtrace exp i) env (add address (list i))))
                            (trace-set subscore (add (trace-get subscore) s))
                            v)
                          (range n)))
                   (define new-addr (add address (addr (procedure-key (trace-subtrace exp 0)))))
                   (define [val score]
                     (infer-apply (first values)
                                  (rest values)
                                  (if intervene (lookup intervene new-addr) nil)
                                  (if target (lookup target new-addr) nil)
                                  (if output (lookup output new-addr) nil)))
                   [val (add (trace-get subscore) score)])

            "variable"
            [(env-lookup env (trace-get exp "name")) 0]

            "literal"
            [(trace-get exp "value") 0]

            "gen"
            [(block (define proc (empty-trace))
                    (trace-set proc "prob prog")
                    (trace-set proc "name" (trace-name exp))
                    (trace-set-subtrace proc
                                        "generative-source"
                                        exp)
                    (trace-set proc "environment" env)
                    (trace-as-procedure proc
                                        ;; This may be unnecessary, but leaving it
                                        ;; in place for the time being
                                        (gen [& inputs]
                                          (nth (infer-apply proc inputs
                                                            nil nil nil)
                                               0))))
             0]

            "if"
            (block
             (define [pred p_score]
               (walk
                (lookup exp "predicate") env
                (add address (list "predicate"))))
             (if pred
               (block
                (define [val score]
                  (walk (lookup exp "then") env
                        (add address (list "then"))))
                [val (add p_score score)])
               (block
                (define [val score]
                  (walk (lookup exp "else") env
                        (add address (list "else"))))
                [val (add p_score score)])))

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
                                (add address (list i))))
                        (trace-set
                         subscore
                         (add (trace-get subscore) s))
                        v)
                      (range n)))
                   (if (gt (length values) 0)
                     [(last values) (trace-get subscore)]
                     [(empty-trace) (trace-get subscore)]))
            "definition"
            (block (define subaddr
                     (name_for_definiens
                      (lookup exp "pattern")))
                   (define [val score]
                     (walk (lookup exp subaddr) env
                           (add address subaddr)))
                   [(match-bind
                     (lookup exp "pattern")
                     val
                     env)
                    score])

            "this"
            [(capture-tag-address intervene target output)
             0]

            "with_address"
            (block (define [tag_addr tag_score]
                     (walk (lookup exp "tag") env
                           (add address (addr "tag"))))
                   ;; tag_addr is actually a quasi-address 
                   ;; (captured . address) - not an address.
                   ;; Must be constructed using (pair x (addr ...))
                   (define [new_intervene new_target new_output]
                     (resolve-tag-address tag_addr))
                   (define [val score]
                     (infer-eval (lookup exp "expression")
                                 env
                                 new_intervene
                                 new_target
                                 new_output))
                   [val (add tag_score score)])

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
   (gen [proc inputs] (clojure.core/apply proc (make-immutable inputs)))))


;; This file was automatically generated, then edited

;; Although this will almost alway run as clojure code, it
;; is a goal to retain the ability to run it as metaprob
;; (i.e. to make it self-applicable!).

(ns metaprob.infer
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

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
                          (freeze pattern) (length (trace-keys pattern))
                          (freeze inputs) (length inputs) env]))
               ;; TBD: handle [x & y] properly
               (for-each2 (gen [p i]
                            (assert (not (eq p "&")) "NYI")
                            (match-bind p i env))
                          subpatterns
                          inputs))
        (assert false "bad pattern")))
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
;; Use with (pair (capture-tag-address ...) (addr ...))

(define capture-tag-address
  (gen [i t o]
    ;; Cannot freeze, freezing is hereditary
    ;; Ergo, these things can't go into addresses (addr)
    (trace "intervention-trace" i
           "target-trace" t
           "output-trace" o
           :value "captured tag address")))

;; resolve-tag-address
;; Convert a quasi-address, whose first element was returned by 
;; capture-tag-address, into the appropriate trace

(define resolve-tag-address
  (gen [quasi_addr]
    (define captured (first quasi_addr))
    (define more (rest quasi_addr))
    (define i (trace-get captured "intervention-trace"))
    (define t (trace-get captured "target-trace"))
    (define o (trace-get captured "output-trace"))
    (tuple (if i (lookup i more) nil)
           (if t (lookup t more) nil)
           (if o (lookup o more) nil))))

;; ----------------------------------------------------------------------------

(declare infer infer-apply-native infer-eval)

;; infer
(define infer
  (gen [prog inputs intervention_trace target_trace output_trace]
    (assert (or (list? inputs) (tuple? inputs)) ["inputs neither list nor tuple" inputs])
    (if (if (trace? prog) (trace-has? prog "infer-method") false)
      ;; return the value+score that the infer-method computes
      ((trace-get prog "infer-method")
       inputs intervention_trace target_trace output_trace)
      (block
       (define [value score]
         (if (if (trace? prog) (trace-has? prog "generative-source") false)
           (infer-apply-native prog
                               inputs
                               intervention_trace
                               target_trace
                               output_trace)
           (if (foreign-procedure? prog)
             [(generate-foreign prog inputs) 0]
             (block (pprint prog)
                    (error "not a procedure" prog)))))
       ;; Potentially modify value and score based on intervention and target traces
       (define [value2 score2]
         (if (if target_trace (trace-has? target_trace) false)
           [(trace-get target_trace) 0]    ;???
           (if (if intervention_trace (trace-has? intervention_trace) false)
             [(trace-get intervention_trace) 0]  ;???
             [value score])))
       ;; Store value in output trace
       (if output_trace
         (trace-set output_trace value2))
       [value2 score2]))))

;; Query a 'native' generator (i.e. one written in metaprob and interpreted).

(define infer-apply-native
  (gen [self
        inputs
        intervention_trace
        target_trace
        output_trace]
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
              intervention_trace
              target_trace
              output_trace)))

(define infer-eval
  (gen [exp env intervention_trace target_trace output_trace]
    (define walk
      (gen [exp address]
        (define [v score]
          (if (eq (trace-get exp) "application")
            (block
             (define n (length (trace-keys exp)))
             (define subscore
               (block
                (define __trace_0__ (empty-trace))
                (trace-set __trace_0__ 0)
                __trace_0__))
             (define values
               (map (gen [i]
                      (define [v s]
                        (walk (trace-subtrace exp i) (add address (list i))))
                      (trace-set subscore (add (trace-get subscore) s))
                      v)
                    (range n)))
             (define oper (first values))
             (define name (procedure-name oper))
             (define [val score]
               (infer oper
                      (rest values)
                      (if intervention_trace
                        (lookup intervention_trace (add address (addr name)))
                        nil)
                      (if target_trace
                        (lookup target_trace (add address (addr name)))
                        nil)
                      (if output_trace
                        (lookup output_trace (add address (addr name)))
                        nil)))
             (tuple val (add (trace-get subscore) score)))
            (if (eq (trace-get exp) "variable")
              (tuple (env-lookup env
                                 (trace-get exp "name"))
                     0)
              (if (eq (trace-get exp) "literal")
                (tuple (trace-get exp "value") 0)
                (if (eq (trace-get exp) "gen")
                  (block
                   (tuple
                    (block
                     (define __trace_1__ (empty-trace))
                     (trace-set __trace_1__ "prob prog")
                     (trace-set __trace_1__ "name" (trace-name exp))
                     (trace-set-subtrace-at
                      __trace_1__
                      (list "generative-source")
                      exp)
                     (trace-set __trace_1__ "environment" env)
                     (trace-as-procedure __trace_1__
                                         ;; This may be unnecessary, but leaving it
                                         ;; in place for the time being
                                         (gen [& args]
                                           (nth (infer __trace_1__
                                                       args
                                                       nil nil nil)
                                                0))))
                    0))
                  (if (eq (trace-get exp) "if")
                    (block
                     (define [pred p_score]
                       (walk
                        (lookup exp "predicate")
                        (add address (list "predicate"))))
                     (if pred
                       (block
                        (define [val score]
                          (walk
                           (lookup exp "then")
                           (add address (list "then"))))
                        (tuple val (add p_score score)))
                       (block
                        (define [val score]
                          (walk
                           (lookup exp "else")
                           (add address (list "else"))))
                        (tuple val (add p_score score)))))
                    (if (eq (trace-get exp) "block")
                      (block
                       (define n (length (trace-keys exp)))
                       ;; (define new-env (make-env env))
                       (define
                         subscore
                         (block
                          (define __trace_2__ (empty-trace))
                          (trace-set __trace_2__ 0)
                          __trace_2__))
                       (define
                         values
                         (map          ;; How do we know map is left to right?
                          (gen [i]
                            (define [v s]
                              (walk (lookup exp i)
                                    (add address (list i))))
                            (trace-set
                             subscore
                             (add (trace-get subscore) s))
                            v)
                          (range n)))
                       (if (gt (length values) 0)
                         (block
                          (tuple (last values) (trace-get subscore)))
                         (block
                          (tuple (empty-trace) (trace-get subscore)))))
                      (if (eq (trace-get exp) "tuple")
                        (block
                         (define n (length (trace-keys exp)))
                         (define
                           subscore
                           (block
                            (define __trace_3__ (empty-trace))
                            (trace-set __trace_3__ 0)
                            __trace_3__))
                         (define
                           values
                           (map
                            (gen [i]
                              (define [v s]
                                (walk (lookup exp i)
                                      (add address (list i))))
                              (trace-set
                               subscore
                               (add (trace-get subscore) s))
                              v)
                            (range n)))
                         (tuple
                          (to-tuple values)
                          (trace-get subscore)))
                        (if (eq (trace-get exp) "definition")
                          (block
                           (define subaddr
                             (name_for_definiens
                              (lookup exp "pattern")))
                           (define [val score]
                             (walk (lookup exp subaddr)
                                   (add address subaddr)))
                           (tuple
                            (match-bind
                             (lookup exp "pattern")
                             val
                             env)
                            score))
                          (if (eq (trace-get exp) "this")
                            (tuple (capture-tag-address
                                    intervention_trace
                                    target_trace
                                    output_trace)
                                   0)
                            (if (eq (trace-get exp) "with_address")
                              (block
                               (define [tag_addr tag_score]
                                 (walk (lookup exp "tag")
                                       (add address (addr "tag"))))
                               (define [new_intervene new_target new_output]
                                 (resolve-tag-address tag_addr))
                               (define [val score]
                                 (infer-eval (lookup exp "expression")
                                             env
                                             new_intervene
                                             new_target
                                             new_output))
                               (tuple val (add tag_score score)))
                              (block
                               (pprint exp)
                               (error
                                "Not a code expression")))))))))))))
        (if (if intervention_trace
              (trace-has? intervention_trace address)
              false)
          (block
           (tuple (trace-get intervention_trace address) score))
          (block (tuple v score)))))
    (walk exp (addr))))

(define inf
  (gen [name infer-method]
    (define tr (empty-trace))
    (trace-set tr "name" (add "inf-" (procedure-name infer-method)))
    (trace-set tr "infer-method" infer-method)
    (trace-as-procedure tr
                        (gen [& inputs]
                          (nth (infer-method inputs nil nil nil)
                               0)))))

(define apply
  (inf "apply"
       (gen [inputs i t o]
         (infer (first inputs) (rest inputs) i t o))))

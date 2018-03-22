;; This file was automatically generated, then edited

(ns metaprob.metacirc.query
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]))

(ns metaprob.metacirc.query
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare query query-lifted query-native ptc_eval)

;; query
(define query
  (program
    [prog
     inputs
     intervention_trace
     target_trace
     output_trace]

    ;; Three cases: lifted, foreign, and native
    (if (trace-has-key? prog "query-method")    ;lifted
      (query-lifted (trace-get prog "query-method")
                    inputs
                    intervention_trace
                    target_trace
                    output_trace)
      (if (trace-has-key? prog "native-generate-method")      
        (query-native (lookup prog "native-generate-method") ;was "source"
                      (trace-get prog "environment")
                      inputs
                      intervention_trace
                      target_trace
                      output_trace)
        (if (trace-has-key? prog "foreign-generate-method")
          (query-foreign (trace-get prog "foreign-generate-method") ;"execute"
                         inputs
                         intervention_trace
                         target_trace
                         output_trace)      
          (block (pprint prog)
                 (assert false "Not a prob prog" prog)))))))

;; Query a 'lifted' probprog.
;; Pass the traces into the probprog, as well as the arguments.
;; The probprog returns the score as part of the return value.

(define query-lifted
  (probprog [query-method
             inputs
             intervention_trace
             target_trace
             output_trace]

            (define [result+score ignored-score]
              (query query-method
                     [inputs
                      intervention_trace
                      target_trace
                      output_trace]
                     nil nil nil))

            result+score))

;; Query a 'native' probprog (i.e. one written in metaprob and interpreted).
;; source and environment are traces.

(define query-native
  (probprog [source
             environment
             inputs
             intervention_trace
             target_trace
             output_trace]

            (define new_env (make-env environment))

            (match-bind (lookup source "pattern")
                        inputs
                        new_env)

            (define [answer score]
              (ptc_eval (lookup source "body")
                        new_env
                        ;; Do not let the interpreter affect any of the traces.
                        ;; Any changes to the traces needs to be made by the code
                        ;; itself.
                       intervention_trace
                       target_trace
                       output_trace))

            (define [answer score]
              (if (if target_trace (trace-has? target_trace) false)
                [(trace-get target_trace) 0]
                (if (if intervention_trace (trace-has? intervention_trace) false)
                  [(trace-get intervention_trace) 0]
                  [answer score])))

            (if output_trace
              (trace-set output_trace answer))

            [answer score]))

;; generate
(define generate
  (probprog [pp & args]
    (define [answer _] (query pp args nil nil nil))
    answer))

;;; This doesn't really belong here.
;
;(define make-lifted-probprog
;  (probprog [query-method]
;            (define l (empty-trace))
;            (trace-set l "query-method" query-method)
;            l))
;
;;; Provide a probprog with a score method.
;
;(define provide-score-method
;  (probprog [start score-method]
;            (make-lifted-probprog
;             (probprog [argseq i t o]
;                       (define [answer score]
;                         (query start argseq i t o))
;                       [answer (score-method argseq answer)]))))

(define
  ptc_eval
  (program
    [exp env intervention_trace target_trace output_trace]
    (define
      walk
      (program
        [exp addr]
        (define
          [v score]
          (if (eq (trace-get exp) "application")
            (block
              (define n (length (trace-subkeys exp)))
              (define
                subscore
                (block
                  (define __trace_0__ (empty-trace))
                  (trace-set __trace_0__ 0)
                  __trace_0__))
              (define
                values
                (map
                  (program
                    [i]
                    (define
                      [v s]
                      (walk (lookup exp (list i)) (add addr (list i))))
                    (trace-set subscore (add (trace-get subscore) s))
                    v)
                  (range n)))
              (define oper (first values))
              (define name (trace-get (lookup oper (list "name"))))
              (define
                [val score]
                (query
                  oper
                  (rest values)
                  (lookup intervention_trace (add addr (list name)))
                  (lookup target_trace (add addr (list name)))
                  (if output_trace
                    (lookup output_trace (add addr (list name)))
                    output_trace)))
              (tuple val (add (trace-get subscore) score)))
            (if (eq (trace-get exp) "variable")
              (block
                (tuple
                  (env-lookup
                    env
                    (trace-get (lookup exp (list "name"))))
                  0))
              (if (eq (trace-get exp) "literal")
                (block
                  (tuple (trace-get (lookup exp (list "value"))) 0))
                (if (eq (trace-get exp) "program")
                  (block
                    (tuple
                      (block
                        (define __trace_1__ (empty-trace))
                        (trace-set __trace_1__ "prob prog")
                        (trace-set
                          (lookup __trace_1__ (list "name"))
                          exp)
                        (trace-set-subtrace-at
                          __trace_1__
                          (list "native-generate-method")
                          exp)
                        (trace-set
                          (lookup __trace_1__ (list "environment"))
                          env)
                        (trace-to-probprog
                         __trace_1__))
                      0))
                  (if (eq (trace-get exp) "if")
                    (block
                      (define
                        [pred p_score]
                        (walk
                          (lookup exp (list "predicate"))
                          (add addr (list "predicate"))))
                      (if pred
                        (block
                          (define
                            [val score]
                            (walk
                              (lookup exp (list "then"))
                              (add addr (list "then"))))
                          (tuple val (add p_score score)))
                        (block
                          (define
                            [val score]
                            (walk
                              (lookup exp (list "else"))
                              (add addr (list "else"))))
                          (tuple val (add p_score score)))))
                    (if (eq (trace-get exp) "block")
                      (block
                        (define n (length (trace-subkeys exp)))
                        (define
                          subscore
                          (block
                            (define __trace_2__ (empty-trace))
                            (trace-set __trace_2__ 0)
                            __trace_2__))
                        (define
                          values
                          (map
                            (program
                              [i]
                              (define
                                [v s]
                                (walk
                                  (lookup exp (list i))
                                  (add addr (list i))))
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
                          (define n (length (trace-subkeys exp)))
                          (define
                            subscore
                            (block
                              (define __trace_3__ (empty-trace))
                              (trace-set __trace_3__ 0)
                              __trace_3__))
                          (define
                            values
                            (map
                              (program
                                [i]
                                (define
                                  [v s]
                                  (walk
                                    (lookup exp (list i))
                                    (add addr (list i))))
                                (trace-set
                                  subscore
                                  (add (trace-get subscore) s))
                                v)
                              (range n)))
                          (tuple
                            (list-to-array values)
                            (trace-get subscore)))
                        (if (eq (trace-get exp) "definition")
                          (block
                            (define
                              subaddr
                              (name_for_definiens
                                (lookup exp (list "pattern"))))
                            (define
                              [val score]
                              (walk
                                (lookup exp subaddr)
                                (add addr subaddr)))
                            (tuple
                              (match-bind
                                (lookup exp (list "pattern"))
                                val
                                env)
                              score))
                          (if (eq (trace-get exp) "this")
                            (block
                              (tuple
                                (capture-tag-address
                                  intervention_trace
                                  target_trace
                                  output_trace)
                                0))
                            (if (eq (trace-get exp) "with_address")
                              (block
                                (define
                                  [tag_addr tag_score]
                                  (walk
                                    (lookup exp (list "tag"))
                                    (add addr (list "tag"))))
                                (define
                                  [new_intervene new_target new_output]
                                  (resolve-tag-address tag_addr))
                                (define
                                  [val score]
                                  (ptc_eval
                                    (lookup exp (list "expression"))
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
              (trace-has? (lookup intervention_trace addr))
              false)
          (block
            (tuple (trace-get (lookup intervention_trace addr)) score))
          (block (tuple v score)))))
    (walk exp (list))))

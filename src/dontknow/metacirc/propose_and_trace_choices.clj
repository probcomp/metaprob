;; This file was automatically generated, then edited

(clojure.core/ns dontknow.metacirc.propose-and-trace-choices
  (:refer-clojure :only [declare ])
  (:require [dontknow.syntax :refer :all]
            [dontknow.builtin :refer :all]
            [dontknow.prelude :refer :all]
            ;; Added manually
            [dontknow.metacirc.interpret :refer [interpret]]))

(declare propose_and_trace_choices ptc_eval)

(define
  propose_and_trace_choices
  (program
    [program-noncolliding
     inputs
     intervention_trace
     target_trace
     output_trace]
    (if (trace_has_key
          program-noncolliding
          "custom_choice_tracing_proposer")
      (block
        (define
          ptc_inputs
          (tuple inputs intervention_trace target_trace output_trace))
        (interpret
          (trace_get
            (lookup
              program-noncolliding
              (list "custom_choice_tracing_proposer")))
          ptc_inputs
          (mk_nil)))
      (if (trace_has_key program-noncolliding "source")
        (block
          (define
            new_env
            (make_env
              (trace_get
                (lookup program-noncolliding (list "environment")))))
          (match_bind
            (lookup program-noncolliding (list "source" "pattern"))
            inputs
            new_env)
          (ptc_eval
            (lookup program-noncolliding (list "source" "body"))
            new_env
            intervention_trace
            target_trace
            output_trace))
        (if (trace_has_key program-noncolliding "executable")
          (block
            (define
              val
              (interpret_prim
                (trace_get
                  (lookup program-noncolliding (list "executable")))
                inputs
                intervention_trace))
            (trace_set output_trace val)
            (tuple val 0))
          (block
            (pprint program-noncolliding)
            (error "Not a prob prog")))))))

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
          (if (eq (trace_get exp) "application")
            (block
              (define n (length (trace_subkeys exp)))
              (define
                subscore
                (block
                  (define __trace_0__ (mk_nil))
                  (trace_set __trace_0__ 0)
                  __trace_0__))
              (define
                values
                (map
                  (program
                    [i]
                    (define
                      [v s]
                      (walk (lookup exp (list i)) (add addr (list i))))
                    (trace_set subscore (add (trace_get subscore) s))
                    v)
                  (range n)))
              (define oper (first values))
              (define name (trace_get (lookup oper (list "name"))))
              (define
                [val score]
                (propose_and_trace_choices
                  oper
                  (rest values)
                  (lookup intervention_trace (add addr (list name)))
                  (lookup target_trace (add addr (list name)))
                  (lookup output_trace (add addr (list name)))))
              (tuple val (add (trace_get subscore) score)))
            (if (eq (trace_get exp) "variable")
              (block
                (tuple
                  (env_lookup
                    env
                    (trace_get (lookup exp (list "name"))))
                  0))
              (if (eq (trace_get exp) "literal")
                (block
                  (tuple (trace_get (lookup exp (list "value"))) 0))
                (if (eq (trace_get exp) "program")
                  (block
                    (tuple
                      (block
                        (define __trace_1__ (mk_nil))
                        (trace_set __trace_1__ "prob prog")
                        (trace_set
                          (lookup __trace_1__ (list "name"))
                          exp)
                        (trace_set_subtrace_at
                          __trace_1__
                          (list "source")
                          exp)
                        (trace_set
                          (lookup __trace_1__ (list "environment"))
                          env)
                        __trace_1__)
                      0))
                  (if (eq (trace_get exp) "if")
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
                    (if (eq (trace_get exp) "block")
                      (block
                        (define n (length (trace_subkeys exp)))
                        (define
                          subscore
                          (block
                            (define __trace_2__ (mk_nil))
                            (trace_set __trace_2__ 0)
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
                              (trace_set
                                subscore
                                (add (trace_get subscore) s))
                              v)
                            (range n)))
                        (if (gt (length values) 0)
                          (block
                            (tuple (last values) (trace_get subscore)))
                          (block
                            (tuple (mk_nil) (trace_get subscore)))))
                      (if (eq (trace_get exp) "tuple")
                        (block
                          (define n (length (trace_subkeys exp)))
                          (define
                            subscore
                            (block
                              (define __trace_3__ (mk_nil))
                              (trace_set __trace_3__ 0)
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
                                (trace_set
                                  subscore
                                  (add (trace_get subscore) s))
                                v)
                              (range n)))
                          (tuple
                            (list_to_array values)
                            (trace_get subscore)))
                        (if (eq (trace_get exp) "definition")
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
                              (match_bind
                                (lookup exp (list "pattern"))
                                val
                                env)
                              score))
                          (if (eq (trace_get exp) "this")
                            (block
                              (tuple
                                (capture_tag_address
                                  intervention_trace
                                  target_trace
                                  output_trace)
                                0))
                            (if (eq (trace_get exp) "with_address")
                              (block
                                (define
                                  [tag_addr tag_score]
                                  (walk
                                    (lookup exp (list "tag"))
                                    (add addr (list "tag"))))
                                (define
                                  [new_intervene new_target new_output]
                                  (resolve_tag_address tag_addr))
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
        (if (trace_has (lookup intervention_trace addr))
          (block
            (tuple (trace_get (lookup intervention_trace addr)) score))
          (block (tuple v score)))))
    (walk exp (list))))


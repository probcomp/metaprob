;; This file was automatically generated, then edited

(ns metaprob.metacirc.interpret
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

;; Manual edit: moved name_for_definiens to prelude.clj

(declare interpret interpret_eval construct_probprog)

(define
  interpret
  (program
   [program-noncolliding inputs intervention_trace]
   (if (trace_has_key program-noncolliding "query_method")
     (block
      (define i_inputs (tuple inputs intervention_trace))
      (interpret
       (trace_get
        (lookup
         program-noncolliding
         (list "query_method")))
       i_inputs
       (empty-trace)))
     (if (trace_has_key program-noncolliding "native-generate-method")
       (block
        (define
          new_env
          (make_env
           (trace_get
            (lookup program-noncolliding (list "environment")))))
        (match_bind
         (lookup program-noncolliding (list "native-generate-method" "pattern"))
         inputs
         new_env)
        (interpret_eval
         (lookup program-noncolliding (list "native-generate-method" "body"))
         new_env
         intervention_trace))
       ;; Had to move this case
       (if (trace_has_key program-noncolliding "foreign-generate-method")
         (block
          (interpret_prim
           (trace_get (lookup program-noncolliding (list "foreign-generate-method")))
           inputs
           intervention_trace))
         (block
          (pprint program-noncolliding)
          (error "Not a prob prog")))))))

(define
  interpret_eval
  (program
    [exp env intervention_trace]
    (define
      walk
      (program
        [exp addr]
        (define
          v
          (if (eq (trace_get exp) "application")
            (block
              (define n (length (trace_subkeys exp)))
              (define
                values
                (map
                  (program
                    [i]
                    (walk (lookup exp (list i)) (add addr (list i))))
                  (range n)))
              (define oper (first values))
              (define name (trace_get (lookup oper (list "name"))))
              (interpret
                oper
                (rest values)
                (lookup intervention_trace (add addr (list name)))))
            (if (eq (trace_get exp) "variable")
              (block
                (env_lookup
                  env
                  (trace_get (lookup exp (list "name")))))
              (if (eq (trace_get exp) "literal")
                (block (trace_get (lookup exp (list "value"))))
                (if (eq (trace_get exp) "program")
                  (block
                    (block
                      (define __trace_0__ (empty-trace))
                      (trace_set __trace_0__ "prob prog")
                      (trace_set
                        (lookup __trace_0__ (list "name"))
                        exp)
                      (trace_set_subtrace_at
                        __trace_0__
                        (list "native-generate")
                        exp)
                      (trace_set
                        (lookup __trace_0__ (list "environment"))
                        env)
                      __trace_0__))
                  (if (eq (trace_get exp) "if")
                    (block
                      (define
                        pred
                        (walk
                          (lookup exp (list "predicate"))
                          (add addr (list "predicate"))))
                      (if pred
                        (block
                          (walk
                            (lookup exp (list "then"))
                            (add addr (list "then"))))
                        (block
                          (walk
                            (lookup exp (list "else"))
                            (add addr (list "else"))))))
                    (if (eq (trace_get exp) "block")
                      (block
                        (define n (length (trace_subkeys exp)))
                        (define
                          values
                          (map
                            (program
                              [i]
                              (walk
                                (lookup exp (list i))
                                (add addr (list i))))
                            (range n)))
                        (if (gt (length values) 0)
                          (last values)
                          (empty-trace)))
                      (if (eq (trace_get exp) "tuple")
                        (block
                          (define n (length (trace_subkeys exp)))
                          (define
                            values
                            (map
                              (program
                                [i]
                                (walk
                                  (lookup exp (list i))
                                  (add addr (list i))))
                              (range n)))
                          (list_to_array values))
                        (if (eq (trace_get exp) "definition")
                          (block
                            (define
                              subaddr
                              (name_for_definiens
                                (lookup exp (list "pattern"))))
                            (define
                              val
                              (walk
                                (lookup exp subaddr)
                                (add addr subaddr)))
                            (match_bind
                              (lookup exp (list "pattern"))
                              val
                              env))
                          (if (eq (trace_get exp) "this")
                            (block
                              (capture_tag_address
                                intervention_trace
                                (empty-trace)
                                (empty-trace)))
                            (if (eq (trace_get exp) "with_address")
                              (block
                                (define
                                  tag_addr
                                  (walk
                                    (lookup exp (list "tag"))
                                    (add addr (list "tag"))))
                                (define
                                  [new_intervene _ _]
                                  (resolve_tag_address tag_addr))
                                (interpret_eval
                                  (lookup exp (list "expression"))
                                  env
                                  new_intervene))
                              (block
                                (pprint exp)
                                (error
                                  "Not a code expression")))))))))))))
        (if (trace_has (lookup intervention_trace addr))
          (block (trace_get (lookup intervention_trace addr)))
          (block v))))
    (walk exp (list))))

(define
  construct_probprog
  (program
    [name source env]
    (block
      (block
        (define __trace_1__ (empty-trace))
        (trace_set __trace_1__ "prob prog")
        (trace_set (lookup __trace_1__ (list "name")) name)
        (trace_set_subtrace_at __trace_1__ (list "native-generate") source)
        (trace_set (lookup __trace_1__ (list "environment")) env)
        __trace_1__))))

;; Manual edit: moved name_for_definiens to prelude.clj

;; This file was automatically generated

(ns metaprob.examples.ad.backpropagate
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  backpropagate
  get_default
  compound_backpropagator
  backpropagator_of)

(define
  backpropagate
  (program
    [body var_map trace direction]
    (block
      (if (is_app body)
        (block
          (define
            opers
            (filter
              (program [n] (not (is_integer n)))
              (trace_subkeys trace)))
          (assert
            (eq (length opers) 1)
            "Wrong number of recorded operators")
          (define sp_name (first opers))
          (define
            backpropagator
            (backpropagator_of
              (trace_get (lookup trace (list sp_name "operator")))))
          (define
            arg_dirs
            (pair
              0
              (backpropagator
                (lookup trace (list sp_name))
                direction)))
          (define subexpr_keys (trace_subkeys (app_subs body)))
          (for_each2
            (program
              [subexpr_key subdir]
              (backpropagate
                (lookup (app_subs body) (list subexpr_key))
                var_map
                (lookup trace (list subexpr_key))
                subdir))
            subexpr_keys
            arg_dirs))
        (block
          (if (is_var body)
            (block
              (define box (lookup var_map (list (var_name body))))
              (if (trace_has box) "ok" (block (trace_set box 0)))
              (trace_set box (add (trace_get box) direction)))
            (block
              (if (is_lit body)
                (block "ok")
                (block
                  (if (is_lam body)
                    (block (error "Encountered a lambda expression"))
                    (block
                      (if (is_alt body)
                        (block
                          (if (contains (trace_subkeys trace) "then")
                            (block
                              (assert
                                (not
                                  (contains
                                    (trace_subkeys trace)
                                    "else"))
                                "Expected exactly one if branch to be taken")
                              (backpropagate
                                (alt_cons body)
                                var_map
                                (lookup trace (list "then"))
                                direction))
                            (block
                              (assert
                                (contains (trace_subkeys trace) "else")
                                "Expected an if branch to be taken")
                              (backpropagate
                                (alt_alt body)
                                var_map
                                (lookup trace (list "else"))
                                direction))))
                        (block
                          (if (is_seq body)
                            (block
                              (define
                                n
                                (length
                                  (trace_subkeys (seq_subs body))))
                              (for_each
                                (reverse (range n))
                                (program
                                  [i]
                                  (define
                                    subexp
                                    (lookup body (list i)))
                                  (backpropagate
                                    subexp
                                    var_map
                                    (lookup trace (list i))
                                    (if
                                      (eq i (sub n 1))
                                      direction
                                      0)))))
                            (block
                              (if (is_def body)
                                (block
                                  (if
                                    (is_var (def_pat body))
                                    (block
                                      (define
                                        sub_dir
                                        (get_default
                                          var_map
                                          (var_name (def_pat body))
                                          0))
                                      (backpropagate
                                        (def_expr body)
                                        var_map
                                        (lookup
                                          trace
                                          (list
                                            (var_name (def_pat body))))
                                        sub_dir))
                                    (block
                                      (error
                                        "Encountered a nontrivial pattern match"))))
                                (block
                                  (error
                                    "Unrecognized expression type"))))))))))))))))))

(define
  get_default
  (program
    [trace key default]
    (if (trace_has (lookup trace (list key)))
      (trace_get (lookup trace (list key)))
      default)))

(define
  compound_backpropagator
  (program
    [sp_name]
    (define [pat body] (destructure_compound_name sp_name))
    (define var_map (mk_nil))
    (define n (length pat))
    (program
      [app_trace direction]
      (backpropagate body var_map app_trace direction)
      (map
        (program
          [i]
          (block
            (get_default
              var_map
              (trace_get (lookup pat (list i "name")))
              0)))
        (range n)))))

(define
  backpropagator_of
  (program
    [thing]
    (if (is_trace thing)
      (if (trace_has_key thing "backpropagator")
        (trace_get (lookup thing (list "backpropagator")))
        (block
          (if (trace_has_key thing "source")
            (compound_backpropagator (lookup thing (list "source")))
            (block (error "Can't derive a backpropagator")))))
      (block (primitive_backpropagator thing)))))


;; This file was automatically generated

(ns metaprob.metacirc.envs
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare make_env match_bind env_lookup)

(define
  make_env
  (program
    [parent]
    (define __trace_0__ (mk_nil))
    (trace_set __trace_0__ "environment")
    (trace_set (lookup __trace_0__ (list "parent")) parent)
    __trace_0__))

(define
  match_bind
  (program
    [pat val env]
    (block
      (if (eq (trace_get pat) "variable")
        (block
          (define name (trace_get (lookup pat (list "name"))))
          (if (neq name "_")
            (block
              (trace_set (lookup env (list "variables" name)) val))
            "ok"))
        (if (eq (trace_get pat) "tuple")
          (block
            (define vals (collection_to_array val))
            (define n (length pat))
            (for_each
              (range n)
              (program
                [k]
                (block
                  (match_bind
                    (lookup pat (list k))
                    (trace_get (lookup vals (list k)))
                    env)))))
          (block (pprint pat) (error "Invalid binding pattern.")))))))

(define
  env_lookup
  (program
    [env name]
    (block
      (if (is_builtin_env env)
        (block (lookup env name))
        (if (trace_has_key (lookup env (list "variables")) name)
          (block (trace_get (lookup env (list "variables" name))))
          (if (trace_has_key env "parent")
            (block
              (env_lookup
                (trace_get (lookup env (list "parent")))
                name))
            (block (error (add "Unbound variable " name)))))))))


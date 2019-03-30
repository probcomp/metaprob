(ns metaprob.autotrace
  (:require [metaprob.code-handlers :as code]
            [metaprob.expander :as expander]
            [metaprob.generative-functions :refer [gen]]))

(declare autotrace-expression)

(defn autotrace-expressions
  [expressions stack]
  (map-indexed #(autotrace-expression %2 (cons %1 stack)) expressions))

(defmacro autotrace #?(:clj [gen-expr] :cljs [env gen-expr])
  (let [expr (expander/mp-expand #?(:cljs env) gen-expr)
        result
        `(gen ~@(if (code/gen-has-annotations? expr)
                  [(code/gen-annotations expr)]
                  [])
           ~(code/gen-pattern expr)
           ~@(autotrace-expressions (code/gen-body expr) '()))]
    result))

(defn autotrace-expression
  [expr stack]
  (cond
    (or (code/fn-expr? expr) (code/gen-expr? expr))
    `(~(first expr) ~@(if (code/gen-has-annotations? expr) [(code/gen-annotations expr)] [])
      ~(code/gen-pattern expr)
      ~@(autotrace-expressions (code/gen-body expr) stack))

    (code/quote-expr? expr)
    expr

    (code/if-expr? expr)
    `(if ~(autotrace-expression (code/if-predicate expr) (cons "predicate" stack))
       ~(autotrace-expression (code/if-then-clause expr) (cons "then" stack))
       ~(autotrace-expression (code/if-else-clause expr) (cons "else" stack)))

    (seq? expr)
    (if (or (= (first expr) 'apply-at) (= (first expr) 'at) (code/fn-expr? (first expr)))
      (autotrace-expressions expr stack)
      `(~'at
        '~(reverse (cons (str (first expr)) stack))
        ~@(autotrace-expressions expr stack)))

    true expr))

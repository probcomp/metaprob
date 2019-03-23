(ns metaprob.autotrace
  (:require [metaprob.code-handlers :refer :all]
            [metaprob.expander :refer [mp-expand]]
            [metaprob.generative-functions :refer [gen]]))

(declare autotrace-expression)

(defn autotrace-expressions
  [expressions tracer-name stack]
  (map-indexed #(autotrace-expression %2 tracer-name (cons %1 stack)) expressions))

(defmacro autotrace [gen-expr]
  (let [tracer-name (gensym "trace")
        expr (mp-expand gen-expr)
        result
        `(gen ~(assoc (gen-annotations expr) :tracing-with tracer-name)
           ~(gen-pattern expr)
           ~@(autotrace-expressions (gen-body expr) tracer-name '()))]
    result))

(defn autotrace-expression
  [expr tracer-name stack]
  (cond
    (gen-expr? expr)
    (if (gen-tracer-name expr)
      expr
      `(autotrace ~expr))

    (quote-expr? expr)
    expr

    (if-expr? expr)
    `(if ~(autotrace-expression (if-predicate expr) tracer-name (cons "predicate" stack))
       ~(autotrace-expression (if-then-clause expr) tracer-name (cons "then" stack))
       ~(autotrace-expression (if-else-clause expr) tracer-name (cons "else" stack)))

    (seq? expr)
    `(~tracer-name '~(reverse (cons (str (first expr)) stack))
       ~@(let [[f & args] (autotrace-expressions expr tracer-name stack)]
           (list f (vec args))))

    true expr))

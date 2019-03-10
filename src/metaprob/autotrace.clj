(ns metaprob.autotrace
  (:refer-clojure :exclude [apply get contains? dissoc assoc empty? keys get-in map replicate reduce])
  (:require [clojure.pprint :as pprint]
            [metaprob.builtin :refer :all]
            [metaprob.compound :refer :all]
            [metaprob.code-handlers :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.expander :refer [register-transformation!]]
            [metaprob.syntax :refer :all]))

(declare autotrace-expression)

(defn autotrace-expressions
  [expressions tracer-name stack]
  (map-indexed #(autotrace-expression %2 tracer-name (cons %1 stack)) expressions))

(defn autotrace-expression
  [expr tracer-name stack]
  (cond
    (gen-expr? expr)
    (if (gen-has-annotations? expr)
      expr
      `(gen {:transform "autotrace"} ~(gen-pattern expr) ~@(gen-body expr)))

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

;; A transformation can assume its argument is of the form
;; (gen {...} [...] body), where body has been expanded.
(register-transformation!
  "autotrace"
  (fn [expr]
    (let [tracer-name (gensym "trace")
          result
          `(gen ~(assoc (gen-annotations expr) :tracing-with tracer-name)
             ~(gen-pattern expr)
             ~@(autotrace-expressions (gen-body expr) tracer-name '()))]
      result)))


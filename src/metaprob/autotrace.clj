(ns metaprob.autotrace
  (:require [metaprob.code-handlers :refer :all]
            [metaprob.expander :refer [mp-expand]]
            [metaprob.generative-functions :refer [gen]]))

(declare autotrace-expression)

(defn autotrace-expressions
  [expressions stack]
  (map-indexed #(autotrace-expression %2 (cons %1 stack)) expressions))

(defmacro autotrace [gen-expr]
  (let [expr (mp-expand gen-expr)
        result
        `(gen ~@(if (gen-has-annotations? expr) [(gen-annotations expr)] [])
           ~(gen-pattern expr)
           ~@(autotrace-expressions (gen-body expr) '()))]
    result))

(defn autotrace-expression
  [expr stack]
  (cond
    (or (fn-expr? expr) (gen-expr? expr))
    `(~(first expr) ~@(if (gen-has-annotations? expr) [(gen-annotations expr)] [])
       ~(gen-pattern expr)
       ~@(autotrace-expressions (gen-body expr) stack))

    (quote-expr? expr)
    expr

    (if-expr? expr)
    `(if ~(autotrace-expression (if-predicate expr) (cons "predicate" stack))
       ~(autotrace-expression (if-then-clause expr) (cons "then" stack))
       ~(autotrace-expression (if-else-clause expr) (cons "else" stack)))

    (seq? expr)
    (if (or (= (first expr) 'apply-at) (= (first expr) 'at) (fn-expr? (first expr)))
      (autotrace-expressions expr stack)
      `(~'at '~(reverse (cons (str (first expr)) stack)) ~@(autotrace-expressions expr stack)))

    true expr))

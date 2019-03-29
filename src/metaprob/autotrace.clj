(ns metaprob.autotrace
  (:require [metaprob.code-handlers :refer :all]
            [metaprob.expander :refer [mp-expand]]
            [metaprob.generative-functions :refer [gen]]))

(declare autotrace-expression)

(defn autotrace-expressions
  [expressions tracer-name stack]
  (map-indexed #(autotrace-expression %2 tracer-name (cons %1 stack)) expressions))

(defmacro autotrace [gen-expr]
  (let [tracer-name 'trace-at
        expr (mp-expand gen-expr)
        result
        `(gen ~@(if (gen-has-annotations? expr) [(gen-annotations expr)] [])
           ~(gen-pattern expr)
           ~@(autotrace-expressions (gen-body expr) tracer-name '()))]
    result))

(defn autotrace-expression
  [expr tracer-name stack]
  (cond
    (or (fn-expr? expr) (gen-expr? expr))
    `(~(first expr) ~@(if (gen-has-annotations? expr) [(gen-annotations expr)] [])
       ~(gen-pattern expr)
       ~@(autotrace-expressions (gen-body expr) tracer-name stack))

    (quote-expr? expr)
    expr

    (if-expr? expr)
    `(if ~(autotrace-expression (if-predicate expr) tracer-name (cons "predicate" stack))
       ~(autotrace-expression (if-then-clause expr) tracer-name (cons "then" stack))
       ~(autotrace-expression (if-else-clause expr) tracer-name (cons "else" stack)))

    (seq? expr)
    (let [already-traced? (= (first expr) tracer-name)
          addr (if already-traced? (second expr) (list 'quote (reverse (cons (str (first expr)) stack))))
          [f args]
          (if already-traced?
            (autotrace-expressions (rest (rest expr)) tracer-name stack)
            [(autotrace-expression (first expr) tracer-name (cons "head" stack))
             (vec (autotrace-expressions (rest expr) tracer-name (cons "tail" stack)))])]
      `(~tracer-name ~addr
         ~f ~args))

    true expr))

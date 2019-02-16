(ns metaprob.autotrace
  (:refer-clojure :exclude [apply get contains? dissoc assoc empty? keys get-in map replicate reduce])
  (:require [clojure.pprint :as pprint]
            [metaprob.builtin :refer :all]
            [metaprob.compound :refer :all]
            [metaprob.code-handlers :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.expander :refer [def-transformation]]
            [metaprob.syntax :refer :all]))

(declare autotrace-expression)

(def autotrace-expressions
  (gen [expressions tracer-name stack]
    (map-indexed
      (gen [i sub-expr]
        (autotrace-expression sub-expr tracer-name (cons i stack)))
      expressions)))

(def autotrace-expression
  (gen [expr tracer-name stack]
    (cond
      (do-expr? expr)
      (cons 'do (autotrace-expressions (do-body expr) tracer-name stack))

      (if-expr? expr)
      `(if ~(autotrace-expression (if-predicate expr) tracer-name (cons "predicate" stack))
         ~(autotrace-expression (if-then-clause expr) tracer-name (cons "then" stack))
         ~(autotrace-expression (if-else-clause expr) tracer-name (cons "else" stack)))

      (let-expr? expr)
      `(let
         ~(vec (interleave
                 (let-patterns expr)
                 (map-indexed
                   (gen [i [pattern expr]]
                     (autotrace-expression
                       expr tracer-name
                       (cons
                         (if (and (symbol? pattern) (not= '_ pattern))
                           (str pattern)
                           (str i "," pattern))
                         stack)))
                   (let-bindings expr))))
         ~@(autotrace-expressions (let-body expr) tracer-name (cons "let-body" stack)))

      (or (gen-expr? expr) (quote-expr? expr))
      expr

      (seq? expr)
      `(~tracer-name '~(reverse (cons (str (first expr)) stack))
         ~@(autotrace-expressions expr tracer-name stack))

      true expr)))

;; A transformation can assume its argument is of the form
;; (gen {...} [...] body), where body has been expanded.
(register-transformation!
  "autotrace"
  (gen [expr]
    (let [tracer-name (gensym "trace")
          result
          `(gen ~(assoc (gen-annotations expr) :tracing-with tracer-name)
             ~(gen-pattern expr)
             ~@(autotrace-expressions (gen-body expr) tracer-name '()))]
      result)))



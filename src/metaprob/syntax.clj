(ns metaprob.syntax
  (:refer-clojure :exclude [get contains? dissoc assoc empty? apply keys get-in])
  (:require [metaprob.code-handlers :refer :all]
            [metaprob.expander :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.compound :refer :all]))


(declare gen)

(defmacro generator [expand? gen-expr]
  {:style/indent 2}
  (let [expr
        (if expand? (map-gen mark-as-already-macroexpanded (mp-expand gen-expr)) gen-expr)

        body
        (gen-body expr)

        name
        (gen-name expr)

        tracer-name
        (gen-tracer-name expr)

        params
        (gen-pattern expr)

        thunk-name
        (gensym (str (if name name "") "thunk"))

        named-fn-body
        (if name
          `((let [~name (~thunk-name)]
             ~@body))
          body)

        innermost-fn-expr
        `(fn ~params ~@named-fn-body)

        clojure-fn-expr
        (if tracer-name
          `(let [~tracer-name (fn [~'_ ~'f & ~'args] (apply ~'f ~'args))]
             ~innermost-fn-expr)
          innermost-fn-expr)

        clojure-impl-expr
        (if tracer-name
          `(fn [~tracer-name] ~innermost-fn-expr)
          nil)]

        `(let [~thunk-name (fn ~thunk-name [] (with-meta ~clojure-fn-expr {:generative-source '~expr, :clojure-impl ~clojure-impl-expr, :name '~name}))]
             (~thunk-name))))

(defmacro gen
  "like fn, but for metaprob procedures"
  {:style/indent 1}
  [& form]
  `(generator
     ~(not (get (meta &form) :no-expand?))
     ~&form))

(ns metaprob.syntax
  (:refer-clojure :exclude [get contains? dissoc assoc empty? apply keys get-in])
  (:require [metaprob.code-handlers :as code]
            [metaprob.expander :as expander]
            [metaprob.builtin :as builtin]
            [metaprob.compound :as compound]))

(defmacro gen
  "like fn, but for metaprob procedures"
  {:style/indent 1}
  [& form]
  `(generator
     ~(not (compound/get (meta &form) :no-expand?))
     ~&form))

(defmacro generator [expand? gen-expr]
  {:style/indent 2}
  (let [expr
        (if expand? (code/map-gen expander/mark-as-already-macroexpanded (expander/mp-expand gen-expr)) gen-expr)

        body
        (code/gen-body expr)

        name
        (code/gen-name expr)

        tracer-name
        (code/gen-tracer-name expr)

        params
        (code/gen-pattern expr)

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
          `(let [~tracer-name (fn [~'_ ~'f & ~'args] (metaprob.builtin/apply ~'f ~'args))]
             ~innermost-fn-expr)
          innermost-fn-expr)

        clojure-impl-expr
        (if tracer-name
          `(fn [~tracer-name] ~innermost-fn-expr)
          nil)]

        `(let [~thunk-name (fn ~thunk-name [] (with-meta ~clojure-fn-expr {:generative-source '~expr, :clojure-impl ~clojure-impl-expr, :name '~name}))]
             (~thunk-name))))

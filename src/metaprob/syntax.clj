(ns metaprob.syntax
  (:refer-clojure :exclude [get contains? dissoc assoc empty? apply keys get-in])
  (:require [metaprob.code-handlers :refer :all]
            [metaprob.expander :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.compound :refer :all]))



;; Most general way of creating a generative function: provide implementations of its
;; methods. All other ways of creating generative functions boil down, ultimately, to
;; a call to this function.
(defn make-generative-function
  ([run-in-clojure observe] (make-generative-function run-in-clojure observe {}))
  ([run-in-clojure observe others] (with-meta run-in-clojure (assoc others :observe observe))))

(declare make-observe-implementation-from-traced-code)

;; Create a generative function using executable code that samples from other generative functions.
(defmacro gen [& gen-expr]
  {:style/indent 1}
  (let [expr
        (if (and (not (get (meta &form) :no-expand?)) (gen-transformation &form))
          (map-gen mark-as-already-macroexpanded (mp-expand &form))
          &form)

        body
        (gen-body expr)

        name
        (gen-name expr)

        tracer-name
        (gen-tracer-name expr)

        params
        (gen-pattern expr)

        thunk-name
        (gensym (str (or name "") "thunk"))

        named-fn-body
        (if name
          `((let [~name (~thunk-name)]
              ~@body))
          body)

        innermost-fn-expr
        `(fn ~params ~@named-fn-body)

        run-in-clojure-expr
        (if tracer-name
          `(let [~tracer-name (fn [addr# f# args#] (apply f# args#))]
             ~innermost-fn-expr)
          innermost-fn-expr) ;; TODO: should untraced gens be allowed?

        observe-expression
        `(make-observe-implementation-from-traced-code (fn [~(or tracer-name (gensym 'unused-tracer))] ~innermost-fn-expr))]
        ;(if tracer-name
        ;  `(make-observe-implementation-from-traced-code
        ;       (fn [~tracer-name] ~innermost-fn-expr))
        ;  nil)] ;; TODO: should untraced gens be allowed?

        `(let [~thunk-name (fn ~thunk-name [] (make-generative-function ~run-in-clojure-expr ~observe-expression
                                                                        {:name ~name, :generative-source '~expr}))]
           (~thunk-name))))

;; observe : generative function, observation trace -> generative function
(defn observe [procedure observations]
  ((or (get procedure :observe)
      (fn [observations]
        (gen [& args]
          [(apply procedure args) {} 0]))) observations))


;; Helper used by macroexpanded (gen ...) code.
(defn make-observe-implementation-from-traced-code [fn-accepting-tracer]
  (fn [observations]
    (gen {:tracing-with u} [& args]
      (let [score (atom 0)
            trace (atom {})
            t (fn [addr traceable args]
                (let [[v tr s] (u addr (observe traceable (maybe-subtrace observations addr)) args)]
                  (swap! score + s)
                  (swap! trace merge-subtrace addr tr)
                  v))
            result (apply (fn-accepting-tracer t) args)]
          [result (deref trace) (deref score)]))))



;; Create a "primitive" generative function out of a sampler and scorer
(defn make-primitive [sampler scorer]
  (make-generative-function
    sampler
    (fn [observations]
      (if (trace-has-value? observations)
        (gen [& args]
          [(trace-value observations)
           {:value (trace-value observations)}
           (scorer (trace-value observations) args)])
        (gen {:tracing-with t} [& args]
          (let [result (t '() (make-primitive sampler scorer) args)]
            [result {:value result} 0]))))))


(defn inf [model impl]
  (make-generative-function
    (fn [& args] (first (impl args {})))
    (fn [observations] (gen {:tracing-with t} [& args] (t '() impl [args observations])))
    {:model model}))



;(defmacro generator [expand? gen-expr]
;  {:style/indent 2}
;  (let [expr
;        (if expand? (map-gen mark-as-already-macroexpanded (mp-expand gen-expr)) gen-expr)
;
;        body
;        (gen-body expr)
;
;        name
;        (gen-name expr)
;
;        tracer-name
;        (gen-tracer-name expr)
;
;        params
;        (gen-pattern expr)
;
;        thunk-name
;        (gensym (str (if name name "") "thunk"))
;
;        named-fn-body
;        (if name
;          `((let [~name (~thunk-name)]
;             ~@body))
;          body)
;
;        innermost-fn-expr
;        `(fn ~params ~@named-fn-body)
;
;        clojure-fn-expr
;        (if tracer-name
;          `(let [~tracer-name (fn [~'_ ~'f & ~'args] (apply ~'f ~'args))]
;             ~innermost-fn-expr)
;          innermost-fn-expr)
;
;        clojure-impl-expr
;        (if tracer-name
;          `(fn [~tracer-name] ~innermost-fn-expr)
;          nil)]
;
;        `(let [~thunk-name (fn ~thunk-name [] (with-meta ~clojure-fn-expr {:generative-source '~expr, :clojure-impl ~clojure-impl-expr, :name '~name}))]
;             (~thunk-name))))
;
;(defmacro gen
;  "like fn, but for metaprob procedures"
;  {:style/indent 1}
;  [& form]
;  `(generator
;     ~(not (get (meta &form) :no-expand?))
;     ~&form))

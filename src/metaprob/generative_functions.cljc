(ns metaprob.generative-functions
  (:require [metaprob.code-handlers :refer [gen-body gen-name gen-tracer-name gen-pattern]]
            [metaprob.trace :refer [maybe-subtrace merge-subtrace trace-value trace-has-value?]]))

;; Most general way of creating a generative function: provide implementations of its
;; methods. All other ways of creating generative functions boil down, ultimately, to
;; a call to this function.
(defn make-generative-function
  ([run-in-clojure make-constrained-generator]
   (make-generative-function run-in-clojure make-constrained-generator {}))
  ([run-in-clojure make-constrained-generator others]
   (with-meta run-in-clojure (assoc others :make-constrained-generator make-constrained-generator))))

(declare make-implementation-of-make-constrained-generator-from-traced-code)

;; Create a generative function using executable code that samples from other generative functions.
(defmacro gen [& _]
  {:style/indent 1}
  (let [expr
        &form

        body
        (gen-body expr)

        name
        (gen-name expr)

        tracer-name
        (gen-tracer-name expr)

        params
        (gen-pattern expr)

        thunk-name
        (if name (gensym (str name "thunk")) nil)

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

        make-constrained-generator-expression
        `(make-implementation-of-make-constrained-generator-from-traced-code
           (fn [~(or tracer-name (gensym 'unused-tracer))] ~innermost-fn-expr))

        generative-function-expression
        `(make-generative-function ~run-in-clojure-expr ~make-constrained-generator-expression
                                   {:name '~name, :generative-source '~expr})]

    (if name
      `((fn ~thunk-name [] ~generative-function-expression))
      generative-function-expression)))

;; make-constrained-generator : generative function, observation trace -> generative function
(defn make-constrained-generator [procedure observations]
  ((or (get (meta procedure) :make-constrained-generator)
      (fn [observations]
        (gen [& args]
          [(apply procedure args) {} 0]))) observations))


;; Helper used by macroexpanded (gen ...) code.
(defn make-implementation-of-make-constrained-generator-from-traced-code
  [fn-accepting-tracer]
  (fn [observations]
    (gen {:tracing-with u} [& args]
      (let [score (atom 0)
            trace (atom {})
            t (fn [addr gf args]
                (let [[v tr s] (u addr (make-constrained-generator gf (maybe-subtrace observations addr)) args)]
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


(def infer-and-score
  (gen {:tracing-with t} [& {:keys [procedure inputs observation-trace]
                             :or {inputs [], observation-trace {}}}]
       (t '() (make-constrained-generator procedure observation-trace) inputs)))

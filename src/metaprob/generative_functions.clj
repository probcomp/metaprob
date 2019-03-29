(ns metaprob.generative-functions
  (:require [metaprob.code-handlers :refer :all]
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

(defn trace-at [& args]
  (assert false "Cannot invoke trace-at outside of a (gen ...) form."))


(defmacro let-traced [bindings & body]
  (let [binding-pairs (partition 2 bindings)

        trace-with-name
        (fn trace-with-name [expr name]
          (cond
            (if-expr? expr)
            `(if ~(trace-with-name (if-predicate expr) name)
               ~(trace-with-name (if-then-clause expr) name)
               ~(trace-with-name (if-else-clause expr) name))

            (do-expr? expr)
            (cons 'do (map #(trace-with-name % name) (rest expr)))

            (or (not (seq? expr))
                (special-symbol? (first expr))
                (let-expr? expr)
                (let-traced-expr? expr)
                (fn-expr? expr)
                (gen-expr? expr))
            expr

            ;; If a macro, try expanding
            (not= (macroexpand-1 expr) expr)
            (recur (macroexpand-1 expr) name)

            true
            `(~'trace-at ~name ~(first expr) ~(vec (rest expr)))))

        convert-binding
        (fn [[lhs rhs]]
          (if (symbol? lhs)
            [lhs (trace-with-name rhs (str lhs))]
            [lhs rhs]))

        new-bindings (vec (apply concat (map convert-binding binding-pairs)))]
    `(let ~new-bindings ~@body)))

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
        'trace-at

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
        `(let [~tracer-name (fn [addr# f# & maybe-args#] (let [args# (if maybe-args# (first maybe-args#) [])] (apply f# args#)))]
           ~innermost-fn-expr)

        make-constrained-generator-expression
        `(make-implementation-of-make-constrained-generator-from-traced-code
           (fn [~tracer-name] ~innermost-fn-expr))

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
    (gen [& args]
      (let [score (atom 0)
            trace (atom {})
                  (swap! score + s)
            t (fn [addr gf & maybe-args]
                (let [args (if maybe-args (first maybe-args) [])
                      [v tr s] (trace-at addr (make-constrained-generator gf (maybe-subtrace observations addr)) args)]
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
        (gen [& args]
          (let [result (trace-at '() (make-primitive sampler scorer) args)]
            [result {:value result} 0]))))))


(def infer-and-score
  (gen [& {:keys [procedure inputs observation-trace]
           :or {inputs [], observation-trace {}}}]
    (trace-at '() (make-constrained-generator procedure observation-trace) inputs)))

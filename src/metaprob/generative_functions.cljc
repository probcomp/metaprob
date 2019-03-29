(ns metaprob.generative-functions
  (:require #?(:cljs [cljs.analyzer :as ana])
            [metaprob.code-handlers :as code]
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

(defn at [& args]
  (assert false "Cannot invoke at outside of a (gen ...) form."))

(defn apply-at [& args]
  (assert false "Cannot invoke apply-at outside of a (gen ...) form."))

(defmacro let-traced [bindings & body]
  (let [binding-pairs (partition 2 bindings)

        trace-with-name
        (fn trace-with-name [expr name]
          (cond
            (code/if-expr? expr)
            `(if ~(trace-with-name (code/if-predicate expr) name)
               ~(trace-with-name (code/if-then-clause expr) name)
               ~(trace-with-name (code/if-else-clause expr) name))

            (code/do-expr? expr)
            (cons 'do (map #(trace-with-name % name) (rest expr)))

            (or (not (seq? expr))
                (special-symbol? (first expr))
                (code/let-expr? expr)
                (code/let-traced-expr? expr)
                (code/fn-expr? expr)
                (code/gen-expr? expr))
            expr

            ;; If a macro, try expanding
            (not= #?(:clj (macroexpand-1 expr)
                     :cljs (ana/macroexpand-1 &env expr))
                  expr)
            (recur #?(:clj (macroexpand-1 expr)
                      :cljs (ana/macroexpand-1 &env expr))
                   name)

            true
            `(~'at ~name ~@expr)))

        convert-binding
        (fn [[lhs rhs]]
          (if (symbol? lhs)
            [lhs (trace-with-name rhs (str lhs))]
            [lhs rhs]))

        new-bindings (vec (apply concat (map convert-binding binding-pairs)))]
    `(let ~new-bindings ~@body)))

;; Create a generative function using executable code that samples from other generative functions.
#?(:clj (defmacro gen [& _]
          {:style/indent 1}
          (let [expr
                &form

                body
                (gen-body expr)

                name
                (gen-name expr)

                tracer-name
                'at

                apply-tracer-name
                'apply-at

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
                `(let [~tracer-name (fn [addr# f# & args#] (apply f# args#))
                       ~apply-tracer-name (fn [addr# f# args#] (apply f# args#))]
                   ~innermost-fn-expr)

                make-constrained-generator-expression
                `(make-implementation-of-make-constrained-generator-from-traced-code
                  (fn [~tracer-name ~apply-tracer-name] ~innermost-fn-expr))

                generative-function-expression
                `(make-generative-function ~run-in-clojure-expr ~make-constrained-generator-expression
                                           {:name '~name, :generative-source '~expr})]

            (if name
              `((fn ~thunk-name [] ~generative-function-expression))
              generative-function-expression))))

;; make-constrained-generator : generative function, observation trace -> generative function
#?(:clj (defn make-constrained-generator [procedure observations]
          ((or (get (meta procedure) :make-constrained-generator)
               (fn [observations]
                 (gen [& args]
                   [(apply procedure args) {} 0]))) observations)))


;; Helper used by macroexpanded (gen ...) code.
#?(:clj (defn make-implementation-of-make-constrained-generator-from-traced-code
          [fn-accepting-tracer]
          (fn [observations]
            (gen [& args]
              (let [score (atom 0)
                    trace (atom {})
                    apply-traced
                    (fn [addr gf args]
                      (let [[v tr s] (apply-at addr (make-constrained-generator gf (maybe-subtrace observations addr)) args)]
                        (swap! score + s)
                        (swap! trace merge-subtrace addr tr)
                        v))
                    at-impl
                    (fn [addr gf & args]
                      (apply-traced addr gf args))
                    result (apply (fn-accepting-tracer at-impl apply-traced) args)]
                [result (deref trace) (deref score)])))))



;; Create a "primitive" generative function out of a sampler and scorer
#?(:clj (defn make-primitive [sampler scorer]
          (make-generative-function
           sampler
           (fn [observations]
             (if (trace-has-value? observations)
               (gen [& args]
                 [(trace-value observations)
                  {:value (trace-value observations)}
                  (scorer (trace-value observations) args)])
               (gen [& args]
                 (let [result (apply-at '() (make-primitive sampler scorer) args)]
                   [result {:value result} 0])))))))


#?(:clj (def infer-and-score
          (gen [& {:keys [procedure inputs observation-trace]
                   :or {inputs [], observation-trace {}}}]
            (apply-at '() (make-constrained-generator procedure observation-trace) inputs))))

(ns metaprob.expander
  (:require #?(:cljs [cljs.analyzer :as ana])
            [metaprob.code-handlers :as code]))

(declare mp-expand)

;; Translate a Clojure fn* expression, which defines a potentially
;; anonymous function that dispatches to different function bodies
;; based on number of arguments. Our Metaprob implementation creates
;; a variadic (gen [& args] ...) that dispatches on (count args).
(defn convert-fn*-exp
  #?(:clj [exp] :cljs [env exp])
  (let [mp-expand #?(:clj mp-expand :cljs #(mp-expand env %))
        name (if (symbol? (second exp)) (second exp) nil),

        ;; Rest of the fn* expression, without the word fn* or
        ;; the (optional) function name
        exp-wo-name (if name (rest (rest exp)) (rest exp)),

        ;; We are looking at either
        ;; ([params...] body) or
        ;; (([params1...] body1) ([params2...] body2)...)
        sigs (if (vector? (first exp-wo-name))
               (list exp-wo-name) ;; One signature
               exp-wo-name),      ;; Multiple signatures

        ;; Helpers for counting arguments in a signature.
        count-vars (fn [pat] (count (take-while (partial not= '&) pat))),
        variadic? (fn [pat] (contains? pat '&)),

        ;; Come up with a name for the single variadic argument of the new fn
        argname (gensym),

        ;; arities: a map from number-of-arguments to signature-to-execute
        ;; variadic-n: either false, if this function has no variadic signature,
        ;;    or the number of non-variadic args in the variadic signature
        [arities variadic-n]
        (reduce
         (fn [[arities variadic-n] sig]
           (let [pat (first sig)]
             (assert (not (contains? arities (count-vars pat)))  "Repeat arity in fn*")
             (assert (or (not (variadic? pat)) (not variadic-n)) "Only one variadic allowed in fn*")
             (assert (or (not (variadic? pat)) (> (count-vars pat) (apply max (keys pat))))
                     "Non-variadic cannot have more args than variadic implementation")
             [(assoc arities
                     (count-vars pat)
                     `(let
                          [~pat ~argname]
                        ~@(rest sig))),
              (if (variadic? pat) (count-vars pat) variadic-n)]))
         [{} false] sigs),

        ;; Generate cond clauses for the body of the synthesized
        ;; gen expression.
        clauses
        (apply concat
               (concat
                (map
                 (fn [a]
                   `((= (count ~argname) ~a) ~(mp-expand (get arities a))))
                 (sort (keys arities)))
                (list (if variadic-n
                        `(true ~(mp-expand (get arities variadic-n)))
                        `(true (~'assert false "Wrong arity")))))),

        fn-expr (if (= (count sigs) 1)
                  (cons 'fn
                        (cons (first (first sigs))
                              (map mp-expand
                                   (rest (first sigs)))))
                  `(~'fn [& ~argname] (cond ~@clauses)))]

    (if name
      (cons (first fn-expr) (cons name (rest fn-expr)))
      fn-expr)))

(defn map-from-pairs
  [& pairs]
  (into {} pairs))

(defn vector-from-elems
  [& elems]
  (vec elems))

(defn expand-let-expr
  #?(:clj [form] :cljs [env form])
  (if (empty? (code/let-bindings form))
    (mp-expand #?(:cljs env) `((~'fn [] ~@(code/let-body form))))
    (let [[first-name first-value] (first (code/let-bindings form))
          other-bindings (apply concat (rest (code/let-bindings form)))]
      (mp-expand env `((~'fn [~first-name]
                        (let [~@other-bindings]
                          ~@(code/let-body form))) ~first-value)))))


(defn mp-expand
  #?(:clj [form] :cljs [env form])
  ;; (pprint/pprint ["Expanding" form])
  (let [mp-expand #?(:clj mp-expand :cljs #(mp-expand env %))]
    (cond
      ;; Vector literals
      (vector? form)
      `(vector-from-elems ~@(map mp-expand form))

      ;; Map literals
      (map? form)
      `(map-from-pairs ~@(map (fn [[k v]] `(vector-from-elems ~(mp-expand k) ~(mp-expand v))) form))

      ;; Other literals
      (or (nil? form) (not (seq? form)))
      form

      true
      (case (if (symbol? (first form)) (name (first form)) "")
        ;; Metaprob special forms
        "quote"
        form

        "gen"
        (code/map-gen mp-expand form)

        "do"
        (mp-expand `((fn* [] ~@(rest form))))
        ;; (cons 'do (doall (map mp-expand (rest form))))

        "let"
        #?(:clj (expand-let-expr form)
           :cljs (expand-let-expr env form))

        "letfn"
        form

        "if"
        (map mp-expand form)

        ;; Clojure special forms that need translating
        "let*"
        (mp-expand (cons 'let (rest form)))

        "letfn*"
        ;; TODO: Replace with letgen when we have it.
        (mp-expand (cons 'let (rest form)))

        "fn*"
        ;; We need to handle cases where the `fn` has a name (and
        ;; therefore may be recursive) and also cases where it may have
        ;; more than one arity defined.
        ;; no recursive call, because convert-fn*-exp handles all expansion
        #?(:clj (convert-fn*-exp form)
           :cljs (convert-fn*-exp env form))

        "loop"
        (throw (ex-info "Cannot use loop* in Metaprob." {}))

        "case*"
        (throw (ex-info "Cannot use case* in Metaprob." {}))

        "throw"
        `(~'assert false ~(str "Clojure throw statement encountered: " form))

        ;; TOTAL HACK and should be removed or made to work:
        "." (cons '. (map mp-expand (rest form)))

        ("new" "monitor-exit" "monitor-enter" "try" "finally"
         "import*" "deftype*" "set!" "var" "catch" "def" "reify*")
        (throw (ex-info "mp-expand encountered unsupported Clojure special form"
                        {:form form}))

        ;; It's a function or macro...
        (let [next #?(:clj (macroexpand-1 form)
                      :cljs (ana/macroexpand-1 env form))]
          (if (= next form)
            (map mp-expand form)
            (mp-expand next)))))))

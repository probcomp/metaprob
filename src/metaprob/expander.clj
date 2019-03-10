(ns metaprob.expander
  (:refer-clojure :exclude [get contains? dissoc assoc empty? keys get-in])
  (:require [metaprob.code-handlers :refer :all]
            [metaprob.compound :refer :all]
            [clojure.pprint :as pprint]))

(declare mp-expand)

(def transformations (atom {}))

;; Most permissive:
;; Strip transform,
;; Expand body,
;; Pass to transform,
;; Use as syntax.
;; "mark-as-macroexpanded" can only happen after transform is done.
;; Gen body is expanded given to transformation, new gen expression comes out, which is evaluated.

(defn register-transformation!
  [name f]
  (swap! transformations assoc name f))

;; Translate a Clojure fn* expression, which defines a potentially
;; anonymous function that dispatches to different function bodies
;; based on number of arguments. Our Metaprob implementation creates
;; a variadic (gen [& args] ...) that dispatches on (count args).
(defn convert-fn*-exp [exp]
  (let
    [name (if (symbol? (second exp)) (second exp) nil),

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

     ;; Come up with a name for the single variadic argument of the gen
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
              (list (if variadic-n `(true ~(mp-expand (get arities variadic-n))) `(true (~'assert false "Wrong arity")))))),

     gen-expr (if (= (count sigs) 1)
                (cons 'gen (cons (first (first sigs)) (map mp-expand (rest (first sigs)))))
                `(~'gen [& ~argname] (cond ~@clauses)))]

    (if name
      `(let [~name ~gen-expr] ~name)
      gen-expr)))

(defmacro or-nil?
  ([] nil)
  ([x] x)
  ([x & next] `(let [or# ~x] (if (nil? or#) (or-nil? ~@next) or#))))

(defn map-from-pairs
  [& pairs]
  (into {} pairs))

(defn vector-from-elems
  [& elems]
  (vec elems))

(defn expand-let-expr [form]
  (if (empty? (let-bindings form))
    (mp-expand `((~'gen [] ~@(let-body form))))
    (let [[first-name first-value] (first (let-bindings form))
          other-bindings (apply concat (rest (let-bindings form)))]
      (mp-expand `((~'gen [~first-name]
                     (let [~@other-bindings]
                       ~@(let-body form))) ~first-value)))))


(defn mp-expand [form]
 ; (pprint/pprint ["Expanding" form])
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
      "quote" form

      "gen"
      (if (gen-transformation form)
        (recur
          ((get transformations (gen-transformation form))
           `(~(first form) ~(dissoc (second form) :transform)
             ~(gen-pattern form) ~@(map mp-expand (gen-body form)))))
        (doall (map-gen mp-expand form)))

      "do"
      (recur `((~'gen [] ~@(rest form))))
      ; (cons 'do (doall (map mp-expand (rest form))))

      "let"
      (expand-let-expr form)
      ; (doall (map-let mp-expand form))

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
      ;; no recursive call, b/c all code wrapped in `gen`.
      (convert-fn*-exp form)

      "loop"
      (throw (IllegalArgumentException. "Cannot use loop* in Metaprob."))

      "case*"
      (throw (IllegalArgumentException. "Cannot use case* in Metaprob."))

      "throw"
      `(~'assert false ~(str "Clojure throw statement encountered: " form))

      ;; TOTAL HACK and should be removed or made to work:
      "." (cons '. (map mp-expand (rest form)))

      ("new" "monitor-exit" "monitor-enter" "try" "finally"
           "import*" "deftype*" "set!" "var" "catch" "def" "reify*")
      (throw (IllegalArgumentException. (str "mp-expand encountered unsupported Clojure special form" form)))

      ; It's a function or macro...
      (let [next (macroexpand-1 form)]
        (if (= next form)
          (map mp-expand form)
          (recur next))))))


; Mark every `gen` as having already undergone macroexpansion
(defn mark-as-already-macroexpanded
  [form]
  (cond
    (or (literal? form) (variable? form)) form
    (not (seq? form)) (throw (IllegalArgumentException. (str "Weird form: " form "--of type--" (type form))))

    (gen-expr? form) (vary-meta (map-gen mark-as-already-macroexpanded form) assoc :no-expand? true)
    (let-expr? form) (map-let mark-as-already-macroexpanded form)
    (quote-expr? form) `(~'quote ~(quote-quoted form))

    ((name-checker "letfn") form) form

    true (map mark-as-already-macroexpanded form)))


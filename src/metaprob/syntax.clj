(ns metaprob.syntax
  (:refer-clojure :exclude [get contains? dissoc assoc empty? keys get-in])
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [metaprob.compound :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.builtin-impl :as impl]))

;; This module is intended for import by metaprob code, and defines
;; the syntactic constructs to be used in metaprob programs.
;; Intended to be used with the builtins module.
(defmacro define
  "like def, but allows patterns"
  [pattern rhs]

  (letfn [(var-for-pattern [pat]
            (if (symbol? pat)
              pat
              (gensym (string/join "|" (map var-for-pattern pat)))))

          ;; Insert name into function expression, if any
          (form-def [name rhs]
            (let [rhs (if (and (seq? rhs)
                               (= (first rhs) 'gen))
                        `(~'named-generator ~name ~(not (:no-expand? (meta &form))) ~@(rest rhs))
                        rhs)]
              `(def ~name ~rhs)))

          ;; Returns a list [[var val] ...]
          ;; to be turned into, say, (block (define var val) ...)
          ;; or into (let [var val ...] ...)

          (explode-pattern [pattern rhs]
            (if (symbol? pattern)
              (list (form-def pattern rhs))
              (let [var (var-for-pattern pattern)]
                (cons (form-def var rhs)
                      (mapcat (fn [subpattern i]
                                (if (and (symbol? subpattern)
                                         (= (name subpattern) "_"))
                                  (list)
                                  (explode-pattern subpattern `(nth ~var ~i))))
                              pattern
                              (range (count pattern)))))))
          ]

    `(do ~@(explode-pattern pattern rhs))))

;; Run time

;; This is incredibly crude, but it doesn't much matter if there are a
;; few junk names in the result, since they'll get filtered out
(defn free-vars-approximately
  "Recursively descends through nested vectors, sequences, and maps and returns a
  set of all the symbols found therein."
  [form]
  ;; This is incredibly crude, but it doesn't much matter if there are a
  ;; few junk names in the result, since they'll get filtered out
  (into #{}
        (filter symbol?)
        (tree-seq (some-fn vector? seq? map?)
                  #(if (map? %) (sequence cat %) (seq %))
                  form)))


(defn convert-fn*-exp [exp]
  (let
    [name (if (symbol? (second exp)) (second exp) nil),
     exp-wo-name (if name (rest (rest exp)) (rest exp)),
     sigs (if (vector? (first exp-wo-name))
            (list exp-wo-name)
            exp-wo-name),
     count-vars (fn [pat] (count (take-while (partial not= '&) pat))),
     variadic? (fn [pat] (contains? pat '&)),
     argname (gensym),
     [arities variadic-n]
     (reduce
       (fn [[arities variadic-n] sig]
         (let [pat (first sig)]
           (assert (not (contains? arities (count-vars pat)))  "Repeat arity in fn*")
           (assert (or (not (variadic? pat)) (not variadic-n)) "Only one variadic allowed in fn*")
           (assert (or (not (variadic? pat)) (> (count-vars pat) (apply max (keys pat)))) "Non-variadic cannot have more args than variadic implementation")
           [(assoc arities (count-vars pat) `(~'block
                                               (~'define ~pat ~argname)
                                               ~@(rest sig))), (if (variadic? pat) (count-vars pat) variadic-n)]))

       [{} false]
       sigs),

     clauses
     (apply concat
       (concat
         (map
           (fn [a]
             `((= (count ~argname) ~a) ~(get arities a)))
           (sort (keys arities)))
         (list (if variadic-n `(true ~(get arities variadic-n)) `(true (~'assert false "Wrong arity")))))),

     gen-expr (if (= (count sigs) 1)
                (cons 'gen (first sigs))
                `(~'gen [& ~argname] (cond ~@clauses)))]

    (if name
      `(~'block
         (~'define ~name ~gen-expr) ~name)
      gen-expr)))

(defmacro or-nil?
  ([] nil)
  ([x] x)
  ([x & next] `(let [or# ~x] (if (nil? or#) (or-nil? ~@next) or#))))


(defn mp-expand [form]
  (cond
    (vector? form) (vec (map mp-expand form))
    (map? form) (into {} (map (fn [[k v]] [(mp-expand k) (mp-expand v)]) form))
    ; TODO: nil probably shouldn't be considered compound data
    (or (nil? form) (not (seq? form)))
    form
    true
    (case (first form)
      ; Metaprob special forms
      quote form
      gen   (cons 'gen (cons (second form) (map mp-expand (rest (rest form)))))
      with-explicit-tracer (cons 'with-explicit-tracer (cons (second form) (map mp-expand (rest (rest form)))))
      (block do) (cons 'block (map mp-expand (rest form)))
      define (list 'define (second form) (mp-expand (nth form 2)))
      if (map mp-expand form)
      let* (recur (cons 'block (concat (map #(cons 'define %) (partition 2 (second form))) (rest (rest form)))))
      fn*
        ; We need to handle cases where the `fn` has a name (and therefore may be recursive);
        ; and also cases where it may have more than one arity defined.
      (convert-fn*-exp form) ; no recursive call, b/c all code wrapped in `gen`.
      letfn*
        ; Our `let` can already handle recursive definitions (?).
        (mp-expand (cons 'let (rest form)))
      loop* (throw (IllegalArgumentException. "Cannot use loop* in Metaprob."))
      case* (throw (IllegalArgumentException. "Cannot use case* in Metaprob."))
      throw `(~'assert false (str "Clojure throw statement encountered: " ~form))
      . (cons '. (map mp-expand (rest form)))
      (new monitor-exit monitor-enter try finally clojure.core/import* deftype* set! var catch def reify*)
      (throw (IllegalArgumentException. (str "mp-expand encountered unsupported Clojure special form" form)))

      ; It's a function or macro...
      (let [next (macroexpand-1 form)]
        (if (= next form)
          (map mp-expand form)
          (recur next))))))

;; This can fail with forward references to recursive functions
(defn make-generative [fun name exp top-env names values]
  (p ::making-generative (clojure.core/with-meta
    fun
    {:name (impl/trace-name exp name)
    :generative-source exp
    :environment
    (if (empty? names)
      top-env
      (into {:parent top-env}
            (map (fn [name value] [name value]) names values)))})))

; Takes in a Metaprob environment and a Clojure
; fn expression, and evaluates the fn expression
; in a context where Metaprob local variables + top level environment
; are available.
; TODO: Doesn't work for recursive definitions. We could call this at the end of a block...
; TODO: If we know the name of this function, we can put it in the inner `fn`.
; TODO: But how to make this work for mutually recursive definitions?
(defn eval-fn-expr-in-mp-env
  [mp-env fn-exp]
  (let
    [free-vars (free-vars-approximately fn-exp)
     env-to-bindings
     (fn [mpe so-far]
       (if (instance? clojure.lang.Namespace mpe)
         [so-far mpe]
         (recur
           (get mpe :parent)
           (concat
             (filter (fn [[k v]]
                       (and (symbol? k)
                            (clojure.core/contains? free-vars k))) (unbox mpe))
             so-far))))
     [bindings user-ns] (env-to-bindings mp-env [])
     expr-to-eval `(fn [~@(map first bindings)] ~fn-exp)]
    (apply
      (binding [*ns* user-ns]
        (eval expr-to-eval)) (map second bindings))))

(defn gen-to-clojure-fn [mp-env params body]
  (let [fn-body (if (some #(= '& %) params)
                   (let [screwy (last params)]
                     `(let [~screwy (if (= ~screwy nil) '() ~screwy)]
                        (block ~@body)))
                   `(block ~@body))
         fn-exp `(fn
                   ~params
                   ~fn-body)]
     (eval-fn-expr-in-mp-env mp-env fn-exp)))

; TODO: This does not work with mutual recursion.
(defn compile-mp-proc-fast [mp-proc]
  ; We expect the MP expression to be a map with (at least)
  ; generative-source and
  (let [source (get mp-proc :generative-source)]
    (with-meta
      (gen-to-clojure-fn
        (get mp-proc :environment)
        (second source)
        (rest (rest source)))
      (assoc mp-proc :generative-source (cons 'gen (cons (second source) (map mp-expand (rest (rest source)))))))))

; Change `gen` to `gen-no-expand`, which works the same way but doesn't perform macroexpansion on its source
(defn change-gens [form]
  (cond
    (not (compound? form))
    form
    (vector? form) (vec (map change-gens form))
    (map? form) (into {} (map (fn [[k v]] [(change-gens k) (change-gens v)]) form))
    (not (seq? form)) (throw (IllegalArgumentException. (str "Weird form: " form "--of type--" (type form))))
    (= (first form) 'quote) (cons 'quote (rest form))
    (or (= (first form) 'gen) (= (first form) 'define))
    (vary-meta (cons (first form) (cons (second form) (map change-gens (rest (rest form))))) assoc :no-expand? true)
    ; (cons 'gen-no-expand (cons (second form) (map change-gens (rest (rest form)))))
    true
    (map change-gens form)))


;(if (seq? s)
;  (doall (map doall-recur
;              s))
;  s))
;; Compile time
(defmacro named-generator [name expand? params & body]
  {:style/indent 2}
  (let [expanded-body (if expand? (p ::expanding (map mp-expand body)) body)
        gen-changed-body (if expand? (p ::change-genning (map change-gens expanded-body)) expanded-body)
        fn-body (if (some #(= '& %) params)
                  (let [screwy (last params)]
                    `(let [~screwy (if (= ~screwy nil) '() ~screwy)]
                       (block ~@gen-changed-body)))
                  `(block ~@gen-changed-body)) ;cope with defines if any
        fn-exp `(fn ~@(if name `(~name) `())
                  ~params
                  ~fn-body)
        exp-trace `(~'gen ~params ~@expanded-body) ; TODO: profile
        names (p ::analyzing-free-vars (vec (set/intersection (free-vars-approximately fn-exp)
                                      (set (keys &env)))))]
    ;;(if (not (empty? names))
    ;;  (impl/metaprob-print ["the names are:" names]))

    `(make-generative ~fn-exp
                      '~name
                      '~exp-trace
                      (impl/make-top-level-env *ns*)
                      ;; Or: {name val name val ... "*parent*" top-env}  ?
                      '~(seq names)
                      ~names)))

(defmacro gen
  "like fn, but for metaprob procedures"
  {:style/indent 1}
  [params & body]
  `(named-generator nil ~(not ((meta &form) :no-expand?)) ~params ~@body))

(defmacro gen-no-expand
  {:style/indent 1}
  [params & body]
  `(named-generator nil false ~params ~@body))

;; Oddly, the source s-expressions don't seem to answer true to list?

(defmacro with-explicit-tracer
  [t & body]
  `(let [~t (fn [~'_ ~'f & ~'args] (apply ~'f ~'args))] (block ~@body)))


(defmacro block
  "like do, but for metaprob - supports local definitions"
  [& forms]
  (letfn [(definition? [form]
            (and (seq? form)
                 (symbol? (first form))
                 ;; Can't compare to 'define, wrong namespace
                 (= (name (first form)) "define")
                 (do (assert (= (count form) 3)) true)))
          (definition-pattern [form]
            (second form))
          (definition-rhs [form]
            (nth form 2))
          (generator-definition? [form]
            (and (definition? form)
                 (symbol? (definition-pattern form))
                 (let [rhs (definition-rhs form)]
                   (and (seq? rhs)
                        (symbol? (first rhs))
                        (= (name (first rhs)) "gen")))))
          (qons [x y] ;huh?
            (if (list? y)
              (conj y x)
              (conj (concat (list) y) x)))
          (process-definition [form]
            (assert generator-definition? form)
            (let [rhs (definition-rhs form)       ;a gen-expression
                  prog-pattern (definition-pattern rhs)
                  prog-body (rest (rest rhs))]
              ;; (name [args] body1 body2 ...) as in letfn
              (qons (definition-pattern form)
                    (list prog-pattern
                          (qons 'block prog-body)))))

          ;; Returns let-list [var exp var exp ...]
          (explode-pattern [pattern rhs]
            (if (symbol? pattern)
              (list pattern rhs)
              (let [var (gensym '?subject?)]
                (cons var
                      (cons rhs
                            (mapcat (fn [subpattern i]
                                      (if (= subpattern '_)
                                        (list)
                                        (explode-pattern subpattern `(nth ~var ~i))))
                                    pattern
                                    (range (count pattern))))))))

          (block-to-body [forms]
            (if (empty? forms)
              '()
              (let [here (first forms)
                    more (block-to-body (rest forms))]    ; list of forms
                (if (definition? here)
                  (let [pattern (definition-pattern here)
                        rhs (definition-rhs here)]
                    ;; A definition must not be last expression in a block
                    (if (empty? (rest forms))
                      (print (format "** Warning: Definition of %s occurs at end of block\n"
                                     pattern)))
                    (list
                     (if (generator-definition? here)
                       (let [spec (process-definition here)
                             next (first more)]
                         (if (and (list? next)
                                  (= (first next) `letfn))
                           (do (assert (empty? (rest more)))
                               ;; next = (letfn [...] ...)
                               ;;    (letfn [...] & body)
                               ;; => (letfn [(name pattern & prog-body) ...] & body)
                               (let [[_ specs & body] next]
                                 (qons `letfn
                                       (qons (vec (cons spec specs))
                                             body))))
                           ;; next = (first more)
                           (qons `letfn
                                 (qons [spec]
                                       more))))
                       ;; Definition, but not of a function
                       (qons `let
                             (qons (vec (explode-pattern pattern rhs))
                                   more)))))
                  ;; Not a definition
                  (qons here more)))))

          (formlist-to-form [forms]
            (assert (seqable? forms))
            (if (empty? forms)
              `nil
              (if (empty? (rest forms))
                (first forms)
                (if (list? forms)
                  (qons `do forms)
                  (qons `do (concat (list) forms))))))]
    (formlist-to-form (block-to-body forms))))

;; Don't create variables with these names...
;;   (tbd: look for :meta on a Var in this namespace ??)
(def prohibited-names #{"block" "gen" "define" "if" "quote"})

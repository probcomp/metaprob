(ns metaprob.syntax
  (:require [clojure.string]
            [metaprob.trace :refer :all]
            [metaprob.builtin-impl :as impl]))

(def metaprob-nth impl/metaprob-nth)

;; This module is intended for import by metaprob code, and defines
;; the syntactic constructs to be used in metaprob programs.
;; Intended to be used with the builtins module.

(defmacro define
  "like def, but allows patterns"
  [pattern rhs]

  (letfn [(var-for-pattern [pat]
            (if (symbol? pat)
              pat
              (symbol (clojure.string/join "|"
                                           (map var-for-pattern pat)))))

          ;; Insert name into function expression, if any

          (form-def [name rhs]
            (let [rhs (if (and (seq? rhs)
                               (= (first rhs) 'gen))
                        `(~'named-generator ~name ~@(rest rhs))
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
                                  (explode-pattern subpattern `(metaprob-nth ~var ~i))))
                              pattern
                              (range (count pattern)))))))
          ]

    `(do ~@(explode-pattern pattern rhs))))

(declare from-clojure from-clojure-pattern)

(defn make-generator [fun name params body top]
  (let [exp `(~'gen ~params ~@body)
        exp-trace (from-clojure exp)
        env (impl/make-top-level-env top)
        proc-name (impl/trace-name exp-trace name)]               ;original metaprob
    (trace-as-procedure (trace-from-map {"name" (new-trace proc-name)
                                         "generative-source" exp-trace
                                         ;; Environment is always the top level
                                         ;; env, and often this is incorrect.
                                         "environment" (new-trace env)}
                                        "prob prog")
                        ;; When called from clojure:
                        fun)))          ;; ??!!??

(defmacro named-generator [name params & body]
  {:style/indent 2}
  `(make-generator (fn
                    ~@(if name `(~name) `())
                    ~params
                    (block ~@body))
                  '~name
                  '~params
                  '~body
                  ;; *ns* will be ok at top level as a file is loaded,
                  ;; but will be nonsense at other times.  Fix somehow.
                  ;; (should be lexical, not dynamic.)
                  *ns*))

(defmacro gen
  "like fn, but for metaprob procedures"
  {:style/indent 1}
  [params & body]
  `(named-generator nil ~params ~@body))

(defmacro opaque
  "like fn, but for metaprob procedures"
  {:style/indent 1}
  [params & body]
  `(make-opaque (named-generator nil ~params ~@body)))

;; Oddly, the source s-expressions don't seem to answer true to list?

(defmacro block
  "like do, but for metaprob - supports local definitions"
  [& forms]
  (letfn [(definition? [form]
            (and (seqable? form)
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
                   (and (seqable? rhs)
                        (symbol? (first rhs))
                        (= (name (first rhs)) "gen")))))
          (qons [x y]
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
              (let [var '?subject?]
                (cons var
                      (cons rhs
                            (mapcat (fn [subpattern i]
                                      (if (= subpattern '_)
                                        (list)
                                        (explode-pattern subpattern `(metaprob-nth ~var ~i))))
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

;(defmacro tuple [& members]
;  `(trace-from-map ~(zipmap (range (count members))
;                           (map (fn [x] `(new-trace ~x)) members))))

(defmacro with-addr [addr & body]
  `(do ~addr ~@body))

(defmacro with-address [addr & body]
  `(with-addr ~addr ~@body))

(defmacro &this []
  "(&this) makes no sense when clojure-compiled")

(defmacro mp-splice [x]
  (assert false ["splice used in evaluated position!?" x]))

;; unquote is a name conflict with clojure

(defmacro mp-unquote [x]
  (assert false ["unquote used in evaluated position!?" x]))

(def this "please do not use 'this' in the absence of run-time traces")

; -----------------------------------------------------------------------------

; Convert a clojure expression to a metaprob parse tree / trie.
; Assumes that the input was generated by the to_clojure converter.

; These could all be marked ^:private, but they all contain hyphens,
; which can't occur in metaprob identifiers... for now at least...

(defn from-clojure-seq [seq val]
  (trace-from-subtrace-seq (map from-clojure seq) val))

;; Trace       => clojure               => trace
;; (x)->{a;b;} => (gen [x] (block a b)) => (x)->{a;b;}
;; (x)->{a;}   => (gen [x] (block a))   => (x)->{a;}
;; (x)->a      => (gen [x] a)           => (x)->a
;;
;; safe abbreviation:
;; (x)->{a;b;} => (gen [x] a b) => (x)->{a;b;}

(defn from-clojure-program [exp]
  (let [[_ pattern & body] exp]
    (let [body-exp (if (= (count body) 1)
                     (first body)
                     (cons 'block body))]
      (trace-from-map {"pattern" (from-clojure-pattern pattern)
                       "body" (from-clojure body-exp)}
                      "gen"))))

(defn from-clojure-pattern [pattern]
  (if (symbol? pattern)
    (trace-from-map {"name" (new-trace (str pattern))} "variable")
    (do (assert (vector? pattern) pattern)
        (trace-from-subtrace-seq (map from-clojure-pattern pattern) "tuple"))))

(defn from-clojure-if [exp]
  (let [[_ pred thn els] exp]
    (trace-from-map {"predicate" (from-clojure pred)
                     "then" (from-clojure thn)
                     "else" (from-clojure els)}
                    "if")))

(defn from-clojure-block [exp]
  (from-clojure-seq (rest exp) "block"))

(defn from-clojure-with-address [exp]
  (let [[_ tag ex] exp]
    (trace-from-map {"tag" (from-clojure tag)
                     "expression" (from-clojure ex)}
                    "with_address")))

; This doesn't handle _ properly.  Fix later.

(defn from-clojure-definition [exp]
  (let [[_ pattern rhs] exp
        key (if (symbol? pattern) (str pattern) "definiens")]
    (trace-from-map {"pattern" (from-clojure pattern)
                     key (from-clojure rhs)}
                    "definition")))

(defn from-clojure-application [exp]
  (from-clojure-seq exp "application"))

(defn from-clojure-tuple [exp]
  (from-clojure-application (cons 'tuple exp)))

;; N.b. strings are seqable

(defn literal-exp? [exp]
  (or (number? exp)
      (string? exp)
      (boolean? exp)
      (keyword? exp)))

;; Don't create variables with these names...
;;   (tbd: look for :meta on a Var in this namespace ??)
(def prohibited-names #{"block" "gen" "define" "if"})

(defn from-clojure-1 [exp]
  (cond (vector? exp) (from-clojure-tuple exp)    ;; Pattern - shouldn't happen

        (literal-exp? exp)    ;; including string
          (trace-from-map {"value" (new-trace exp)} "literal")

        (symbol? exp)
        (let [s (str exp)]
          (if (= s "this")
            (trace-from-map {} "this")
            (do (assert (not (contains? prohibited-names s)) exp)
                (trace-from-map {"name" (new-trace s)}
                                "variable"))))

        ;; I don't know why this is sometimes a non-list seq.
        ;; TBD: check that (first exp) is a non-namespaced symbol.
        (seqable? exp) (case (first exp)
                         gen (from-clojure-program exp)
                         probprog  (from-clojure-program exp)    ;DEPRECATED
                         program  (from-clojure-program exp)     ;DEPRECATED
                         if (from-clojure-if exp)
                         block (from-clojure-block exp)
                         mp-splice (trace-from-map {"expression" (from-clojure exp)} "splice")
                         mp-unquote (trace-from-map {"expression" (from-clojure exp)} "unquote")
                         with-address (from-clojure-with-address exp)
                         with-addr (from-clojure-with-address exp)
                         define (from-clojure-definition exp)
                         &this (trace-from-map {} "this")
                         ;; else
                         (from-clojure-application exp))
        ;; Literal
        true (assert false ["bogus expression" exp])))
        
(defn from-clojure [exp]
  (let [answer (from-clojure-1 exp)]
    (assert (trace? answer) ["bad answer" answer])
    answer))

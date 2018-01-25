(ns dontknow.syntax
  (:require [clojure.string]
            [dontknow.trie :refer :all]))

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

          ;; Returns a list [[var val] ...]
          ;; to be turned into, say, (block (define var val) ...)
          ;; or into (let [var val ...] ...)

          (explode-pattern [pattern rhs]
            (if (symbol? pattern)
              (list `(def ~pattern ~rhs))
              (let [var (var-for-pattern pattern)]
                (cons `(def ~var ~rhs)
                      (mapcat (fn [subpattern i]
                                (if (and (symbol? subpattern)
                                         (= (name subpattern) "_"))
                                  (list)
                                  (explode-pattern subpattern `(nth ~var ~i))))
                              pattern
                              (range (count pattern)))))))]

    `(do ~@(explode-pattern pattern rhs))))

(declare from-clojure)

(defn make-program [fun name params body ns]
  (let [exp (from-clojure `(program ~params ~@body))
        env ns]
    (with-meta fun {:name name
                    :trace (trie-from-map {"name" (new-trie exp)
                                           "source" exp
                                           "environment" (new-trie env)}
                                          "prob prog")})))

(defmacro named-program [name params & body]
  `(make-program (fn ~params (block ~@body))
                 '~name
                 '~params
                 '~body
                 ;; *ns* will be ok at top level as a file is loaded,
                 ;; but will be nonsense at other times.  Fix somehow.
                 ;; (should be lexical, not dynamic.)
                 *ns*))

(defmacro program
  "like fn, but for metaprob programs"
  [params & body]
  `(named-program unnamed ~params ~@body))

(defmacro block
  "like do, but for metaprob - supports local definitions"
  [& forms]
  (letfn [(definition? [form]
            (and (seqable? form)
                 ;; Can't compare to 'define, wrong namespace
                 (= (name (first form)) "define")
                 (= (count form) 3)))
          (definition-pattern [form]
            (second form))
          (definition-rhs [form]
            (nth form 2))
          (program-definition? [form]
            (and (definition? form)
                 (symbol? (definition-pattern form))
                 (let [rhs (definition-rhs form)]
                   (and (seqable? rhs)
                        (= (name (first rhs)) "program")))))
          (qons [x y]
            (if (list? y)
              (conj y x)
              (conj (concat (list) y) x)))
          (process-definition [form]
            (assert program-definition? form)
            (let [rhs (definition-rhs form)       ;a program-expression
                  prog-pattern (definition-pattern rhs)
                  prog-body (rest (rest rhs))]
              (qons (definition-pattern form)
                    (qons prog-pattern
                          prog-body))))

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
                                        (explode-pattern subpattern `(nth ~var ~i))))
                                    pattern
                                    (range (count pattern))))))))

          (extend-specs [pattern rhs specs body]
            (let [allow-patterns? true
                  new-specs (vec (if allow-patterns?
                                   (concat (explode-pattern pattern rhs) specs)
                                   (cons pattern (cons rhs specs))))]
              (qons `let
                    (qons new-specs
                          body))))

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
                    (if (program-definition? here)
                      (list
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
                                       more)))))
                      ;; Definition, but not of a function
                      ;; here has the form (def pattern rhs)
                      (list                   ;Single form
                       (let [next (first more)]
                         (if (and (list? next)
                                  (= (first next) `let))
                           ;; Combine two lets into one
                           (let [[_ specs & body] next]
                             (assert (empty? (rest more)))
                             (extend-specs pattern rhs specs body))
                           (extend-specs pattern rhs [] more))))))
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

(defmacro tuple [& members]
  `(trie-from-map ~(zipmap (range (count members))
                           (map (fn [x] `(new-trie ~x)) members))))

(defmacro with-address [addr & body]
  `(do ~addr ~@body))

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
  (trie-from-seq (map from-clojure seq) val))

(defn from-clojure-program [exp]
  (let [[_ pattern & body] exp]
    (let [body-exp (if (= (count body) 1)
                     (first body)
                     (cons `block body))]
      (trie-from-map {"pattern" (from-clojure pattern)    ;Careful!
                      "body" (from-clojure body-exp)}
                     "program"))))

(defn from-clojure-if [exp]
  (let [[_ pred thn els] exp]
    (trie-from-map {"predicate" (from-clojure pred)
                    "then" (from-clojure thn)
                    "else" (from-clojure els)}
                   "if")))

(defn from-clojure-block [exp]
  (from-clojure-seq (rest exp) "block"))

(defn from-clojure-with-address [exp]
  (let [[_ tag ex] exp]
    (trie-from-map {"tag" (from-clojure tag)
                    "expression" (from-clojure ex)}
                   "with_address")))

; This doesn't handle _ properly.  Fix later.

(defn from-clojure-definition [exp]
  (let [[_ pattern rhs] exp
        key (if (symbol? pattern) (str pattern) "definiens")]
    (trie-from-map {"pattern" (from-clojure pattern)
                    key (from-clojure rhs)}
                   "definition")))

(defn from-clojure-application [exp]
  (from-clojure-seq exp "application"))

(defn from-clojure-tuple [exp]
  (from-clojure-seq exp "tuple"))

;; N.b. strings are seqable

(defn literal-exp? [exp]
  (or (number? exp)
      (string? exp)
      (boolean? exp)))

(defn from-clojure-1 [exp]
  (cond (vector? exp) (from-clojure-tuple exp)    ;; Hmm, no, not really.  No translation
        (literal-exp? exp)
        (trie-from-map {"value" (new-trie exp)} "literal")
        ;; I don't know why this is sometimes a non-list seq.
        (seqable? exp) (case (first exp)
                         program (from-clojure-program exp)
                         if (from-clojure-if exp)
                         block (from-clojure-block exp)
                         mp-splice (trie-from-map {"expression" (from-clojure exp)} "splice")
                         mp-unquote (trie-from-map {"expression" (from-clojure exp)} "unquote")
                         tuple (from-clojure-seq exp "tuple")
                         with-address (from-clojure-with-address exp)
                         define (from-clojure-definition exp)
                         ;; else
                         (from-clojure-application exp))
        (= exp `this) (trie-from-map {} "this")
        (symbol? exp) (trie-from-map {"name" (new-trie (str exp))} "variable")
        ;; Literal
        true (assert false ["bogus expression" exp])))
        
(defn from-clojure [exp]
  (let [answer (from-clojure-1 exp)]
    (assert (trie? answer) ["bad answer" answer])
    answer))

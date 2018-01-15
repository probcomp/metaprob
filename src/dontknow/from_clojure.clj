(ns dontknow.from-clojure
  (:require [dontknow.trie :refer :all])
  (:require [dontknow.builtin :refer :all]))

; Convert a clojure expression to a metaprob parse tree / trie.
; Assumes that the input is in the image of the to_clojure converter.

(declare from-clojure)

(defn from-clojure-seq [seq val]
  (trie-from-seq (map from-clojure seq) val))

(defn from-clojure-program [exp]
  (let [[_ pattern & body] exp]
    (let [body-exp (if (= (count body) 1)
                     (first body)
                     (cons 'block body))]
      (trie-from-map {"pattern" (from-clojure pattern)
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

(defn from-clojure-1 [exp]
  (cond (vector? exp) (from-clojure-tuple exp)
        ;; I don't know why this is sometimes a non-list seq.
        (seq? exp) (case (first exp)
                     program (from-clojure-program exp)
                     if (from-clojure-if exp)
                     block (from-clojure-block exp)
                     splice (trie-from-map {"expression" (from-clojure exp)} "splice")
                     unquote (trie-from-map {"expression" (from-clojure exp)} "unquote")
                     with-address (from-clojure-with-address exp)
                     define (from-clojure-definition exp)
                     ;; else
                     (from-clojure-application exp))
        (= exp 'this) (trie-from-map {} "this")
        (symbol? exp) (trie-from-map {"name" (new-trie (str exp))} "variable")
        ;; Literal
        true (do (assert (or (number? exp)
                             (string? exp)
                             (boolean? exp))
                         ["bogus expression" exp])
                 (trie-from-map {"value" (new-trie exp)} "literal"))))
        

(defn from-clojure [exp]
  (let [answer (from-clojure-1 exp)]
    (assert (trie? answer) ["bad answer" answer])
    answer))



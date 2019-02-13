(ns metaprob.code-handlers
  (:refer-clojure :exclude [get contains? dissoc assoc empty? keys get-in])
  (:require [metaprob.compound :refer :all]
            [clojure.pprint :as pprint]))

(defn name-checker
  [n]
  (fn [x]
    (and (seq? x)
         (symbol? (first x))
         (= (name (first x)) n))))

(defn symbol-checker
  [n]
  (fn [x]
    (and (seq? x)
         (= (first x) n))))

(def gen-expr? (name-checker "gen"))

(defn gen-name
  [expr]
  (cond
    (symbol? (second expr)) (second expr)
    (map? (second expr)) (get (second expr) :name)
    true nil))

(defn gen-annotations
  [expr]
  (if (map? (second expr))
    (second expr)
    {}))

(defn gen-transformation
  [expr]
  (if (map? (second expr))
    (get (second expr) :transform) nil))

(defn gen-tracer-name
  [expr]
  (if (map? (second expr))
    (get (second expr) :tracing-with) nil))

(defn gen-has-annotations?
  [expr]
  (not (vector? (second expr))))

(defn gen-pattern
  [expr]
  (if (gen-has-annotations? expr)
    (nth expr 2)
    (second expr)))

(defn gen-body
  [expr]
  (if (gen-has-annotations? expr)
    (rest (rest (rest expr)))
    (rest (rest expr))))

(defn map-gen
  [f gen-expr]
  (if (gen-has-annotations? gen-expr)
    (cons (first gen-expr)
          (cons (second gen-expr)
                (cons (gen-pattern gen-expr) (map f (gen-body gen-expr)))))
    (cons (first gen-expr)
          (cons (gen-pattern gen-expr) (map f (gen-body gen-expr))))))

(def if-expr? (symbol-checker 'if))

(def if-predicate second)

(defn if-then-clause
  [expr]
  (nth expr 2))

(defn if-else-clause
  [expr]
  (if (< (count expr) 4)
    nil
    (nth expr 3)))
;
;(def definition? (name-checker "define"))
;
;(def definition-pattern second)
;
;(defn definition-rhs
;  [expr]
;  (nth expr 2))
;
;(def block-expr? (name-checker "block"))
;
;(def block-body rest)

(def let-expr? (name-checker "let"))
(def let-body #(rest (rest %)))
(def let-bindings #(partition 2 (second %)))
(def let-patterns #(vec (map first (let-bindings %))))
(def let-values #(vec (map second (let-bindings %))))

(def letgen-expr? (name-checker "letgen"))
(def letgen-specs second)
(def letgen-body #(rest (rest %)))

;; If f is a function that transforms expressions,
;; apply it to each Metaprob sub-expression within a let.
(defn map-let
  [f let-expr]
  `(let
     ~(vec (interleave (let-patterns let-expr) (map f (let-values let-expr))))
     ~@(doall (map f (let-body let-expr)))))

(def do-expr? (symbol-checker 'do))
(def do-body rest)

(def variable? symbol?)

(def quote-expr? (symbol-checker 'quote))
(def quote-quoted second)

(defn literal?
  [expr]
  (or
    (and (not (seq? expr)) (not (vector? expr)) (not (map? expr)))
    (empty? expr)))
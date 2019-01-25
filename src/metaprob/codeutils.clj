(ns metaprob.codeutils
  (:refer-clojure :only [declare ns name seq?])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all :exclude [infer-apply]]
            [metaprob.prelude :refer :all]))

(define name-checker
  (gen [n] (gen [x] (and (seq? x) (symbol? (first x)) (= (name (first x)) n)))))

(define symbol-checker
  (gen [n] (gen [x] (and (seq? x) (= (first x) n)))))

(define gen-expr? (name-checker "gen"))

(define gen-pattern second)

(define gen-body
  (gen [expr] (rest (rest expr))))

(define if-expr? (symbol-checker 'if))

(define if-predicate second)

(define if-then-clause
  (gen [expr] (nth expr 2)))

(define if-else-clause
  (gen [expr] (if (< (count expr) 4) nil (nth expr 3))))

(define definition? (name-checker "define"))

(define definition-pattern second)

(define definition-rhs
  (gen [expr] (nth expr 2)))

(define block-expr? (name-checker "block"))

(define block-body rest)

(define variable? symbol?)

(define quote-expr? (symbol-checker 'quote))

(define quote-quoted second)

(define with-explicit-tracer-expr? (name-checker "with-explicit-tracer"))
(define explicit-tracer-var-name second)
(define explicit-tracer-body (gen [expr] (rest (rest expr))))
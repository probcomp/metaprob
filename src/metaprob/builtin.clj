;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin
  (:refer-clojure :exclude
                  [newline not + = boolean? > >= < <= - * / number?
                   symbol? keyword? map? vector? list? not=
                   list sort first second rest
                   and or case cond
                   fn? str cons count nth
                   range concat
                   assert print apply
                   get contains? dissoc assoc empty? keys get-in])
  (:require [metaprob.trace :as trace])
  (:require [metaprob.compound :as compound])
  (:require [metaprob.builtin-impl :as impl]
            [clojure.set :as set]))

(defmacro define-foreign-procedure [mp-name generate-fn]
  (let [namestring (if (clojure.core/symbol? mp-name) (clojure.core/str mp-name) mp-name)]
    `(do (declare ~mp-name)
         (def ~mp-name
           (impl/make-foreign-procedure ~namestring
                                        ~generate-fn)))))

;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

;; General
(define-foreign-procedure = clojure.core/=)
(define-foreign-procedure not= clojure.core/not=)
(define-foreign-procedure assert impl/metaprob-assert)
(define-foreign-procedure error impl/error)
(define-foreign-procedure procedure-name impl/procedure-name)
(define-foreign-procedure trace-name impl/trace-name)

;; Compound
(define-foreign-procedure compound? compound/compound?)
(define-foreign-procedure get compound/get)
(define-foreign-procedure contains? compound/contains?)
(define-foreign-procedure get-in compound/get-in)
(define-foreign-procedure keys compound/keys)
(define-foreign-procedure representation compound/representation)
(define-foreign-procedure empty? compound/empty?)
(define-foreign-procedure listable? compound/listable?)
(define-foreign-procedure to-list compound/to-list)
(define-foreign-procedure list clojure.core/list)
(define-foreign-procedure list? clojure.core/list?)
(define-foreign-procedure vector? clojure.core/vector?)
(define-foreign-procedure map? clojure.core/map?)
(define-foreign-procedure fn? clojure.core/fn?)
(define-foreign-procedure symbol? clojure.core/symbol?)
(define-foreign-procedure str clojure.core/str)
(define-foreign-procedure unbox compound/unbox)
(define-foreign-procedure unbox-all compound/unbox-all)
(define-foreign-procedure to-map compound/to-map)
(define-foreign-procedure assoc impl/assoc)
(define-foreign-procedure dissoc impl/dissoc)

;; Logical
(define-foreign-procedure not clojure.core/not)
(define-foreign-procedure boolean? clojure.core/boolean?)

;; Numeric
(define-foreign-procedure > clojure.core/>)
(define-foreign-procedure >= clojure.core/>=)
(define-foreign-procedure <= clojure.core/<=)
(define-foreign-procedure < clojure.core/<)
(define-foreign-procedure + clojure.core/+)
(define-foreign-procedure - clojure.core/-)
(define-foreign-procedure * clojure.core/*)
(define-foreign-procedure / clojure.core//)
(define-foreign-procedure log impl/log)
(define-foreign-procedure cos impl/cos)
(define-foreign-procedure sin impl/sin)
(define-foreign-procedure log1p impl/log1p)
(define-foreign-procedure exp impl/exp)
(define-foreign-procedure sqrt impl/sqrt)
(define-foreign-procedure normal impl/normal)
(define-foreign-procedure floor impl/floor)
(define-foreign-procedure round impl/round)
(define-foreign-procedure number? clojure.core/number?)
(define-foreign-procedure expt impl/expt)

;; Sample from uniform distribution, with RNG as hidden state
(define-foreign-procedure sample-uniform impl/sample-uniform)

;; Traces
(define-foreign-procedure trace? trace/trace?)
(define-foreign-procedure trace-has-value? trace/trace-has-value?)
(define-foreign-procedure trace-has-subtrace? trace/trace-has-subtrace?)
(define-foreign-procedure trace-subtrace trace/trace-subtrace)
(define-foreign-procedure trace-value trace/trace-value)
(define-foreign-procedure trace-keys trace/trace-keys)
(define-foreign-procedure trace-count trace/subtrace-count)

(define-foreign-procedure trace-set-value trace/trace-set-value)
(define-foreign-procedure trace-clear-value trace/trace-clear-value)
(define-foreign-procedure trace-set-subtrace trace/trace-set-subtrace)
(define-foreign-procedure trace-clear-subtrace trace/trace-clear-subtrace)
(define-foreign-procedure trace-merge trace/trace-merge)

(define-foreign-procedure addresses-of impl/addresses-of)


;; Sequences
(define-foreign-procedure cons clojure.core/cons)
(define-foreign-procedure count clojure.core/count)
(define-foreign-procedure nth clojure.core/nth)
(define-foreign-procedure range clojure.core/range)
(define-foreign-procedure concat clojure.core/concat)
; TODO: Write a version of set difference for MPCompound?
(define-foreign-procedure set-difference impl/set-difference)
(define-foreign-procedure sort clojure.core/sort)
(define-foreign-procedure first clojure.core/first)
(define-foreign-procedure second clojure.core/second)
(define-foreign-procedure rest clojure.core/rest)

;; Environments
(define-foreign-procedure top-level-lookup impl/top-level-lookup)
(define-foreign-procedure top-level-environment? trace/top-level-environment?)
(define-foreign-procedure make-top-level-env impl/make-top-level-env)

;; Printing
(define-foreign-procedure print impl/metaprob-print)
(define-foreign-procedure newline trace/metaprob-newline)
(define-foreign-procedure pprint trace/metaprob-pprint)
(define-foreign-procedure binned-histogram impl/binned-histogram)

;; Special procedures
(define-foreign-procedure inf impl/inf)
(define-foreign-procedure infer-apply impl/infer-apply)

;; -----------------------------------------------------------------------------
;; Work in progress

(define-foreign-procedure generate-foreign impl/generate-foreign)
(define-foreign-procedure make-foreign-procedure impl/make-foreign-procedure)

(def positive-infinity Double/POSITIVE_INFINITY)
(def negative-infinity Double/NEGATIVE_INFINITY)
(defmacro and [& forms] `(clojure.core/and ~@forms))
(defmacro or [& forms] `(clojure.core/or ~@forms))
(defmacro cond [& forms] `(clojure.core/cond ~@forms))
(defmacro case [& forms] `(clojure.core/case ~@forms))

;--- kludge. based on clojure time macro.

(defn report-on-elapsed-time [tag thunk]
  (let [start (. System (nanoTime))
        ret (thunk)
        t (java.lang.Math/round (/ (double (- (. System (nanoTime)) start)) 1000000000.0))]
    (if (> t 1)
      (print (str tag ": elapsed time " t " sec\n")))
    ret))

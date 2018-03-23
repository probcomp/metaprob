;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin
  (:refer-clojure :exclude
                  [not and or
                   assert pprint print
                   list first rest last nth range])
  (:require [metaprob.trace :as trace])
  (:require [metaprob.builtin-impl :as impl]))

(defmacro define-foreign-probprog [mp-name generate-fn]
  (let [namestring (if (symbol? mp-name) (str mp-name) mp-name)]
    `(do (declare ~mp-name)
         (def ~mp-name
           (impl/make-foreign-probprog ~namestring
                                       ~generate-fn)))))

;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

;; General
(define-foreign-probprog eq =)
(define-foreign-probprog neq impl/neq)
(define-foreign-probprog exactly impl/exactly)    ;?
(define-foreign-probprog assert impl/metaprob-assert)
(define-foreign-probprog error impl/error)
(define-foreign-probprog generate-foreign impl/generate-foreign)
(define-foreign-probprog probprog-name impl/probprog-name)
(define-foreign-probprog capture-tag-address impl/capture-tag-address)
(define-foreign-probprog resolve-tag-address impl/resolve-tag-address)

;; Logical
(define-foreign-probprog not clojure.core/not)
(define-foreign-probprog and impl/metaprob-and)
(define-foreign-probprog or impl/metaprob-or)

;; Numeric
(define-foreign-probprog gt >)
(define-foreign-probprog gte >=)
(define-foreign-probprog lte <=)
(define-foreign-probprog lt <)
(define-foreign-probprog add impl/add)
(define-foreign-probprog sub -)
(define-foreign-probprog mul *)
(define-foreign-probprog div /)
(define-foreign-probprog log impl/log)
(define-foreign-probprog cos impl/cos)
(define-foreign-probprog sin impl/sin)
(define-foreign-probprog log1p impl/log1p)
(define-foreign-probprog log-gamma impl/log-gamma)
(define-foreign-probprog exp impl/exp)
(define-foreign-probprog sqrt impl/sqrt)
(define-foreign-probprog normal impl/normal)

;; Sample from uniform distribution, with RNG as hidden state
(define-foreign-probprog sample-uniform impl/sample-uniform)

;; Traces
(define-foreign-probprog empty-trace trace/empty-trace)
(define-foreign-probprog trace-has? trace/trace-has?)
(define-foreign-probprog trace-get trace/trace-get)
(define-foreign-probprog trace-set trace/trace-set)
(define-foreign-probprog lookup trace/lookup)
(define-foreign-probprog trace-delete trace/trace-delete)
(define-foreign-probprog trace-keys trace/trace-keys)
(define-foreign-probprog trace-set-subtrace-at trace/trace-set-subtrace-at)

(define-foreign-probprog addresses-of impl/addresses-of)

;; Lists
(define-foreign-probprog pair trace/pair)
(define-foreign-probprog is-pair trace/metaprob-pair?)
(define-foreign-probprog list impl/metaprob-list)

;; Array/tuple
(define-foreign-probprog list-to-array impl/list-to-tuple)
(define-foreign-probprog array-to-list impl/tuple-to-list)
(define-foreign-probprog is-array trace/metaprob-tuple?)

;; Generic
(define-foreign-probprog first trace/metaprob-first)
(define-foreign-probprog rest trace/metaprob-rest)
(define-foreign-probprog length trace/length)
(define-foreign-probprog last impl/metaprob-last)
(define-foreign-probprog nth impl/metaprob-nth)
(define-foreign-probprog range impl/metaprob-range)
(define-foreign-probprog append impl/append)
(define-foreign-probprog set-difference impl/set-difference)

;; addr - like list
(define-foreign-probprog addr impl/addr)

;; Environments
(define-foreign-probprog env-lookup impl/env-lookup)
(define-foreign-probprog make-env impl/make-env)
(define-foreign-probprog match-bind impl/match-bind)

;; Printing
(define-foreign-probprog print impl/metaprob-print)
(define-foreign-probprog pprint impl/metaprob-pprint)
(define-foreign-probprog binned-histogram impl/binned-histogram)

;; -----------------------------------------------------------------------------
;; Work in progress

(define-foreign-probprog generate-foreign impl/generate-foreign)
(define-foreign-probprog make-foreign-probprog impl/make-foreign-probprog)
(define-foreign-probprog export-probprog impl/export-probprog)

;(define-foreign-probprog make-lifted-probprog impl/make-lifted-probprog)
;(define-foreign-probprog trace-to-probprog impl/trace-to-probprog)


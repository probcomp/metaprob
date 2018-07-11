(ns metaprob.interpreters
  (:refer-clojure :exclude [apply map replicate])
  (:require [metaprob.syntax :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.sequence :refer :all]
            [metaprob.builtin-impl :refer :all]
            [metaprob.prelude :refer [maybe-subtrace maybe-set-subtrace]]
            [metaprob.builtin :as builtin]
            [metaprob.infer :as infer]
            [metaprob.compositional :as comp]))


;; 'user-friendly' tool.

;; Choose either of these, or some other.
;; (def default-interpreter infer/infer-apply)
(def default-interpreter comp/infer-apply)

(define null-trace (trace))

;; Returns [value output-trace score]

(defn infer [& {:keys [procedure inputs intervention-trace
                       target-trace output-trace
                       output-trace?
                       interpreter]}]
  (let [inputs (if (= inputs nil) [] inputs)
        intervention-trace (if (= intervention-trace nil)
                             null-trace
                             intervention-trace)
        target-trace (if (= target-trace nil)
                       null-trace
                       target-trace)
        output-trace? (or output-trace?
                          (not (= output-trace nil)))
        interpreter (if (= interpreter nil)
                      default-interpreter
                      interpreter)
        [value out score]
        (binding [*ambient-interpreter* interpreter]
          (infer-apply procedure
                       inputs
                       intervention-trace
                       target-trace
                       output-trace?))]
    (assert (number? score) score)
    [value
     (if (and output-trace? output-trace out)
       (do (trace-merge! output-trace out)
           (trace-thaw! output-trace)
           output-trace)
       out)
     score]))

;; Returns score only

(defn get-score [proc & inputs]
  (let [[_ target _]
        (infer :procedure proc
               :inputs inputs
               :output-trace? true)]
    (let [[_ _ score]
          (infer :procedure proc
                 :inputs inputs
                 :target-trace target
                 :output-trace? false)]
      score)))

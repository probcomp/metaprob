(ns metaprob.interpreters
  (:refer-clojure :exclude [apply map replicate dissoc assoc])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin-impl :refer :all]
            [metaprob.prelude :refer [maybe-subtrace maybe-set-subtrace]]
            [metaprob.compositional :as comp]))


;; 'user-friendly' tool.

;; Choose either of these, or some other.
;; (def default-interpreter infer/infer-apply)
(def default-interpreter comp/infer-apply)

;; Returns [value output-trace score]

(defn infer [& {:keys [procedure inputs intervention-trace
                       target-trace output-trace? interpreter]}]
  (binding [*ambient-interpreter* (or interpreter default-interpreter)]
    (infer-apply procedure
                 (or inputs [])
                 (or intervention-trace {})
                 (or target-trace {})
                 (not= false output-trace?))))

;; Returns score only
(defn get-score [proc & inputs]
  (let [[_ target _]
        (infer :procedure proc
               :inputs inputs)]
    (let [[_ _ score]
          (infer :procedure proc
                 :inputs inputs
                 :target-trace target
                 :output-trace? false)]
      score)))

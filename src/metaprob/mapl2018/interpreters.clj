(ns metaprob.mapl2018.interpreters
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin-impl :as impl]
            [metaprob.infer :refer [infer-apply]]))

(defn infer [& {:keys [procedure inputs intervention-trace
                       target-trace output-trace]}]
  (infer-apply procedure
               inputs
               intervention-trace
               target-trace
               output-trace))

(defn interpret [& {:keys [program inputs interventions]}]
  (impl/metaprob-nth (infer-apply program inputs interventions nil  nil)
                     0))


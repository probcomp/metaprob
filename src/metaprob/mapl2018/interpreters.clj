(ns metaprob.mapl2018.interpreters
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :as b]
            [metaprob.prelude :as p]
            [metaprob.metacirc.query :as query]))

(defn query [& {:keys [procedure inputs intervention-trace
                       target-trace output-trace]}]
  ;; (print (format "query: output_trace = %s" output-trace))
  (query/query procedure
               inputs
               intervention-trace
               target-trace
               output-trace))

(defn interpret [& {:keys [program inputs interventions]}]
  (b/nth (query/query program inputs interventions nil  nil)
         0))


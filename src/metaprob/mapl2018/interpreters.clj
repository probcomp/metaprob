(ns metaprob.mapl2018.interpreters
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :as b]
            [metaprob.prelude :as p]
            [metaprob.metacirc.interpret :as interpret]
            [metaprob.metacirc.trace-choices :as trace_choices]
            [metaprob.metacirc.query :as query]))

(defn interpret [& {:keys [program inputs interventions]}]
  (interpret/interpret program inputs interventions))

(defn query [& {:keys [probprog inputs intervention-trace
                       target-trace output-trace]}]
  (query/query probprog inputs
               intervention-trace
               target-trace output-trace))

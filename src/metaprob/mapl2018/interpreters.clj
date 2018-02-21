(ns metaprob.mapl2018.interpreters
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :as b]
            [metaprob.prelude :as p]
            [metaprob.metacirc.interpret :as interpret]
            [metaprob.metacirc.trace-choices :as trace_choices]
            [metaprob.metacirc.propose-and-trace-choices :as propose_and_trace_choices]))

(defn interpret [& {:keys [program inputs interventions]}]
  (interpret/interpret program inputs interventions))


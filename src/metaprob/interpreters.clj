(ns metaprob.interpreters
  (:refer-clojure :only [declare ns defn])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :as builtin]
            [metaprob.infer :as infer]
            [metaprob.compositional :as comp]))

;; Returns [value score]

(defn infer [& {:keys [procedure inputs intervention-trace
                       target-trace output-trace]}]
  (infer/infer-apply procedure
                     inputs
                     intervention-trace
                     target-trace
                     output-trace))

(def inf infer/inf)
(def opaque infer/opaque)
(def apply infer/apply)

(def map infer/map)
(def replicate infer/replicate)


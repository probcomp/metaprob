(ns metaprob.interpreters
  (:refer-clojure :exclude [apply map replicate])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :as builtin]
            [metaprob.infer :as infer]
            [metaprob.compositional :as comp]))

(defn null-interpreter [proc inputs intervene target output]
  (clojure.core/apply proc inputs))

(def ^:dynamic *interpreter* null-interpreter)

(def pure-interpreter comp/infer-apply)

(defn effectful-interpreter [proc inputs intervene target output?]
  (let [output (if output? (builtin/empty-trace) nil)]
    (let [[value score]
          (infer/infer-apply proc inputs intervene target output)]
      [value output score])))

(def default-interpreter effectful-interpreter)

;; Returns [value ?output-trace? score]

(defn infer [& {:keys [procedure inputs intervention-trace
                       target-trace output-trace
                       output-trace?
                       interpreter]}]
  (let [output-trace? (if (= output-trace? nil)    ;Force it to be boolean
                        (not (= output-trace nil))
                        output-trace?)
        [value out score]
        (binding [*interpreter* (or interpreter default-interpreter)]
          (*interpreter* procedure
                         inputs
                         intervention-trace
                         target-trace
                         output-trace?))]
    (if (and output-trace? output-trace out)
      (builtin/trace-merge! output-trace out))
    [value out score]))

(def opaque infer/opaque)
(def apply infer/apply)

(def map infer/map)
(def replicate infer/replicate)


;; This module is intended for import by metaprob code.

(ns metaprob.prelude
  (:refer-clojure :exclude [map reduce apply replicate])
  (:require [clojure.set :as set]
            [metaprob.trace :refer :all]
            [metaprob.generative-functions :refer [gen make-generative-function make-constrained-generator]]
            [clojure.java.io :as io])
  (:import (java.util Random)))


;; Useful math
(defn exp [x] (Math/exp x))
(defn expt [x y] (Math/pow x y))
(defn sqrt [x] (Math/sqrt x))
(defn log [x] (Math/log x))
(defn cos [x] (Math/cos x))
(defn sin [x] (Math/sin x))
(defn log1p [x] (Math/log1p x))
(defn floor [x] (Math/floor x))
(defn round [x] (Math/round x))
(def negative-infinity Double/NEGATIVE_INFINITY)

;; Randomness
(def ^:dynamic *rng* (Random. 42))
(defn sample-uniform
  ([] (.nextDouble *rng*))
  ([a b] (+ a (* (.nextDouble *rng*) (- b a)))))

;; Set difference
(defn set-difference [s1 s2]
  (seq (set/difference (set s1) (set s2))))

;; Apply
(def apply
  (with-meta clojure.core/apply {:apply? true}))


;; Eager, generative-function versions of common list functions
(def map
  (gen {:tracing-with t}
       [f l]
       (doall (map-indexed (gen [i x] (t i f [x])) l))))

(def replicate
  (gen {:tracing-with t} [n f]
       (map (gen [i] (t i f [])) (range n))))

(def doall*
  (gen [s]
       (dorun (tree-seq seq? seq s)) s))
;; -----------------------------------------------------------------------------
;; Graphical output (via gnuplot or whatever)

(defn binned-histogram [& {:keys [name samples overlay-densities
                                  sample-lower-bound sample-upper-bound
                                  number-of-intervals]}]
  (let [samples (seq samples)
        sample-lower-bound (or sample-lower-bound -5)
        sample-upper-bound (or sample-upper-bound 5)
        number-of-intervals (or number-of-intervals 20)
        fname (clojure.string/replace name " " "_")
        path (str "results/" fname ".samples")
        commands-path (str path ".commands")]
    (print (format "Writing commands to %s for histogram generation\n" commands-path))
    ;;(print (format " overlay-densities = %s\n" (freeze overlay-densities)))
    (with-open [writor (io/writer commands-path)]
      (.write writor (format "reset\n"))
      (.write writor (format "min=%s.\n" sample-lower-bound))
      (.write writor (format "max=%s.\n" sample-upper-bound))
      (.write writor (format "n=%s\n" number-of-intervals))
      (.close writor))
    (print (format "Writing samples to %s\n" path))
    (with-open [writor (io/writer path)]
      (doseq [sample samples]
        (.write writor (str sample))
        (.write writor "\n"))
      (.close writor))))

;; (defn print-source [f] (clojure.pprint/pprint (get (meta f) :generative-source)))
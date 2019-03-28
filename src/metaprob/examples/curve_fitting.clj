(ns metaprob.examples.curve-fitting
  (:refer-clojure :exclude [map replicate apply])
  (:require [metaprob.trace :refer :all]
            [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer [map expt]]
            [metaprob.distributions :refer :all]
            [metaprob.inference :refer :all]))


;; Generate a random polynomial of degree 0, 1, 2, or 3
(def random-polynomial
  (gen {:tracing-with t} []
    (let [degree (t "degree" uniform-discrete [[0 1 2 3]])
          coeffs (map (fn [i] (t `("coeffs" ~i) gaussian [0 1])) (range (inc degree)))]
      (fn [x]
        (reduce + (map-indexed (fn [n c] (* c (expt x n))) coeffs))))))

;; Create a generative function that is a noisy version of
;; a deterministic input function
(def add-noise
  (gen {:tracing-with t} [f]
    (let [noise-level (t "noise" gamma [1 1])
          prob-outlier (t "prob-outlier" beta [1 10])]
      (gen {:tracing-with u} [x]
        (if (u "outlier?" flip [prob-outlier])
          (u "y" gaussian [0 10])
          (u "y" gaussian [(f x) noise-level]))))))

;; Given a list of xs, create a list of ys that are related
;; via a noisy polynomial relationship
(def curve-model
  (gen {:tracing-with t} [xs]
    (let [underlying-curve (t "underlying-curve" random-polynomial [])
          noisy-curve (t "add-noise" add-noise [underlying-curve])]
      (doall (map-indexed (fn [i x] (t `("data" ~i) noisy-curve [x])) xs)))))

;; Useful helpers for curve-fitting
(defn make-observation-trace
  [ys]
  {"data" (into {} (map-indexed (fn [i y] [i {"y" {:value y}}]) ys))})


;; Create datasets
(def point-count 10)
(def x-min -5)
(def x-max 5)
(def x-range (- x-max x-min))
(def x-interval (/ x-range (- point-count 1)))

(def xs (map-indexed (fn [i interval] (+ -5. (* interval i))) (repeat point-count x-interval)))

;; Add a random outlier to a dataset
(defn add-outlier [ys]
  (let [idx (uniform-discrete (range (count ys)))]
    (map-indexed (fn [i y] (if (= i idx) (gaussian 0 10) y)) ys)))

;; y = 2x + 1
(def ys-linear (map #(+ (* 2 %) 1) xs))

;; y = 2x + 1, with two outliers
(def ys-linear-outlier (add-outlier (add-outlier ys-linear)))

;; y = 2x^2 - 2x - 1, with noisy observations and an outlier
(def ys-quadratic (add-outlier (map #(gaussian (- (* 2 % %) (* 2 %) 1) 0.7) xs)))


(defn inference-step [xs]
  (let [curve-step
        (custom-proposal-mh-step
          :model curve-model
          :inputs [xs]
          :proposal (make-resimulation-proposal
                      :model curve-model,
                      :inputs [xs],
                      :address-predicate #(address-contains? % "underlying-curve")))

        noise-step
        (custom-proposal-mh-step
          :model curve-model
          :inputs [xs]
          :proposal (make-resimulation-proposal
                      :model curve-model
                      :inputs [xs]
                      :address-predicate #(address-contains? % "add-noise")))

        outlier-proposal
        (fn [i] (gen {:tracing-with t} [trace]
                  (trace-set-value
                    trace `("data" ~i "outlier?")
                    (not (trace-value trace `("data" ~i "outlier?"))))))

        outlier-steps
        (map (fn [i] (symmetric-proposal-mh-step :model curve-model
                                                 :inputs [xs]
                                                 :proposal (outlier-proposal i)))
             (range (count xs)))

        coeffs-step
        (gaussian-drift-mh-step :model curve-model
                                :inputs [xs]
                                :address-predicate #(address-contains? % "coeffs")
                                :width 0.1)]

    (reduce comp identity `[~curve-step ~coeffs-step ~noise-step ~coeffs-step ~@outlier-steps])))


(defn run-mh [xs ys n]
  (let [[_ initial-trace _]
        (infer-and-score :procedure curve-model :inputs [xs] :observation-trace (make-observation-trace ys))]
    (take n (iterate (inference-step xs) initial-trace))))


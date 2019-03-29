(ns metaprob.examples.curve-fitting
  (:refer-clojure :exclude [map replicate apply])
  (:require [metaprob.trace :refer :all]
            [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer [map expt replicate]]
            [metaprob.distributions :refer :all]
            [clojure.pprint :refer [pprint]]
            [metaprob.inference :refer :all]))

;; Generate a random polynomial of degree 0, 1, 2, or 3
(def random-polynomial
  (gen []
    (let [coeffs (map (fn [i] (trace-at `("coeffs" ~i) gaussian [0 1]))
                       (range (+ 1 (trace-at "degree" uniform-discrete [[0 1 2 3]]))))]
      (fn [x] (reduce + (map-indexed (fn [n c] (* c (expt x n))) coeffs))))))

;; Create a generative function that is a noisy version of
;; a deterministic input function
(def add-noise
  (gen [f]
    (let-traced [noise (gamma 1 1)
                 prob-outlier (beta 1 10)]
      (gen [x]
        (if (trace-at "outlier?" flip [prob-outlier])
          (trace-at "y" gaussian [0 10])
          (trace-at "y" gaussian [(f x) noise]))))))

;; Given a list of xs, create a list of ys that are related
;; via a noisy polynomial relationship
(def curve-model
  (gen [xs]
    (let-traced [underlying-curve (random-polynomial)
                 noisy-curve (add-noise underlying-curve)]
      (doall (map-indexed (fn [i x] (trace-at `("data" ~i) noisy-curve [x])) xs)))))

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
                      :address-predicate #(address-contains? % "noisy-curve")))

        outlier-proposal
        (fn [i] (fn [trace]
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


(defn -main []
  (pprint (last (run-mh xs ys-linear 200)))
  (pprint (last (run-mh xs ys-linear-outlier 300)))
  (pprint (last (run-mh xs ys-quadratic 1000))))
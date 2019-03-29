;; 5.

;; Try this: time lein run -m metaprob.examples.main 10

(ns metaprob.examples.inference-on-gaussian
  (:refer-clojure :exclude [replicate map apply])
  (:require [metaprob.generative-functions :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.inference :refer :all]))

;; Exact versions of prior and target density functions, for
;; graphical comparison with sampled approximations.

(def normal-normal
  (gen []
    (let-traced [x (gaussian 0 1)
                 y (gaussian x 1)]
      y)))

(defn prior-density
   [x]
  (exp (score-gaussian x [0 1])))

(defn target-density
   [x]
  (exp (score-gaussian x [1.5 (/ 1.0 (sqrt 2.0))])))

;; Each sample is an output trace.

;; Find the location of the (assumed unique) peak of the histogram.
;; For debugging.

(defn peak-location
  [samples]
  (let [so (sort samples)
        window (+ 1 (quot (count so) 10))
        lead (drop window so)]
    (nth (first (sort (clojure.core/map (fn [x y] [(- y x) (/ (+ x y) 2)]) so lead))) 1)))

;; For debugging.

(defn analyze
   [samples]
    (print (first samples))
    (print ["average:" (/ (reduce + samples) (count samples))
            "peak:" (peak-location samples)])
    samples)

(defn gaussian-histogram
  [name samples]
  (binned-histogram
    :name    name
    :samples (analyze samples)
    :overlay-densities `(["prior"  ~prior-density]
                         ["target" ~target-density])))

;; Sample from prior & plot

(defn gaussian-prior-samples
  [number-of-runs]
  (replicate number-of-runs normal-normal))

(def obs {"y" {:value 3}})

(defn rejection-assay
  [number-of-runs]
  (replicate
     number-of-runs
     (fn []
       (print "rejection sample") ;Progress meter
       (trace-value
        (rejection-sampling :model normal-normal  ; :model-procedure
                           ; :predicate (fn [tr] (< 2.99 (trace-value tr "y") 3.01)))
                            :observation-trace obs
                            :log-bound 0.5)
        "x"))))

(defn importance-assay
  [n-particles number-of-runs]
    (replicate
     number-of-runs
     (fn []
       (trace-value
        (importance-resampling :model normal-normal  ; :model-procedure
                               :observation-trace obs
                               :n-particles n-particles) "x"))))


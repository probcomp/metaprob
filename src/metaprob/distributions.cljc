(ns metaprob.distributions
  (:refer-clojure :exclude [apply map replicate reduce + - * / < == <= >= >])
  (:require [metaprob.prelude :as mp :refer [map make-primitive]]
            [metaprob.autodiff :as ad]
            #?(:clj [incanter.distributions :as distributions])))

(def exactly
  (make-primitive
   (fn [x] x)
   (fn [y [x]] (if (not= y x) mp/negative-infinity 0))))

(def uniform
  (make-primitive
   (fn [a b] (mp/sample-uniform a b))
   (fn [x [a b]] (if (ad/<= a x b) (ad/- (mp/log (ad/- b a))) mp/negative-infinity))))

(def uniform-discrete
  (make-primitive
   (fn [items] (nth items (Math/floor (ad/* (mp/sample-uniform) (count items)))))
   (fn [item [items]]
     (ad/- (mp/log (count (filter #(= % item) items)))
        (mp/log (count items))))))

(def flip
  (make-primitive
   (fn [weight] (ad/< (mp/sample-uniform) weight))
   (fn [value [weight]]
     (if value
       (mp/log weight)
       (mp/log1p (ad/- weight))))))

(defn normalize-numbers [nums]
  (let [total (clojure.core/reduce ad/+ nums)] (map #(ad// % total) nums)))

(def categorical
  (make-primitive
   (fn [probs]
     (if (map? probs)
       (nth (keys probs) (categorical (normalize-numbers (vals probs)))) ;; TODO: normalization not needed here?
       (let [total (clojure.core/reduce ad/+ probs)
             r (ad/* (mp/sample-uniform) total)]
         (loop [i 0, sum 0]
           (if (ad/< r (ad/+ (nth probs i) sum)) i (recur (inc i) (ad/+ (nth probs i) sum)))))))
   (fn [i [probs]]
     (if (map? probs)
       (if (not (contains? probs i)) mp/negative-infinity (ad/- (mp/log (get probs i)) (mp/log (clojure.core/reduce ad/+ (vals probs)))))
       (mp/log (nth probs i))))))

(defn logsumexp [scores]
  (let [max-score (mp/apply max scores)
        weights (map #(mp/exp (ad/- % max-score)) scores)]
    (ad/+ (mp/log (clojure.core/reduce ad/+ weights)) max-score)))

(defn logmeanexp [scores]
  (ad/- (logsumexp scores) (mp/log (count scores))))

(defn log-scores-to-probabilities [scores]
  (let [log-normalizer (logsumexp scores)]
    (map #(mp/exp (ad/- % log-normalizer)) scores)))


(def log-categorical
  (make-primitive
   (fn [scores]
     (let [probs
           (if (map? scores) (into {} (clojure.core/map (fn [a b] [a b]) (keys scores) (log-scores-to-probabilities (vals scores))))
               (log-scores-to-probabilities scores))]
       (categorical probs)))
   (fn [i [scores]]
     (let [probs
           (if (map? scores) (into {} (clojure.core/map (fn [a b] [a b]) (keys scores) (log-scores-to-probabilities (vals scores))))
               (log-scores-to-probabilities scores))]
       (if (map? probs)
         (if (not (contains? probs i)) mp/negative-infinity (ad/- (mp/log (get probs i)) (mp/log (clojure.core/reduce ad/+ (vals probs)))))
         (mp/log (nth probs i)))))))


(defn generate-gaussian [mu sigma]
  (ad/+ mu (ad/* sigma (mp/sqrt (ad/* -2 (mp/log (mp/sample-uniform)))) (mp/cos (ad/* 2 Math/PI (mp/sample-uniform))))))
(defn standard-gaussian-log-density [x] (ad/* -0.5 (ad/+ (mp/log (ad/* 2 Math/PI)) (ad/* x x))))
(defn score-gaussian [x [mu sigma]]
  (ad/- (standard-gaussian-log-density (ad// (ad/- x mu) sigma)) (mp/log sigma)))

(def gaussian
  (make-primitive
   generate-gaussian
   score-gaussian))

(def geometric
  (make-primitive
   (fn [p] (loop [i 0] (if (flip p) (recur (inc i)) i)))
   (fn [v [p]] (ad/+ (mp/log1p (ad/- p)) (ad/* (mp/log p) v)))))

;; This implementation comes from Anglican:
;;
(defn digamma
  "digamma function psi(x): derivative of gammaln(x),
  apparently bizarrely missing from all Clojure libraries.
  Not yet implemented for negative values of x.
  source: http://en.wikipedia.org/wiki/Digamma_function"
  [x]
  (assert (ad/>= x 0.0))
  (if (ad/<= x 0.0)
    (mp/log 0.0)
    (let [partial-sum (if (ad/< x 1) (ad// -1. x) 0.0)
          x (if (ad/< x 1) (ad/+ x 1.0) x)]
      (ad/+ partial-sum
         (ad/log x)
         (ad// -1. (ad/* 2 x))
         (ad// -1. (ad/* 12 (ad/** x 2)))
         (ad// 1. (ad/* 120 (ad/** x 4)))
         (ad// -1. (ad/* 252 (ad/** x 6)))
         (ad// 1. (ad/* 240 (ad/** x 8)))
         (ad// -5. (ad/* 660 (ad/** x 10)))
         (ad// 691. (ad/* 32760 (ad/** x 12)))
         (ad// -1. (ad/* 12 (ad/** x 14)))))))


; Old: (log (distributions/pdf (distributions/beta-distribution alpha beta) x)))))


#?(:clj
   (def gamma
     (mp/make-primitive
       (fn [shape scale]
         (distributions/draw (distributions/gamma-distribution shape scale)))
       (let [f
             (fn [x shape scale]
               (mp/log (distributions/pdf (distributions/gamma-distribution shape scale) x)))
             df-dx
             (fn [x shape scale]
               (ad/- (ad// (ad/- shape 1.) x) (ad// scale)))
             df-dshape
             (fn [x shape scale]
               (ad/- (mp/log x) (mp/log scale) (digamma shape)))
             df-dscale
             (fn [x shape scale]
               (ad/- (ad// x (mp/expt scale 2)) (ad// shape scale)))
             differentiable-log-pdf
             (ad/lift-real*real*real->real f df-dx df-dshape df-dscale)]
         (fn [x [shape scale]]
           (differentiable-log-pdf x shape scale))))))

#?(:clj
   (def beta
     (mp/make-primitive
       (fn [alpha beta]
         (distributions/draw (distributions/beta-distribution alpha beta)))
       (let
         [f (fn [x alpha beta]
              (mp/log (distributions/pdf (distributions/beta-distribution alpha beta) x)))
          df-dx (fn [x alpha beta]
                  (ad/- (ad// (ad/- alpha 1) x) (ad// (ad/- beta 1) (ad/- 1 x))))
          df-dalpha (fn [x alpha beta]
                      (ad/- (mp/log x) (ad/- (digamma alpha) (digamma (ad/+ alpha beta)))))
          df-dbeta (fn [x alpha beta]
                     (ad/- (mp/log1p (ad/- x)) (ad/- (digamma beta) (digamma (ad/+ alpha beta)))))
          differentiable-log-pdf (ad/lift-real*real*real->real f df-dx df-dalpha df-dbeta)]
         (fn [x [alpha beta]]
           (differentiable-log-pdf x alpha beta))))))

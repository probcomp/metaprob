(ns metaprob.distributions
  (:refer-clojure :exclude [apply map replicate reduce])
  (:require [metaprob.prelude :as mp :refer [map make-primitive]]
            #?(:clj [incanter.distributions :as distributions])))

(def exactly
  (make-primitive
   (fn [x] x)
   (fn [y [x]] (if (not= y x) mp/negative-infinity 0))))

(def uniform
  (make-primitive
   (fn [a b] (mp/sample-uniform a b))
   (fn [x [a b]] (if (<= a x b) (- (mp/log (- b a))) mp/negative-infinity))))

(def uniform-discrete
  (make-primitive
   (fn [items] (nth items (Math/floor (* (mp/sample-uniform) (count items)))))
   (fn [item [items]]
     (- (mp/log (count (filter #(= % item) items)))
        (mp/log (count items))))))

(def flip
  (make-primitive
   (fn [weight] (< (mp/sample-uniform) weight))
   (fn [value [weight]]
     (if value
       (mp/log weight)
       (mp/log1p (- weight))))))

(defn normalize-numbers [nums]
  (let [total (clojure.core/reduce + nums)] (map #(/ % total) nums)))

(def categorical
  (make-primitive
   (fn [probs]
     (if (map? probs)
       (nth (keys probs) (categorical (normalize-numbers (vals probs)))) ;; TODO: normalization not needed here?
       (let [total (clojure.core/reduce + probs)
             r (* (mp/sample-uniform) total)]
         (->> probs
              (reductions +)
              (take-while #(< % r ))
              count))))
   (fn [i [probs]]
     (if (map? probs)
       (if (not (contains? probs i)) mp/negative-infinity (- (mp/log (get probs i)) (mp/log (clojure.core/reduce + (vals probs)))))
       (mp/log (nth probs i))))))

(defn logsumexp [scores]
  (let [max-score (mp/apply max scores)
        weights (map #(Math/exp (- % max-score)) scores)]
    (+ (Math/log (clojure.core/reduce + weights)) max-score)))

(defn logmeanexp [scores]
  (- (logsumexp scores) (mp/log (count scores))))

(defn log-scores-to-probabilities [scores]
  (let [log-normalizer (logsumexp scores)]
    (map #(Math/exp (- % log-normalizer)) scores)))


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
         (if (not (contains? probs i)) mp/negative-infinity (- (mp/log (get probs i)) (mp/log (clojure.core/reduce + (vals probs)))))
         (mp/log (nth probs i)))))))


(defn generate-gaussian [mu sigma]
  (+ mu (* sigma (Math/sqrt (* -2 (Math/log (mp/sample-uniform)))) (Math/cos (* 2 Math/PI (mp/sample-uniform))))))
(defn standard-gaussian-log-density [x] (* -0.5 (+ (Math/log (* 2 Math/PI)) (* x x))))
(defn score-gaussian [x [mu sigma]]
  (- (standard-gaussian-log-density (/ (- x mu) sigma)) (Math/log sigma)))

(def gaussian
  (make-primitive
   generate-gaussian
   score-gaussian))

(def geometric
  (make-primitive
   (fn [p] (loop [i 0] (if (flip p) (recur (+ i 1)) i)))
   (fn [v [p]] (+ (mp/log1p (- p)) (* (mp/log p) v)))))

#?(:clj (def gamma
          (make-primitive
           (fn [shape scale]
             (distributions/draw
              (distributions/gamma-distribution shape scale)))
           (fn [x [shape scale]]
             (mp/log (distributions/pdf
                   (distributions/gamma-distribution shape scale)
                   x))))))

#?(:clj (def beta
          (make-primitive
           (fn [alpha beta]
             (distributions/draw
              (distributions/beta-distribution alpha beta)))
           (fn [x [alpha beta]]
             (mp/log (distributions/pdf
                   (distributions/beta-distribution alpha beta)
                   x))))))

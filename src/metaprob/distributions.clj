(ns metaprob.distributions
  (:refer-clojure :exclude [apply get contains? dissoc assoc empty? keys get-in map replicate reduce])
  (:require [metaprob.builtin :refer :all]
            [metaprob.compound :refer [get contains? empty? keys get-in]]
            [metaprob.prelude :refer :all]
            [incanter.distributions :as distributions]
            [metaprob.syntax :refer :all]
            [metaprob.trace :refer :all]
            [clojure.pprint :as pprint]))


;; TODO: Can exactly's implementation be made traceable?
(def exactly
  (inf
    ; No model
    nil
    ; Implementation
    (gen [[x] observations]
         (let [result
               (if (trace-has-value? observations)
                 (trace-value observations) x)]
           [result {:value result} (if (not= result x) negative-infinity 0)]))))

(def make-inference-procedure-from-sampler-and-scorer
  (gen [sampler scorer]
       (inf
         ;; Model
         nil
         ;(gen {:tracing-with t} [& args]
         ;  (t '() exactly (apply sampler args)))

         ;; Inference implementation
         (gen {:tracing-with t} [args observations]
              (let [result
                    (if (trace-has-value? observations)
                      (trace-value observations)
                      (t '() exactly (apply sampler args)))]
                [result
                 {:value result}
                 (if (trace-has-value? observations)
                   (scorer result args)
                   0)])))))

(def uniform
  (make-inference-procedure-from-sampler-and-scorer
    (gen [a b] (sample-uniform a b))
    (gen [x [a b]]
         (- 0.0 (log (- b a))))))

(def uniform-sample
  (make-inference-procedure-from-sampler-and-scorer
    (gen [items]
         (let [n (uniform 0 (count items))]
           (nth items (floor n))))
    (gen [item [items]]
         (- (log (count (filter (gen [x] (= x item)) items)))
            (log (count items))))))

(def flip
  (make-inference-procedure-from-sampler-and-scorer
    (gen [weight] (< (uniform 0 1) weight))
    (gen [value [weight]]
         (if value
           (log weight)
           (log1p (- 0 weight))))))

;; Cf. CategoricalOutputPSP from discrete.py in Venturecxx
;; This is just the one-argument form, so is simpler than what's in Venture.

(def categorical
  (make-inference-procedure-from-sampler-and-scorer
    (gen [probabilities]
         ;; Returns an index i.
         ;; Assume that probabilities add to 1.
         ;; return simulateCategorical(vals[0], args.np_prng(),
         ;;   [VentureInteger(i) for i in range(len(vals[0]))])
         (let [threshold (uniform 0 1)
               scan (gen scan [i probs running-prob]
                         (if (empty? probs)
                           (- i 1)
                           (let [next-prob (+ (first probs) running-prob)]
                             (if (> next-prob threshold) i (scan (+ i 1) (rest probs) next-prob)))))]
           (scan 0 probabilities 0.0)))
    (gen [i [probabilities]]
         ;; return logDensityCategorical(val, vals[0],
         ;;   [VentureInteger(i) for i in range(len(vals[0]))])
         (log (nth probabilities i)))))

(def scores-to-probabilities
  (gen [scores]
    (let [max-score (apply max scores)
           numerically-stable-scores (map #(- % max-score) scores)
           weights (map exp numerically-stable-scores)
           log-normalizer (+ (log (apply + weights)) max-score)]
       (map #(exp (- % log-normalizer)) scores))))

(def log-categorical
  (make-inference-procedure-from-sampler-and-scorer
    (gen [scores]
      (let [threshold (uniform 0 1)
            scan (gen scan [i probs running-prob]
                   (let [p (+ (first probs) running-prob)]
                     (if (> p threshold)
                       i
                       (scan (+ i 1) (rest probs) p))))] ;; TODO: test recur
        (scan 0 (scores-to-probabilities scores) 0.0)))
    (gen [i [scores]]
      (log (nth (scores-to-probabilities scores) i)))))



(def generate-gaussian
  (gen [mu sigma]
    (let [u1 (uniform 0 1)
          u2 (uniform 0 1)]
      (+ mu (* (* (sqrt (* (- 0 2) (log u1)))    ;CHECK THIS
                              (cos (* (* 2 3.14159265) u2)))
                           sigma)))))

(def standard-gaussian-log-density
  (gen [x]
    (- (* (- 0 0.5) (log (* 2 3.14159265)))
       (* (* 0.5 x) x))))

(def score-gaussian
  (gen [x [mu sigma]]
    (- (standard-gaussian-log-density
         (/ (- x mu) sigma))
       (log sigma))))

(def gaussian
  (make-inference-procedure-from-sampler-and-scorer
    generate-gaussian
    score-gaussian))

(def gamma
  (make-inference-procedure-from-sampler-and-scorer
    (gen [shape scale]
      (distributions/draw
        (distributions/gamma-distribution shape scale)))
    (gen [x [shape scale]]
      (log (distributions/pdf
             (distributions/gamma-distribution shape scale)
             x)))))

(def beta
  (make-inference-procedure-from-sampler-and-scorer
    (gen [alpha beta]
      (distributions/draw
        (distributions/beta-distribution alpha beta)))
    (gen [x [alpha beta]]
      (log (distributions/pdf
             (distributions/beta-distribution alpha beta)
             x)))))


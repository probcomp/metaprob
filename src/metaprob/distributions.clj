(ns metaprob.distributions
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all])
  (:require [metaprob.prelude :refer :all])
  (:require [metaprob.context :refer :all])
  (:require [metaprob.interpreters :refer :all]))

;; -----------------------------------------------------------------------------
;; Distributions (nondeterministic procedures)

(define exactly
  (inf
    "exactly"
    ; No model
    nil
    ; Implementation
    (gen [[x] ctx]
      (with-explicit-tracer t
        (if (not (active-ctx? ctx))
          [x {} 0]
          (block
            (define result
              (if (constrained? ctx '())
                (constrained-value ctx '())
                (t "exactly-sample" exactly x)))

            [result {:value result}
             (if (and (targeted? ctx '()) (not= (target-value ctx '()) x)) negative-infinity 0)]))))))

(define make-inference-procedure-from-sampler-and-scorer
  (gen [name sampler scorer]
    (inf name
         (gen [& args]
           (with-explicit-tracer t
             (t '() exactly (apply sampler args))))
         (gen [args ctx]
           (with-explicit-tracer t
             (if (not (active-ctx? ctx))
               (block
                (define result (apply sampler args))
                [result {} 0])
               (block
                (define result
                  (if (constrained? ctx '())
                    (constrained-value ctx '())
                    (t name apply (make-inference-procedure-from-sampler-and-scorer name sampler scorer) args)))
                [result {:value result}
                 (if (targeted? ctx '())
                   (scorer result args)
                   0)])))))))

;; Uniform

(define uniform
  (make-inference-procedure-from-sampler-and-scorer
   "uniform"
   (gen [a b] (sample-uniform a b))
   (gen [x [a b]]
     (- 0.0 (log (- b a))))))

;; Categorical

(define uniform-sample
  (make-inference-procedure-from-sampler-and-scorer
   "uniform-sample"
   (gen [items]
     ;; items is a metaprob list (or tuple??)
     (define n (uniform 0 (count items)))
     (nth items (floor n)))
   (gen [item [items]]
     (- (log (count (clojure.core/filter (gen [x] (= x item)) items)))
        (log (count items))))))

;; Code translated from class BernoulliOutputPSP(DiscretePSP):
;;
;; The larger the weight, the more likely it is that the sample is
;; true rather than false.
(define flip
  (make-inference-procedure-from-sampler-and-scorer
    "flip"
    (gen [weight] (< (uniform 0 1) weight))
    (gen [value [weight]]
      (if value
        (log weight)
        (log1p (- 0 weight))))))

;; Cf. CategoricalOutputPSP from discrete.py in Venturecxx
;; This is just the one-argument form, so is simpler than what's in Venture.

(define categorical
  (make-inference-procedure-from-sampler-and-scorer
   "categorical"
   (gen [probabilities]
     ;; Returns an index i.
     ;; Assume that probabilities add to 1.
     ;; return simulateCategorical(vals[0], args.np_prng(),
     ;;   [VentureInteger(i) for i in range(len(vals[0]))])
     (define threshold (uniform 0 1))
     ;; iterate over probabilities, accumulate running sum, stop when cumu prob > threshold.
     (define scan (gen [i probs running-prob]
                    (if (empty? probs)
                      (- i 1)
                      (block (define next-prob (+ (first probs) running-prob))
                             (if (> next-prob threshold)
                               i
                               (scan (+ i 1) (rest probs) next-prob))))))
     (scan 0 probabilities 0.0))

   (gen [i [probabilities]]
     ;; return logDensityCategorical(val, vals[0],
     ;;   [VentureInteger(i) for i in range(len(vals[0]))])
     (log (nth probabilities i)))))


(define labeled-categorical
  (make-inference-procedure-from-sampler-and-scorer
    "labeled-categorical"
    (gen [labels probabilities]
      ;; Returns a label.
      ;; Assume that probabilities add to 1.
      ;; return simulateCategorical(vals[0], args.np_prng(),
      ;;   [VentureInteger(i) for i in range(len(vals[0]))])
      (define threshold (uniform 0 1))
      ;; iterate over probabilities, accumulate running sum, stop when cumu prob > threshold.
      (define scan (gen [i probs running-prob]
                     (if (empty? probs)
                       (nth labels (- i 1))
                       (block (define next-prob (+ (first probs) running-prob))
                              (if (> next-prob threshold)
                                (nth labels i)
                                (scan (+ i 1) (rest probs) next-prob))))))
      (scan 0 probabilities 0.0))
    (gen [l [labels probabilities]]
      ;; return logDensityCategorical(val, vals[0],
      ;;   [VentureInteger(i) for i in range(len(vals[0]))])
      (log (apply + (map (gen [i] (if (= l (nth labels i)) (nth probabilities i) 0)) (range (count probabilities))))))))

(declare scores-to-probabilities)

;; Returns 0, 1, 2, ... weighted by the given weights (given as log
;; probabilities, unnormalized).

(define log-categorical
  (make-inference-procedure-from-sampler-and-scorer
   "log-categorical"
   (gen [scores]
     (define threshold (uniform 0 1))
     ;; iterate over probabilities, accumulate running sum, stop when cumu prob > threshold.
     (define scan (gen [i probs running-prob]
                    (define p (+ (first probs) running-prob))
                    (if (> p threshold)
                      i
                      (scan (+ i 1) (rest probs) p))))
     (scan 0 (scores-to-probabilities scores) 0.0))
   (gen [i [scores]]
     (log (nth (scores-to-probabilities scores) i)))))




;;  ^:private
(define scores-to-probabilities
  (gen [scores]
       ;; master branch's
       (define max-score (apply clojure.core/max scores))
       (define numerically-stable-scores
          (map (gen [x] (- x max-score)) scores))
       (define weights (map exp numerically-stable-scores))
       (define log-normalizer (+ (log (apply + weights)) max-score))
       (if (> (apply + weights) 0)
         (map (gen [w] (exp (- w log-normalizer))) scores)
         (map (gen [w] (/ 1 (count scores))) scores))))

;; ----------------------------------------------------------------------------
;; I'm going to defer the implementation of beta until later;
;; it's difficult to access kixi's distribution code from metaprob.

;; Big Beta, an auxiliary used in the calculation of the PDF of a
;; beta distribution, can be calculated using Gamma.  Its log can
;; be calculated using Gamma's log, which kixi provides us.
;;
;; scipy's version is much more robust, and can be found here:
;; https://github.com/scipy/scipy/blob/master/scipy/special/cdflib/betaln.f

;  (:require [kixi.stats.math :as math])
;  (:require [kixi.stats.distribution :as dist])  ;for beta
; (defn log-gamma [x] (math/log-gamma x))

;(defn log-Beta [a b]
;  (- (+ (log-gamma a) (log-gamma b))
;     (log-gamma (+ a b))))

;; BetaOutputPSP(RandomPSP)
;; Please read the comments in lite/continuous.py in Venture

;(define beta
;  (make-inference-procedure-from-sampler-and-scorer
;   "beta"
;   (fn beta [a b]
;     ;; From kixi/stats/distribution.cljc :
;     ;; (let [[r1 r2] (split *rng*)
;     ;;         u (rand-gamma alpha r1)]
;     ;;   (/ u (+ u (rand-gamma beta r2))))
;     ;; rand-gamma is hairy. but defined in same file.
;     (dist/draw (dist/beta :alpha a :beta b)
;                {:seed (sample-long)}))
;   (fn [x params]
;     (let [[a b] (sequence-to-seq params)]
;       ;; Venture does:
;       ;; def logDensityNumeric(self, x, params):
;       ;;   return scipy.stats.beta.logpdf(x,*params)
;       ;; Wikipedia has a formula for pdf; easy to derive logpdf
;       ;; from it.
;       ;; scipy has a better version:
;       ;; https://github.com/scipy/scipy/blob/master/scipy/stats/_continuous_distns.py
;       (- (+ (* (log x) (- a 1.0))
;             (* (log (- 1.0 x)) (- b 1.0)))
;          (log-Beta a b))))))

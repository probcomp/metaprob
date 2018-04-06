(ns metaprob.distributions
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all])
  (:require [metaprob.prelude :refer :all])
  (:require [metaprob.infer :refer :all]))

;; -----------------------------------------------------------------------------
;; Distributions (nondeterministic procedures)

;; Code translated from class BernoulliOutputPSP(DiscretePSP):
;;
;; The larger the weight, the more likely it is that the sample is
;; true rather than false.

(define flip
  (inf "flip"
       (gen [inputs intervene target output]
         (define weight (nth inputs 0))
         (define [value score]
           (if (and intervene (trace-has? intervene))
             ;; Deterministic, so score is 0
             [(trace-get intervene) 0]
             (if (and target (trace-has? target))
               [(trace-get target)
                (if (trace-get target)
                  ;; E.g. if weight is 0.5, (log weight) is -0.69
                  (log weight)
                  (log1p (sub 0 weight)))]
               [(lt (sample-uniform) weight) 0])))
         (if output
           (trace-set output value))
         [value score])))

(define hard-to-name
  (gen [name sampler scorer]
    (inf name
         (gen [inputs intervene target output]
           (define [value score]
             (if (and intervene (trace-has? intervene))
               ;; Deterministic, so score is 0
               [(trace-get intervene) 0]
               (if (and target (trace-has? target))
                 [(trace-get target)
                  (scorer (trace-get target) inputs)]
                 [(apply sampler inputs) 0])))
           (if output
             (trace-set output value))
           [value score]))))

(define flip3
  (hard-to-name "flip"
                (gen [weight] (lt (sample-uniform) weight))
                (gen [value inputs]
                  (define weight (nth inputs 0))
                  (if value
                    (log weight)
                    (log1p (sub 0 weight))))))
                  

;; Uniform

(define uniform
  (hard-to-name
   "uniform"
   (gen [a b] (sample-uniform a b))
   (gen [x inputs] ;; [x [a b]]
     (define a (nth inputs 0))
     (define b (nth inputs 1))
     (sub 0.0 (log (sub b a))))))

;; Categorical

(define uniform-sample
  (hard-to-name
   "uniform-sample"
   (gen [items]
     ;; items is a metaprob list (or tuple??)
     (define n (sample-uniform 0 (length items)))
     (nth items (floor n)))
   (gen [item [items]]
     (sub (log (length (clojure.core/filter (gen [x] (eq x item)) items)))
        (log (length items))))))

;; 

(define log-categorical
  (hard-to-name
   "log-categorical"
   (gen [scores]
     ;; if scores is a tuple, coerce to list
     (define weights (make-immutable (map exp scores)))
     ;; reduce probably won't work
     (define normalizer (clojure.core/reduce add 0 weights))
     (define probabilities (map (gen [w] (div w normalizer)) weights))
     (define sample (sample-uniform 0 1))
     ;; iterate over probabilities, accumulate running sum, stop when cum prob > sample.
     (define scan (gen [i probs running]
                    (define running (add (first probs) running))
                    (if (gt running sample)
                      i
                      (scan (add i 1) (rest probs) running))))
     (scan 0 probabilities 0.0))
   (gen [i [scores]]
     (define weights (map exp scores))
     (define normalizer (clojure.core/reduce add 0 weights))
     (define probabilities (map (gen [w] (div w normalizer)) weights))
     (log (nth probabilities i)))))

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
;  (hard-to-name
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
;     (let [[a b] (metaprob-sequence-to-seq params)]
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


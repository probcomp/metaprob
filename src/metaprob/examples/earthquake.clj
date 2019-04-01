;; Selected nuggets from python-metaprob's src/inference.vnts file
;; See also figure 29 of the 7/17 chapter mss

(ns metaprob.examples.earthquake
  (:refer-clojure :exclude [map replicate apply])
  (:require [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.inference :refer :all]))

;; Convert a tuple of booleans to an integer.
;; Tuple element 0 determines the highest order bit.

(defn bools-to-binary [bools]
  (reduce (fn [n b] (+ (* 2 n) (if b 1 0))) 0 bools))
;
;(defn bools-to-binary [bools]
;  (let [len (count bools)]
;    (loop [i 0, n 0]
;      (if (>= i len)
;        n
;        (recur (inc i) (+ (* 2 n)
;                          (if (nth bools i) 1 0)))))))

(def earthquake-bayesian-network
  (gen []
    (let-traced [earthquake (flip 0.1)
                 burglary (flip 0.1)
                 alarm (cond (and burglary earthquake) (flip 0.9)
                             burglary (flip 0.85)
                             earthquake (flip 0.2)
                             true (flip 0.05))
                 john-call (flip (if alarm 0.8 0.1))
                 mary-call (flip (if alarm 0.9 0.4))]
      (bools-to-binary [earthquake burglary alarm john-call mary-call]))))

(defn trace-to-binary [tr]
  (bools-to-binary
    (map #(trace-value tr %) ["earthquake" "burglary" "alarm" "john-call" "mary-call"])))


(defn earthquake-histogram
   [name samples]
    (binned-histogram
      :name    name
      :samples samples
      :sample-lower-bound 0
      :sample-upper-bound 32
      :number-of-intervals 32
      :overlay-densities '()))

;; ----------------------------------------------------------------------------
;; Calculate exact probabilities


;; Returns a list of output traces

(defn joint-enumerate
  [addresses]
  (if (empty? addresses)
    '({})
    (let [others (joint-enumerate (rest addresses))
          addr (first addresses)
          trace-lists
          (map (fn [value] (map (fn [t] (trace-set-value t addr value)) others)) [true false])]

         (apply concat trace-lists))))

(defn intervene
  [f intervention]
  (gen [& args]
    (first (apply-at '() (make-constrained-generator f intervention) args))))

;; Returns list of [state score] where state is value returned by 
;;  earthquake-bayesian-network

(defn enumerate-executions
  [proc inputs intervention-trace target-trace]
  (let [[_ one-run _]
        (infer-and-score :procedure (intervene proc intervention-trace)
                         :inputs inputs)

        all-addrs (addresses-of one-run)

        candidates
        (joint-enumerate all-addrs)]

       (map (fn [candidate]
               (let [[state _ score]
                     (infer-and-score :procedure (intervene proc intervention-trace)
                                      :inputs inputs
                                      :observation-trace (trace-merge candidate target-trace))]
                 [state score]))
             candidates)))

;(define enumerate-executions
;  (gen [proc inputs intervention-trace target-trace]
;    (print [(count (addresses-of intervention-trace)) "interventions"])
;    (define [_ one-run _]
;      (infer :procedure proc
;             :inputs inputs
;             :intervention-trace intervention-trace))
;    (define all-sites (addresses-of one-run))
;    (print [(count all-sites) "sites"])
;    (define free-sites
;      (set-difference
;       (set-difference all-sites (addresses-of intervention-trace))
;       (addresses-of target-trace)))
;    (print [(count free-sites) "free-sites"])
;    (define candidates (joint-enumerate free-sites))
;    (map (gen [candidate]
;           ;; Returns [state nil score]
;           (define [state _ score]
;             (infer :procedure proc
;                    :inputs inputs
;                    :intervention-trace intervention-trace
;                    :target-trace (trace-merge candidate target-trace)
;                    :output-trace? false))
;           [state score])
;         candidates)))

;; Takes a list of [state score] and returns a list of samples.
;; A good multiplier is 12240.
;; The purpose is just so that we can easily reuse the histogram
;; plotting logic.

(defn fake-samples-for-enumerated-executions
  [state-and-score-list multiplier]
  (mapcat (fn [[state score]]
            (let [count (round (* (exp score) multiplier))]
              (repeat count state))) state-and-score-list))

;; ----------------------------------------------------------------------------
;; Sample from the prior

;; Each sample is an output trace.

(defn prior-samples
  [n-samples]
  (replicate n-samples #((infer-and-score :procedure earthquake-bayesian-network) 1)))

;; Test intervention
(def alarm-went-off {"alarm" {:value true}})

;; TODO: Use predicate version of rejection sampling?
(defn eq-rejection-assay
  [number-of-runs]
  (replicate
    number-of-runs
    (fn []
      (print "rejection sample")
      (rejection-sampling
        :model earthquake-bayesian-network
        :observation-trace alarm-went-off
        :log-bound 0))))

(defn eq-importance-assay
  [n-particles number-of-runs]
  (replicate
    number-of-runs
    (fn []
      (importance-resampling
        :model earthquake-bayesian-network
        :obesrvation-trace alarm-went-off
        n-particles))))


;; TBD: importance sampling
;; TBD: rejection sampling

(defn demo-earthquake
   []
  (clojure.pprint/pprint "Exact prior probabilities")
  (let [exact-probabilities
        (enumerate-executions earthquake-bayesian-network [] {} {})

        fake-samples
        (fake-samples-for-enumerated-executions exact-probabilities 12240)

        exact-awo-probabilities
        (enumerate-executions earthquake-bayesian-network [] alarm-went-off {})

        fake-awo-samples
        (fake-samples-for-enumerated-executions exact-awo-probabilities 12240)

        n-samples 100]

    (earthquake-histogram "exact bayesnet prior probabilities"
                          fake-samples)

    (earthquake-histogram "exact bayesnet alarm-went-off probabilities"
                          fake-awo-samples)

    (earthquake-histogram "bayesnet sampled prior probabilities"
                          (prior-samples n-samples))

    (earthquake-histogram "bayesnet samples from rejection sampling"
                          (map trace-to-binary (eq-rejection-assay n-samples)))

    (earthquake-histogram "bayesnet samples from importance sampling with 20 particles"
                          (map trace-to-binary (eq-importance-assay 20 n-samples)))))

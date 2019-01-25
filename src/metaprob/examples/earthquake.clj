;; Selected nuggets from python-metaprob's src/inference.vnts file
;; See also figure 29 of the 7/17 chapter mss

(ns metaprob.examples.earthquake
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all :exclude [flip]]
            [metaprob.inference :refer :all]
            [metaprob.interpreters :refer :all]))

(define flip
  (assoc metaprob.distributions/flip :support '(true false)))


(define quake-env (make-top-level-env 'metaprob.examples.earthquake))

;; Convert a tuple of booleans to an integer.
;; Tuple element 0 determines the highest order bit.

(define booleans-to-binary
  (opaque        ;Do not score
   (gen [qu]
     (define len (count qu))
     (define luup
       (gen [i n]
         (if (>= i len)
           n
           (luup (+ i 1) (+ (* 2 n)
                            (if (nth qu i) 1 0))))))
     (luup 0 0))))

(define earthquake-bayesian-network
  (gen []
    (define earthquake (flip 0.1))
    (define burglary (flip 0.1))
    (define p_alarm
      (if (and burglary earthquake)
        0.9
        (if burglary 0.85 (if earthquake 0.2 0.05))))
    (define alarm (flip p_alarm))
    (define p_john_call (if alarm 0.8 0.1))
    (define john_call (flip p_john_call))
    (define p_mary_call (if alarm 0.9 0.4))
    (define mary_call (flip p_mary_call))
    (booleans-to-binary
        [earthquake burglary alarm john_call mary_call])))

(define earthquake-histogram
  (gen [name samples]
    (binned-histogram
      :name    name
      :samples samples
      :sample-lower-bound 0
      :sample-upper-bound 32
      :number-of-intervals 32
      :overlay-densities '())))

;; ----------------------------------------------------------------------------
;; Calculate exact probabilities


;; Returns a list of output traces

(define joint-enumerate
  (gen [sites]
    (if (not (empty? sites))
      (block
        (define others (joint-enumerate (rest sites)))
        (define site (first sites))
        (print ["site:" site])
        (if (compound? site)
          (block (define oper-name (clojure.core/last site))
                 (define oper (top-level-lookup quake-env (clojure.core/symbol oper-name)))
                 (if (and (compound? oper)
                          (contains? oper :support))
                   (block
                    (define value-candidates
                      (get oper :support))
                    (define trace-lists
                      (map (gen [value]
                             (map (gen [t]
                                    (trace-set-value t site value))
                                  others))
                           value-candidates))
                    (concat trace-lists))
                   others))
          others))
      ; No sites to enumerate: only the empty trace is possible
      '({}))))

;; Returns list of [state score] where state is value returned by 
;;  earthquake-bayesian-network

(define enumerate-executions
  (gen [proc inputs intervention-trace target-trace]
    (print [(count (addresses-of intervention-trace)) "interventions"])
    (define [_ one-run _]
      (infer :procedure proc
             :inputs inputs
             :intervention-trace intervention-trace))
    (define all-sites (addresses-of one-run))
    (print [(count all-sites) "sites"])
    (define free-sites
      (set-difference
       (set-difference all-sites (addresses-of intervention-trace))
       (addresses-of target-trace)))
    (print [(count free-sites) "free-sites"])
    (define candidates (joint-enumerate free-sites))
    (map (gen [candidate]
           ;; Returns [state nil score]
           (define [state _ score]
             (infer :procedure proc
                    :inputs inputs
                    :intervention-trace intervention-trace
                    :target-trace (trace-merge candidate target-trace)
                    :output-trace? false))
           [state score])
         candidates)))

;; Takes a list of [state score] and returns a list of samples.
;; A good multiplier is 12240.
;; The purpose is just so that we can easily reuse the histogram
;; plotting logic.

(define fake-samples-for-enumerated-executions
  (gen [state-and-score-list multiplier]
    (concat (map (gen [[state score]]
                   ;; state will be a number from 0 to 31
                   (define count (round (* (exp score) multiplier)))
                   (print [state score (exp score) count])
                   (map (gen [_] state)
                        (range count)))
                 state-and-score-list))))

;; ----------------------------------------------------------------------------
;; Sample from the prior

;; Each sample is an output trace.

(define prior-samples
  (gen [n-samples]
    (replicate n-samples
               (gen []
                 ((infer :procedure earthquake-bayesian-network) 1)))))

;; Test intervention

(define alarm-address '(3 "alarm" "flip"))
(define alarm-went-off (trace-set-value {} alarm-address true))

(define check-alarm-intervention
  (gen []
    (define [_ output _]
      (infer :procedure earthquake-bayesian-network))
    (assert (trace-has-value? output alarm-address)
            "check validity of alarm intervention")))

(define eq-rejection-assay
  (gen [number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (print "rejection sample") ;Progress meter
       (trace-value ; TODO: Why trace-value?
        (rejection-sampling earthquake-bayesian-network
                            []        ; inputs 
                            alarm-went-off ;intervention
                            0)))))) ; log-bound 

(define eq-importance-assay
  (gen [n-particles number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (trace-value (importance-resampling ; TODO: Why trace value?
                    earthquake-bayesian-network
                    []  ; inputs
                    alarm-went-off
                    n-particles))))))


;; TBD: importance sampling
;; TBD: rejection sampling

(define demo-earthquake
  (gen []

    (print "Exact prior probabilities")
    (define exact-probabilities 
      (enumerate-executions earthquake-bayesian-network [] {} {}))
    (define fake-samples
      (fake-samples-for-enumerated-executions exact-probabilities 12240))
    (earthquake-histogram "exact bayesnet prior probabilities"
                          fake-samples)

    (print "Exact alarm-went-off probabilities")
    (define exact-awo-probabilities 
      (enumerate-executions earthquake-bayesian-network [] alarm-went-off {}))
    (define fake-awo-samples
      (fake-samples-for-enumerated-executions exact-awo-probabilities 12240))
    (earthquake-histogram "exact bayesnet alarm-went-off probabilities"
                          fake-awo-samples)

    (define number-of-samples 100)

    (print "bayesnet sampling from the prior")
    (earthquake-histogram "bayesnet sampled prior probabilities"
                          (prior-samples number-of-samples))

    (print "bayesnet rejection sampling")
    (earthquake-histogram "bayesnet samples from rejection sampling"
                          (eq-rejection-assay number-of-samples))

    (print "bayesnet importance sampling")
    (earthquake-histogram "bayesnet samples from importance sampling with 20 particles"
                          (eq-importance-assay 20 number-of-samples))))

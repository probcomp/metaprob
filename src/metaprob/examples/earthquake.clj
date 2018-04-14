;; Selected nuggets from python-metaprob's src/inference.vnts file
;; See also figure 29 of the 7/17 chapter mss

(ns metaprob.examples.earthquake
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.infer :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.inference :refer :all]))

;; Convert a tuple of booleans to an integer.
;; Tuple element 0 determines the highest order bit.

(define booleans-to-binary
  (opaque "booleans-to-binary"          ;Do not score
   (gen [qu]
     (define len (length qu))
     (define luup
       (gen [i n]
         (if (gte i len)
           n
           (luup (add i 1) (add (mul 2 n)
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
      :overlay-densities (list))))

;; ----------------------------------------------------------------------------
;; Calculate exact probabilities

;; Kludge

(define top-level-env (trace-get (gen [x] x) "environment"))

(trace-set flip "support" (list true false))

;; Returns a list of output traces

(define joint-enumerate
  (gen [sites]
    (if (pair? sites)
      (block
        (define others (joint-enumerate (rest sites)))
        (define site (first sites))
        (print ["site:" site])
        (if (pair? site)
          (block (define oper-name (last site))
                 (define oper (top-level-lookup top-level-env oper-name))
                 (if (and (trace? oper)
                          (trace-has? oper "support"))
                   (block
                    (define value-candidates
                      (trace-get oper "support"))
                    (define trace-lists
                      (map (gen [value]
                             (map (gen [t]
                                    (define t1 (trace-copy t))
                                    (trace-set t1 site value)
                                    t1)
                                  others))
                           value-candidates))
                    (concat trace-lists))
                   others))
          others))
      (block (pair (empty-trace) (empty-trace))))))

;; Returns list of [state score] where state is value returned by 
;;  earthquake-bayesian-network

(define enumerate-executions
  (gen [proc inputs intervention-trace target-trace]
    (print [(length (addresses-of intervention-trace)) "interventions"])
    (define one-run (empty-trace))
    (infer-apply proc                   ;was trace-choices
                 inputs
                 intervention-trace
                 nil
                 one-run)
    (define all-sites (addresses-of one-run))
    (print [(length all-sites) "sites"])
    (define free-sites
      (set-difference
       (set-difference all-sites (addresses-of intervention-trace))
       (addresses-of target-trace)))
    (print [(length free-sites) "free-sites"])
    (define candidates (joint-enumerate free-sites))
    (map (gen [candidate]
           (trace-update candidate target-trace)
           ;; Returns [state score]
           (infer-apply proc
                        inputs
                        intervention-trace
                        candidate
                        nil))
         candidates)))

;; Takes a list of [state score] and returns a list of samples.
;; A good multiplier is 12240.
;; The purpose is just so that we can easily reuse the histogram
;; plotting logic.

(define fake-samples-for-enumerated-executions
  (gen [state-and-score-list multiplier]
    (concat (map (gen [[state score]]
                   ;; state will be a number from 0 to 31
                   (define count (round (mul (exp score) multiplier)))
                   (print [state score (exp score) count])
                   (map (gen [ignore] state)
                        (range count)))
                 state-and-score-list))))

;; ----------------------------------------------------------------------------
;; Sample from the prior

;; Each sample is an output trace.

(define prior-samples
  (gen [n-samples]
    (replicate n-samples
               (gen []
                 (define output (empty-trace))
                 ;; Was trace_choices
                 (infer-apply earthquake-bayesian-network
                              []
                              nil       ;No intervention
                              nil       ;No target
                              output)
                 output))))

;; Test intervention

(define alarm-went-off (empty-trace))
(define alarm-address (addr 3 "alarm" "flip"))
(trace-set alarm-went-off alarm-address true)

(define check-alarm-intervention
  (gen []
    (define output (empty-trace))
    (infer-apply earthquake-bayesian-network
                 []
                 (empty-trace)
                 nil                ;No target
                 output)
    (assert (trace-has? output alarm-address)
            "check validity of alarm intervention")))

(define eq-rejection-assay
  (gen [number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (print "rejection sample") ;Progress meter
       (trace-get
        (rejection-sampling earthquake-bayesian-network
                            []        ; inputs 
                            alarm-went-off ;intervention
                            0)))))) ; log-bound 

(define eq-importance-assay
  (gen [n-particles number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (trace-get (importance-resampling
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
      (enumerate-executions earthquake-bayesian-network [] (empty-trace) (empty-trace)))
    (define fake-samples
      (fake-samples-for-enumerated-executions exact-probabilities 12240))
    (earthquake-histogram "exact earthquake prior probabilities"
                          fake-samples)

    (print "Exact alarm-went-off probabilities")
    (define exact-awo-probabilities 
      (enumerate-executions earthquake-bayesian-network [] alarm-went-off (empty-trace)))
    (define fake-awo-samples
      (fake-samples-for-enumerated-executions exact-awo-probabilities 12240))
    (earthquake-histogram "exact earthquake alarm-went-off probabilities"
                          fake-awo-samples)

    (define number-of-samples 100)

    (print "Sampling from the prior")
    (earthquake-histogram "sampled earthquake prior probabilities"
                          (prior-samples number-of-samples))

    (print "Rejection sampling")
    (earthquake-histogram "samples from the target"
                          (eq-rejection-assay number-of-samples))

    (print "Importance sampling")
    (earthquake-histogram "samples from importance sampling with 20 particles"
                          (eq-importance-assay 20 number-of-samples))))

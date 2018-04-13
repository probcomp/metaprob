;; Selected nuggets from python-metaprob's src/inference.vnts file
;; See also figure 29 of the 7/17 chapter mss

(ns metaprob.examples.earthquake
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.infer :refer :all]
            [metaprob.distributions :refer :all]))

;; Convert a tuple of booleans to an integer.
;; Tuple element 0 determines the highest order bit.

(define booleans-to-binary
  (opaque                               ;Do not score
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
  (gen [samples name]
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
    (concat (map (gen [[sample score]]
                   ;; sample will be a number from 0 to 31
                   (define count (round (mul (exp score) multiplier)))
                   (print [sample score (exp score) count])
                   (map (gen [ignore] sample)
                        (range count)))
                 state-and-score-list))))

;; ----------------------------------------------------------------------------
;; Sample from the prior

;; Each sample is an output trace.

(define prior-samples
  (gen [n-samples]
    (define
      prior-trace
      (gen []
        (define output (empty-trace))
        ;; Was trace_choices
        (infer-apply earthquake-bayesian-network
                     []
                     (empty-trace)
                     nil                ;No target
                     output)
        output))
    (replicate n-samples prior-trace)))

;; Test intervention

(define alarm_went_off (empty-trace))
(trace-set alarm_went_off (addr 3 "alarm" "flip") true)

(define modified-prior-samples
  (gen [num_replicates]
    (replicate num_replicates
               (gen []
                 (define output (empty-trace))
                 (infer-apply earthquake-bayesian-network
                              []
                              alarm_went_off
                              nil
                              output)
                 output))))

;; propose = no output trace

(if false
     (infer-apply
      earthquake-bayesian-network
      (tuple)
      alarm_went_off
      nil nil))

;; TBD: importance sampling
;; TBD: rejection sampling

(define demo-earthquake
  (gen []
    (define exact-probabilities 
      (enumerate-executions earthquake-bayesian-network [] (empty-trace) (empty-trace)))
    (define fake-samples
      (fake-samples-for-enumerated-executions exact-probabilities 12240))
    (earthquake-histogram fake-samples
                          "exact earthquake prior probabilities")
    (print "Sampling from the prior")
    (earthquake-histogram (prior-samples 100)
                          "sampled earthquake prior probabilities")
    (print "Sampling from the modified prior")
    (earthquake-histogram (modified-prior-samples 100)
                          "sampled earthquake prior probabilities")))


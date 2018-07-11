;; 4.

(ns metaprob.inference
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.interpreters :refer :all]))

;; Probabilistic inference methods

;; ----------------------------------------------------------------------------

(define rejection-sampling
  (gen [model-procedure inputs target-trace log-bound]
    (define [_ candidate-trace score]
      (infer :procedure model-procedure
             :inputs inputs
             :intervention-trace (empty-trace)
             :target-trace       target-trace
             :output-trace?      true))
    (if (lt (log (uniform 0 1)) (sub score log-bound))
      candidate-trace
      (rejection-sampling
	model-procedure inputs target-trace log-bound)) ))

;; ----------------------------------------------------------------------------

(define importance-resampling
  (gen [model-procedure inputs target-trace N]

    ;; generate N candidate traces, called particles, each
    ;; with a score
    
    (define particles
    	    (replicate N
	      (gen []
                (define candidate-trace (mutable-trace))
                (define [_ _ score]
                  (infer :procedure model-procedure
                         :inputs inputs
                         :intervention-trace nil
                         :target-trace target-trace
                         :output-trace candidate-trace))
                [candidate-trace score])))
    (define scores
      (map (gen [p] (nth p 1)) particles))
    ;; return a trace with probability proportional to (exp score)
    (define which (log-categorical scores))

    (define particle (nth particles which)) ;; [candidate-trace score]
    (nth particle 0)))

;; ----------------------------------------------------------------------------
;; Metropolis-Hastings

;; trace is both an input and (by side effect) an output.

(define single-site-metropolis-hastings-step
  (gen [model-procedure inputs trace constraint-addresses]

    ;; choose an address to modify, uniformly at random
    
    (define choice-addresses (addresses-of trace))
    (define candidates (set-difference choice-addresses constraint-addresses))
    (define target-address (uniform-sample candidates))

    ;; generate a proposal trace

    (define initial-value (trace-get trace target-address))
    (define initial-num-choices (length candidates))
    (trace-delete! trace target-address)

    (define new-trace (mutable-trace))
    (define [_ new-trace forward-score]
      (infer :procedure model-procedure
             :inputs inputs
             :intervention-trace nil
             :target-trace trace
             :output-trace new-trace))
    (define new-value (trace-get new-trace target-address))

    ;; the proposal is to move from trace to new-trace
    ;; now calculate the Metropolis-Hastings acceptance ratio

    (define new-choice-addresses (addresses-of new-trace))
    (define new-candidates (set-difference new-choice-addresses constraint-addresses))
    (define new-num-choices (length new-candidates))

    ;; make a trace that can be used to restore the original trace
    
    (define restoring-trace (empty-trace))
    (trace-set! restoring-trace target-address initial-value)
    (for-each (set-difference choice-addresses new-choice-addresses)
    	      (gen [initial-addr] ;; initial-addr in original but not proposed trace
                (trace-set! restoring-trace
                           initial-addr
                           (trace-get trace initial-addr))))

    ;; remove the new value
    (trace-delete! new-trace target-address)

    (define [_ _ reverse-score]
      (infer :procedure model-procedure
             :inputs   inputs
             :intervention-trace restoring-trace
             :target-trace new-trace
             :output-trace? false))
    
    (trace-set! new-trace target-address new-value)
    (define log-acceptance-probability (sub (add forward-score (log new-num-choices))
    	    			       	    (add reverse-score (log initial-num-choices))))
    (if (lt (log (uniform 0 1)) log-acceptance-probability)
        (block
	    (for-each (set-difference choice-addresses new-choice-addresses)
	              (gen [initial-addr] (trace-delete! trace initial-addr)))
	    (for-each new-choice-addresses
	    	      (gen [new-addr]
                        (trace-set! trace
                                   new-addr
                                   (trace-get new-trace new-addr)))))
	(trace-set! trace target-address initial-value))))

;; Should return [output-trace value] ...

(define lightweight-single-site-MH-sampling
  (gen [model-procedure inputs target-trace N]
    (define state (empty-trace))
    (define [_ _ _]
      (infer :procedure model-procedure
             :inputs inputs
             :intervention-trace (empty-trace)
             :target-trace target-trace
             :output-trace state))
    (repeat N
            (gen []
              ;; VKM had keywords :procedure :inputs :trace :constraint-addresses
              (single-site-metropolis-hastings-step
               model-procedure
               (tuple)
               (to-mutable state)
               (addresses-of target-trace))))
    state))

;; -----------------------------------------------------------------------------
;; Utilities for checking that inference is giving acceptable sample sets.
;; These are used in the test suites.

(declare sillyplot)

(define check-bins-against-pdf
  (gen [bins pdf]
    (define nsamples (* (apply + (map length bins)) 1.0))
    (define abs (gen [x] (if (< x 0) (- 0 x) x)))
    (define bin-p (map (gen [bin]
                         ;; Probability that a sample is in this bin, as inferred from sample set
                         (/ (length bin) nsamples))
                       bins))
    (define bin-q (map (gen [bin]
                         ;; Estimated probability that a sample would be
                         ;; in the bin if the sampler were operating
                         ;; correctly.  Could use any pdf in the bin as
                         ;; a density estimate; we use the average.
                         (define bincount (length bin))
                         (* (/ (apply + (map pdf bin)) bincount)
                            ;; Estimate of bin width...
                            (* (- (nth bin (- bincount 1))
                                  (nth bin 0))
                               ;; adustment for fencepost...
                               (/ (+ bincount 1)
                                  (* bincount 1.0)))))
                       bins))
    (define discrepancies (clojure.core/map (gen [p q] (abs (- p q))) bin-p bin-q))
    ;; Trim off first and last bins, since their pdf estimate is 
    ;; likely to be way off
    (define trimmed (rest (reverse (rest discrepancies))))
    (define normalization (/ (length discrepancies) (* (length trimmed) 1.0)))
    [(* normalization (apply + trimmed))
     bin-p
     bin-q]))

(define check-samples-against-pdf
  (gen [samples pdf nbins]
    (define samples (to-tuple (sort samples)))    ; = clojure (vec ...)
    (define nsamples (* (length samples) 1.0))
    (define binsize (/ nsamples nbins))      ;float
    (define bins (map (gen [i]
                        ;; Try to put same number of samples
                        ;; in each bin.  This is not necessary.
                        (define start (clojure.core/int (* i binsize)))
                        (define end (clojure.core/int (* (+ i 1) binsize)))
                        (clojure.core/subvec samples start end))
                      (range nbins)))
    (check-bins-against-pdf bins pdf)))

(define assay
  (gen [tag sampler nsamples pdf nbins threshold]
    (define [badness bin-p bin-q]
      (check-samples-against-pdf (map sampler (range nsamples))
                                 pdf
                                 nbins))
    ;; Diagnostic output.
    (if (or (> badness threshold)
            (< badness (/ threshold 2)))
      (block (clojure.core/print
              (clojure.core/format "%s. n: %s bins: %s badness: %s threshold: %s\n"
                                   tag nsamples nbins badness threshold))
             (sillyplot bin-p)
             (sillyplot bin-q)))
    (< badness (* threshold 1.5))))

(define badness
  (gen [sampler nsamples pdf nbins]
    (define [badness bin-p bin-q]
      (check-samples-against-pdf (map sampler (range nsamples))
                                 pdf
                                 nbins))
    badness))


(define sillyplot
  (gen [l]
    (define nbins (length l))
    (define trimmed (if (> nbins 50)
                      ;; Take middle so that it fits on one line
                      (clojure.core/take
                       (clojure.core/drop l (/ (- nbins 50) 2))
                       50)
                      l))
    (clojure.core/print
     (clojure.core/format "%s\n"
                          (to-tuple (map (gen [p] (Math/round (* p 100)))
                                         trimmed))))))

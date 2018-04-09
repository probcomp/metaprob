(ns metaprob.oopsla.metropolis-hastings-step
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.oopsla.interpreters :refer :all]))

(define single-site-metropolis-hastings-step
  (gen [model-procedure inputs trace constraint-addresses]

    ;; choose an address to modify, uniformly at random
    
    (define choice-addresses (addresses-of trace))
    (define candidates (set-difference choice-addresses constraint-addresses))
    (define target-address (uniform-sample candidates))

    ;; generate a proposal trace

    (define initial-value (trace-get trace target-address))
    (define initial-num-choices (length candidates))
    (trace-delete trace target-address)
    (define new-trace (empty-trace))

    (define (_ forward-score) (infer :procedure model-procedure
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
    (trace-set restoring-trace target-address initial-value)
    (for-each (set-difference choice-addresses new-choice-addresses)
    	      (gen [initial-addr] ;; initial-addr in original but not proposed trace
                (trace-set restoring-trace
                           initial-addr
                           (trace-get trace initial-addr))))

    ;; remove the new value
    (trace-delete new-trace target-address)

    (define (_ reverse-score)
      (infer :procedure model-procedure
             :inputs   inputs
             :intervention-trace restoring-trace
             :target-trace new-trace
             :output-trace (empty-trace)))
    
    (trace-set new-trace target-address new-value)
    (define log-acceptance-probability (sub (add forward-score (log new-num-choices))
    	    			       	    (add reverse-score (log initial-num-choices))))
    (if (lt (log (uniform 0 1)) log-acceptance-probability)
        (block
	    (for-each (set-difference choice-addresses new-choice-addresses)
	              (gen [initial-addr] (trace-delete trace initial-addr)))
	    (for-each new-choice-addresses
	    	      (gen [new-addr]
                        (trace-set trace
                                   new-addr
                                   (trace-get new-trace new-addr)))))
	(trace-set trace target-address initial-value))))

;; Should return a single trace.  Which one?

(define lightweight-single-site-MH-sampling
  (gen [N model-procedure target-trace]
    (define state (empty-trace))
    (infer :procedure model-procedure
           :inputs (tuple)
           :intervention-trace (empty-trace)
           :target-trace target-trace
           :output-trace state)
    (repeat N
            (gen []
              ;; VKM had keywords :procedure :inputs :trace :constraint-addresses
              (single-site-metropolis-hastings-step
               model-procedure
               (tuple)
               state
               (addresses-of target-trace))))
    state))

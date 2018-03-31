(ns metaprob.mapl2018.metropolis-hastings-step
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.mapl2018.interpreters :refer :all]))

(define single-site-metropolis-hastings-step
  (gen
    [model-procedure inputs trace constraint-addresses]

    ;; choose an address to modify, uniformly at random
    
    (define choice-addresses (addresses_of trace))
    (define candidates (set-difference choice-addresses constraint-addresses))
    (define target-address (uniform-sample candidates))

    ;; generate a proposal trace

    (define initial-value (trace-get (lookup trace target-address)))
    (define initial-num-choices (length candidates))
    (trace-delete trace target-address)
    (define new-trace (empty-trace))

    (define (_ forward-score) (infer :procedure model-procedure
                                     :inputs inputs
                                     :intervention-trace (empty-trace)
                                     :target-trace trace
                                     :output-trace new-trace))
    (define new-value (trace-get new-trace target-address))

    ;; the proposal is to move from trace to new-trace
    ;; now calculate the Metropolis-Hastings acceptance ratio

    (define new-choice-addresses (addresses_of new-trace))
    (define new-candidates (set-difference new-choice-addresses constraint-addresses))
    (define new-num-choices (length new-candidates))

    ;; make a trace that can be used to restore the original trace
    
    (define restoring-trace (empty-trace))
    (trace_set (lookup restoring-trace target-address)
    	       initial-value)
    (for_each (set-difference choice-addresses new-choice-addresses)
    	      (gen [initial-addr] ;; initial-addr in original but not proposed trace
	      		(trace_set (lookup restoring-trace initial-addr)
				   (trace-get (lookup trace initial-addr)))))

    ;; remove the new value
    (trace-delete new-trace target-address)

    (define (__ reverse-score)
      (infer :procedure model-procedure
             :inputs   inputs
             :intervention-trace restoring-trace
             :target-trace new-trace
             :output-trace (empty-trace)))
    
    (trace_set (lookup new-trace target-address) new-value)
    (define log-acceptance-probability (sub (add forward-score (log new-num-choices))
    	    			       	    (add reverse-score (log initial-num-choices))))
    (if (lt (log (uniform 0 1)) log-acceptance-probability)
        (block
	    (for_each (set-difference choice-addresses new-choice-addresses)
	              (gen [initial-addr] (trace-delete trace initial-addr)))
	    (for_each new-choice-addresses
	    	      (gen [new-addr]
		      		(trace_set (lookup trace new-addr)
					   (trace-get (lookup new-trace new-addr))))))
	(trace_set (lookup trace target-address) initial-value))))

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
            (gen
              []
              ;; VKM had keywords :procedure :inputs :trace :constraint-addresses
              (single-site-metropolis-hastings-step
               model-procedure
               (tuple)
               state
               (addresses_of target-trace))))
    state))

(ns metaprob.mapl2018.metropolis-hastings-step
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.mapl2018.interpreters :refer :all]))

(define single-site-metropolis-hastings-step
  (probprog
    [model-probprog inputs trace constraint-addresses]

    ;; choose an address to modify, uniformly at random
    
    (define choice-addresses (addresses_of trace))
    (define candidates (set-difference choice-addresses constraint-addresses))
    (define target-address (uniform-sample candidates))

    ;; generate a proposal trace

    (define initial-value (trace-get (lookup trace target-address)))
    (define initial-num-choices (length candidates))
    (trace-delete trace target-address)
    (define new-trace (empty-trace))

    (print "Start pass 2 (forward-score trace)")
    (print new-trace)
    (define (_ forward-score) (query
                               :probprog model-probprog
                               :inputs inputs
                               :intervention-trace (empty-trace)
                               :target-trace trace
                               :output-trace new-trace))
    (print "End pass 2 (forward-score trace)")
    (pprint new-trace)
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
    	      (probprog [initial-addr] ;; initial-addr in original but not proposed trace
	      		(trace_set (lookup restoring-trace initial-addr)
				   (trace-get (lookup trace initial-addr)))))

    ;; remove the new value
    (trace-delete new-trace target-address)

    (define (__ reverse-score) (query
                                   :probprog model-probprog
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
	              (probprog [initial-addr] (trace-delete trace initial-addr)))
	    (for_each new-choice-addresses
	    	      (probprog [new-addr]
		      		(trace_set (lookup trace new-addr)
					   (trace-get (lookup new-trace new-addr))))))
	(trace_set (lookup trace target-address) initial-value)) ))

(define lightweight-single-site-MH-sampling
  (probprog [N model-probprog target-trace]
  	    (define state (empty-trace))
            (print "Start pass 1 (state)")
	    (query
	      :probprog model-probprog
	      :inputs (tuple)
	      :intervention-trace (empty-trace)
	      :target-trace target-trace
	      :output-trace state)
            (print "End pass 1 (state)")
            (pprint state)
            (repeat N
	            (probprog
		      []
                      ;; VKM had keywords :probprog :inputs :trace :constraint-addresses
		      (single-site-metropolis-hastings-step
		        model-probprog
			(tuple)
			state
			(addresses_of target-trace))))))

(ns metaprob.mem
  (:refer-clojure :only [declare ns])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all])
  (:require [metaprob.prelude :refer :all])
  (:require [metaprob.interpreters :refer :all]))

;; For now, the function to be memoized must take exactly one
;; argument, and that argument must be acceptable as a trace
;; key (number, string, boolean).
;;
;; In the future, traces might be changed to allow structured values
;; as keys, in which case these deficiencies can be corrected.

;; TBD: intervention and target
;; TBD: protect for parallelism

(define traces-stored-under-key "memoized-traces")

(define with-memoized
  (inf "with-memoized"
       (gen [fun proc] (proc fun))      ;???? not quite right
       (gen [[fun proc] intervene target output?]
         (define value-cache (mutable-trace))
         (define trace-cache (mutable-trace))
         (define total-score (mutable-trace :value 0))
         (define intervene-cache (maybe-subtrace intervene traces-stored-under-key))
         (define target-cache (maybe-subtrace target traces-stored-under-key))
         ;; Populate cache with values from intevention and target traces
         (define [valu out sco]
           (infer :procedure proc
                  :inputs
                  [(inf "memoized"
                        fun             ;????
                        (gen [[key] _ _ o?]   ;Ignore intervention & target traces - no randomness here
                          ;; We're faking a with-address here
                          [(if (trace-has? value-cache key)
                             (trace-get value-cache key)
                             (block (define [valu out sco]
                                      (infer :procedure fun
                                             :inputs [key]
                                             :intervention-trace (maybe-subtrace intervene-cache key)
                                             :target-trace (maybe-subtrace target-cache key)
                                             :output-trace? o?))
                                    (trace-set! value-cache key valu)
                                    (if (and out (not (empty-trace? out)))
                                      (trace-set-subtrace! trace-cache key out))
                                    (trace-set! total-score
                                                (+ (trace-get total-score) sco))
                                    valu))
                           (trace)
                           0]))]
                  :intervention-trace intervene
                  :target-trace target
                  :output-trace? output?))
         [valu
          (if output?
            (maybe-set-subtrace out
                                traces-stored-under-key    ; Possible collision here (unlikely)
                                (to-immutable trace-cache))
            nil)
          (+ sco (trace-get total-score))])))

;; 4.
(ns metaprob.inference
  (:refer-clojure :exclude [get contains? keys empty? dissoc assoc get-in
                            map reduce replicate apply])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.distributions :refer :all]
            [clojure.set :as set]
            [clojure.pprint :as pprint]))

;; Probabilistic inference methods

;; ----------------------------------------------------------------------------

(def rejection-sampling
  (gen [model-procedure inputs observations log-bound]
    (let
      [[_ candidate-trace score]
       (infer-and-score :procedure model-procedure,
                        :inputs inputs,
                        :observation-trace observations)]
    (if (< (log (uniform 0 1)) (- score log-bound))
      candidate-trace
      (recur model-procedure inputs observations log-bound)))))

;; ----------------------------------------------------------------------------

(def importance-resampling
  (gen [model-procedure inputs observations N]
    (let [particles
          (replicate N
             (gen [] (let [[_ t s]
                       (infer-and-score :procedure model-procedure,
                                        :inputs inputs,
                                        :observation-trace observations)]
                       [t s])))
          scores
          (map second particles)]
      (first (nth particles (log-categorical scores))))))
;
;;; ----------------------------------------------------------------------------
;;; Metropolis-Hastings

(def resimulation-mh-move
  (gen [model inputs tr addresses]
    (let [[current-choices fixed-choices]
          (partition-trace tr addresses)

          ;; Get the log probability of the current trace
          [_ _ old-p]
          (infer-and-score :procedure model,
                           :inputs inputs,
                           :observation-trace tr)

          ;; Propose a new trace, and get its score
          [_ proposed new-p-over-forward-q]
          (infer-and-score :procedure model,
                           :inputs inputs,
                           :observation-trace fixed-choices)

          ;; Figure out what the reverse problem would look like
          [_ reverse-move-starting-point]
          (partition-trace proposed addresses)

          ;; Compute a reverse score
          [_ _ reverse-q]
          (infer-and-score :procedure infer-and-score,
                           :inputs [:procedure model,
                                    :inputs inputs,
                                    :observation-trace reverse-move-starting-point]
                           :observation-trace current-choices)

          log-ratio
          (+ new-p-over-forward-q (- reverse-q old-p))]
      (if (flip (exp log-ratio))
        proposed
        tr))))



;; TODO: Give a way for proposer to specify
;; which addresses it is proposing to, and which it is
;; simply using for itself.
(def custom-mh-move
  (gen [model inputs tr proposer]
    (let
      [[_ proposed-move _]
       (infer-and-score :procedure proposer
                        :inputs [tr])

       [_ _ p-old]
       (infer-and-score :procedure model
                        :inputs inputs
                        :observation-trace tr)

       [_ _ p-new]
       (infer-and-score :procedure model
                        :inputs inputs
                        :observation-trace proposed-move)

       [_ _ q-forward]
       (infer-and-score :procedure proposer
                        :inputs [tr]
                        :observation-trace proposed-move)

       [_ _ q-reverse]
       (infer-and-score :procedure proposer
                        :inputs [proposed-move]
                        :observation-trace tr)

       log-ratio (- (+ p-new q-reverse) (+ p-old q-forward))]

      (if (flip (exp log-ratio))
        proposed-move
        tr))))



(def resimulation-proposal
  (gen [model inputs addresses]
    (gen {:tracing-with t} [tr]
      (let [[_ fixed-choices] (partition-trace tr addresses)]
        (t '() infer-and-score
           :procedure model,
           :inputs inputs,
           :observation-trace fixed-choices)))))

(def resimulation-move-alternative-impl
  (gen [model inputs tr addresses]
    (custom-mh-move
      model inputs tr
      (resimulation-proposal model inputs addresses))))


;(define single-site-metropolis-hastings-step
;  (gen [model-procedure inputs trace constraint-addresses]
;
;    ;; choose an address to modify, uniformly at random
;
;    (define choice-addresses (addresses-of trace))
;    (define candidates (set-difference choice-addresses constraint-addresses))
;    (define target-address (uniform-sample candidates))
;
;    ;; generate a proposal trace
;
;    (define initial-value (trace-value trace target-address))
;    (define initial-num-choices (count candidates))
;    (define new-target (trace-clear-value trace target-address))
;
;    (define [_ new-trace forward-score]
;      (comp/infer-apply model-procedure inputs (make-top-level-tracing-context {} new-target)))
;    (define new-value (trace-value new-trace target-address))
;
;    ;; the proposal is to move from trace to new-trace
;    ;; now calculate the Metropolis-Hastings acceptance ratio
;
;    (define new-choice-addresses (addresses-of new-trace))
;    (define new-candidates (set-difference new-choice-addresses constraint-addresses))
;    (define new-num-choices (count new-candidates))
;
;    ;; make a trace that can be used to restore the original trace
;    (define restoring-trace
;      (trace-set-value
;        (clojure.core/reduce
;          (gen [so-far next-adr] (trace-set-value so-far next-adr (trace-value trace next-adr)))
;          {}
;          (set-difference choice-addresses new-choice-addresses))
;        target-address initial-value))
;
;    ;; remove the new value
;    (define new-target-rev (trace-clear-value new-trace target-address))
;
;    (define [_ _ reverse-score]
;      (infer :procedure model-procedure
;             :inputs   inputs
;             :intervention-trace restoring-trace
;             :target-trace new-target-rev))
;
;    (define log-acceptance-probability
;      (- (+ forward-score (log new-num-choices))
;         (+ reverse-score (log initial-num-choices))))
;
;    (if (flip (exp log-acceptance-probability))
;      new-trace
;      trace)))
;
;;; Should return [output-trace value] ...
;
;(define lightweight-single-site-MH-sampling
;  (gen [model-procedure inputs target-trace N]
;    (clojure.core/reduce
;      (gen [state _]
;        ;; VKM had keywords :procedure :inputs :trace :constraint-addresses
;        (single-site-metropolis-hastings-step
;          model-procedure inputs state (addresses-of target-trace)))
;      (nth (infer :procedure model-procedure :inputs inputs :target-trace target-trace) 1)
;      (range N))))
;
;;; -----------------------------------------------------------------------------
;;; Utilities for checking that inference is giving acceptable sample sets.
;;; These are used in the test suites.
;
;(declare sillyplot)
;
;(define check-bins-against-pdf
;  (gen [bins pdf]
;    (define nsamples (* (apply + (map count bins)) 1.0))
;    (define abs (gen [x] (if (< x 0) (- 0 x) x)))
;    (define bin-p (map (gen [bin]
;                         ;; Probability that a sample is in this bin, as inferred from sample set
;                         (/ (count bin) nsamples))
;                       bins))
;    (define bin-q (map (gen [bin]
;                         ;; Estimated probability that a sample would be
;                         ;; in the bin if the sampler were operating
;                         ;; correctly.  Could use any pdf in the bin as
;                         ;; a density estimate; we use the average.
;                         (define bincount (count bin))
;                         (* (/ (apply + (map pdf bin)) bincount)
;                            ;; Estimate of bin width...
;                            (* (- (nth bin (- bincount 1))
;                                  (nth bin 0))
;                               ;; adustment for fencepost...
;                               (/ (+ bincount 1)
;                                  (* bincount 1.0)))))
;                       bins))
;    (define discrepancies (clojure.core/map (gen [p q] (abs (- p q))) bin-p bin-q))
;    ;; Trim off first and last bins, since their pdf estimate is
;    ;; likely to be way off
;    (define trimmed (rest (clojure.core/reverse (rest discrepancies))))
;    (define normalization (/ (count discrepancies) (* (count trimmed) 1.0)))
;    [(* normalization (apply + trimmed))
;     bin-p
;     bin-q]))
;
;(define check-samples-against-pdf
;  (gen [samples pdf nbins]
;    (define samples (clojure.core/vec (sort samples)))    ; = clojure (vec ...)
;    (define nsamples (* (count samples) 1.0))
;    (define binsize (/ nsamples nbins))      ;float
;    (define bins (map (gen [i]
;                        ;; Try to put same number of samples
;                        ;; in each bin.  This is not necessary.
;                        (define start (clojure.core/int (* i binsize)))
;                        (define end (clojure.core/int (* (+ i 1) binsize)))
;                        (clojure.core/subvec samples start end))
;                      (range nbins)))
;    (check-bins-against-pdf bins pdf)))
;
;(define assay
;  (gen [tag sampler nsamples pdf nbins threshold]
;    (report-on-elapsed-time
;     tag
;     (gen []
;       (define [badness bin-p bin-q]
;         (check-samples-against-pdf (map sampler (range nsamples))
;                                    pdf
;                                    nbins))
;       ;; Diagnostic output.
;       (if (or (> badness threshold)
;               (< badness (/ threshold 2)))
;         (block (clojure.core/print
;                 (clojure.core/format "%s. n: %s bins: %s badness: %s threshold: %s\n"
;                                      tag nsamples nbins badness threshold))
;                (sillyplot bin-p)
;                (sillyplot bin-q)))
;       (< badness (* threshold 1.5))))))
;
;(define badness
;  (gen [sampler nsamples pdf nbins]
;    (define [badness bin-p bin-q]
;      (check-samples-against-pdf (map sampler (range nsamples))
;                                 pdf
;                                 nbins))
;    badness))
;
;
;(define sillyplot
;  (gen [l]
;    (define nbins (count l))
;    (define trimmed (if (> nbins 50)
;                      ;; Take middle so that it fits on one line
;                      (clojure.core/take
;                       (clojure.core/drop l (/ (- nbins 50) 2))
;                       50)
;                      l))
;    (clojure.core/print
;     (clojure.core/format "%s\n"
;                          (clojure.core/vec (map (gen [p] (round (* p 100)))
;                                         trimmed))))))

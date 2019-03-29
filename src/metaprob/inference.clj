(ns metaprob.inference
  (:refer-clojure :exclude [map replicate apply])
  (:require [metaprob.generative-functions :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.distributions :refer :all]))

;; Probabilistic inference methods
;; TODO: Make these all generative functions with sensible tracing
;; ----------------------------------------------------------------------------

;; Rejection Sampling with predicate or log bound+obs trace
(defn rejection-sampling
  [& {:keys [model inputs observation-trace predicate log-bound]
      :or {inputs [] observation-trace {}}}]
  (let [[_ candidate-trace score]
        (infer-and-score :procedure model
                         :inputs inputs
                         :observation-trace observation-trace)]
    (cond
      predicate (if (predicate candidate-trace)
                  candidate-trace
                  (recur (list :model model :inputs inputs :predicate predicate)))
      log-bound (if (< (log (uniform 0 1)) (- score log-bound))
                  candidate-trace
                  (recur (list :model model :inputs inputs :observation-trace observation-trace :log-bound log-bound))))))

;; ----------------------------------------------------------------------------

(defn importance-sampling
  [& {:keys [model inputs f observation-trace n-particles]
      :or {n-particles 1, inputs [], observation-trace {}}}]
  (let [particles (replicate
                    n-particles
                    (fn []
                      (let [[v t s] (infer-and-score :procedure model
                                                     :observation-trace observation-trace
                                                     :inputs inputs)]
                                 [(* (exp s)
                                     (if f (f t) v)
                                     s)])))
        normalizer (exp (logsumexp (map second particles)))]

    (/ (reduce + (map first particles)) normalizer)))

(defn importance-resampling
  [& {:keys [model inputs observation-trace n-particles]
      :or {inputs [], observation-trace {}, n-particles 1}}]
  (let [particles (replicate n-particles
                             (fn []
                               (let [[_ t s] (infer-and-score :procedure model
                                                              :inputs inputs
                                                              :observation-trace observation-trace)]
                                 [t s])))]
    (first (nth particles (log-categorical (map second particles))))))


(defn likelihood-weighting
  [& {:keys [model inputs observation-trace n-particles]
      :or {inputs [] observation-trace {} n-particles 1}}]
  (let [weights (replicate n-particles
                           (fn []
                             (let [[_ _ s] (infer-and-score :procedure model
                                                            :inputs inputs
                                                            :observation-trace observation-trace)]
                               s)))]
    (exp (logmeanexp weights))))


;; TODO: Document requirements on proposer
(defn importance-resampling-custom-proposal
  [& {:keys [model proposer inputs observation-trace n-particles]
      :or {inputs [], observation-trace {}, n-particles 1}}]
  (let [custom-proposal
        (proposer observation-trace)

        proposed-traces
        (replicate n-particles
                   (fn []
                     (let [[_ t _]
                           (infer-and-score :procedure custom-proposal
                                            :inputs inputs)]
                       (trace-merge t observation-trace))))

        scores
        (map (fn [tr]
               (- (nth (infer-and-score :procedure model :inputs inputs :observation-trace tr) 2)
                  (nth (infer-and-score :procedure custom-proposal :inputs inputs :observation-trace tr) 2)))
             proposed-traces)]

    (nth proposed-traces (log-categorical scores))))

;; Custom proposal must not trace at any additional addresses.
;; Check with Marco about whether this can be relaxed; but I think
;; we need exact p/q estimates.
(defn with-custom-proposal-attached
  [orig-generative-function make-custom-proposer condition-for-use]
  (make-generative-function
    ;; To run in Clojure, use the same method as before:
    orig-generative-function

    ;; To create a constrained generator, first check if
    ;; the condition for using the custom proposal holds.
    ;; If so, use it and score it.
    ;; Otherwise, use the original make-constrained-generator
    ;; implementation.
    (fn [observations]
      (if (condition-for-use observations)
        (gen [& args]
             (let [custom-proposal
                   (make-custom-proposer observations)

                   ;; TODO: allow/require custom-proposal to specify which addresses it is proposing vs. sampling otherwise?
                   [_ tr _]
                   (trace-at '() infer-and-score
                      [:procedure custom-proposal,
                       :inputs args])

                   proposed-trace
                   (trace-merge observations tr)

                   [v tr2 p-score]
                   (infer-and-score :procedure orig-generative-function
                                    :inputs args
                                    :observation-trace proposed-trace)

                   [_ _ q-score]
                   (infer-and-score :procedure custom-proposal
                                    :inputs args
                                    :observation-trace proposed-trace)]
               [v proposed-trace (- p-score q-score)]))
        (make-constrained-generator orig-generative-function observations)))))


;;; ----------------------------------------------------------------------------
;;; Metropolis-Hastings

(defn symmetric-proposal-mh-step
  [& {:keys [model inputs proposal]
      :or {inputs []}}]
  (fn [current-trace]
    (let [[_ _ current-trace-score]
          (infer-and-score :procedure model
                           :inputs inputs
                           :observation-trace current-trace)

          proposed-trace
          (proposal current-trace)

          [_ _ proposed-trace-score]
          (infer-and-score :procedure model
                           :inputs inputs
                           :observation-trace proposed-trace)

          log-acceptance-ratio
          (min 0 (- proposed-trace-score current-trace-score))]

      (if (flip (exp log-acceptance-ratio))
        proposed-trace
        current-trace))))

(defn make-gaussian-drift-proposal
  [addresses width]
  (fn [current-trace]
    (reduce
      (fn [tr addr]
        (trace-set-value tr addr (gaussian (trace-value tr addr) width)))
      current-trace
      addresses)))

(defn gaussian-drift-mh-step
  [& {:keys [model inputs addresses address-predicate width]
      :or {inputs [], width 0.1}}]
  (let [proposal (if addresses
                    (fn [tr] (make-gaussian-drift-proposal addresses width))
                    (fn [tr] (make-gaussian-drift-proposal (filter address-predicate (addresses-of tr)) width)))]
    (fn [tr]
      ((symmetric-proposal-mh-step :model model, :inputs inputs, :proposal (proposal tr)) tr))))

(defn custom-proposal-mh-step [& {:keys [model inputs proposal], :or {inputs []}}]
  (fn [current-trace]
    (let [[_ _ current-trace-score]               ;; Evaluate log p(t)
          (infer-and-score :procedure model
                           :inputs inputs
                           :observation-trace current-trace)

          [proposed-trace all-proposer-choices _] ;; Sample t' ~ q(• <- t)
          (infer-and-score :procedure proposal
                           :inputs [current-trace])

          [_ _ new-trace-score]                   ;; Evaluate log p(t')
          (infer-and-score :procedure model
                           :inputs inputs
                           :observation-trace proposed-trace)

          [_ _ forward-proposal-score]            ;; Estimate log q(t' <- t)
          (infer-and-score :procedure proposal
                           :inputs [current-trace]
                           :observation-trace all-proposer-choices)

          [_ _ backward-proposal-score]          ;; Estimate log q(t <- t')
          (infer-and-score :procedure proposal
                           :inputs [proposed-trace]
                           :observation-trace proposed-trace)

          log-acceptance-ratio                  ;; Compute estimate of log [p(t')q(t <- t') / p(t)q(t' <- t)]
          (- (+ new-trace-score backward-proposal-score)
             (+ current-trace-score forward-proposal-score))]

      (if (flip (exp log-acceptance-ratio))   ;; Decide whether to accept or reject
        proposed-trace
        current-trace))))

(defn make-gibbs-step
  [& {:keys [model address support inputs]
      :or {inputs []}}]
  (fn [current-trace]
    (let [log-scores
          (map (fn [value]
                 (nth (infer-and-score :procedure model
                                       :inputs inputs
                                       :observation-trace
                                       (trace-set-value current-trace address value)) 2))
               support)]
      (trace-set-value
        current-trace
        address
        (nth support (log-categorical log-scores))))))


(def make-resimulation-proposal
  (fn [& {:keys [model inputs addresses address-predicate]
          :or {inputs []}}]
    (let [get-addresses
          (if address-predicate
            (fn [tr] (filter address-predicate (addresses-of tr)))
            (fn [tr] addresses))]
       (gen [old-trace]
            (let [addresses
                  (get-addresses old-trace)

                  [_ fixed-choices]
                  (partition-trace old-trace addresses)

                  constrained-generator
                  (make-constrained-generator model fixed-choices)

                  [_ new-trace _]
                  (trace-at '() constrained-generator inputs)]
              new-trace)))))


(defn resimulation-mh-move
  [model inputs tr addresses]
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
      tr)))


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


(defn sillyplot
  [l]
  (let [nbins (count l)
        trimmed
        (if (> nbins 50)
          (take (drop l (/ (- nbins 50) 2)) 50)
          l)]
    (print (format "%s\n" (vec (map (fn [p] p) trimmed))))))


;; A direct port of jar's old code
(defn check-bins-against-pdf
  [bins pdf]
  (let [n-samples
        (reduce + (map count bins))

        abs #(Math/abs %)

        bin-p
        (map (fn [bin] (/ (count bin) (float n-samples))) bins)

        bin-q
        (map (fn [bin]
               (let [bincount (count bin)]
                 (* (/ (reduce + (map pdf bin)) bincount)
                  (* (- (nth bin (- bincount 1))
                        (nth bin 0))
                     (/ (+ bincount 1)
                        (* bincount 1.0))))))
             bins)

        discrepancies
        (clojure.core/map #(abs (- %1 %2)) bin-p bin-q)

        trimmed (rest (reverse (rest discrepancies)))

        normalization (/ (count discrepancies) (* (count trimmed) 1.0))]
    [(* normalization (reduce + trimmed))
     bin-p bin-q]))

(defn check-samples-against-pdf
  [samples pdf nbins]
  (let [samples
        (vec (sort samples))

        n-samples
        (count samples)

        bin-size
        (/ n-samples (float nbins))

        bins
        (map (fn [i]
               (let [start (int (* i bin-size))
                     end (int (* (inc i) bin-size))]
                 (subvec samples start end)))
             (range nbins))]
    (check-bins-against-pdf bins pdf)))


(defn report-on-elapsed-time [tag thunk]
  (let [start (. System (nanoTime))
        ret (thunk)
        t (java.lang.Math/round (/ (double (- (. System (nanoTime)) start)) 1000000000.0))]
    (if (> t 1)
      (print (str tag ": elapsed time " t " sec\n")))
    ret))

(defn assay
  [tag sampler nsamples pdf nbins threshold]
  (report-on-elapsed-time
    tag
    (fn []
      (let [[badness bin-p bin-q]
            (check-samples-against-pdf (map sampler (range nsamples))
                                       pdf nbins)]
        (if (or (> badness threshold)
                (< badness (/ threshold 2)))
          (do (print (format "%s. n: %s bins: %s badness: %s threshold: %s\n" tag nsamples nbins badness threshold))
              (sillyplot bin-p)
              (sillyplot bin-q)))
          (< badness (* threshold 1.5))))))


(defn badness
  [sampler nsamples pdf nbins]
  (let [[badness bin-p bin-q]
        (check-samples-against-pdf (map sampler (range nsamples))
                                   pdf nbins)]
    badness))


(ns metaprob.examples.cgpm
  (:refer-clojure :only [
    defn into int? float? fn format merge repeatedly set select-keys zipmap])
  (:require
    [metaprob.syntax :refer :all]
    [metaprob.builtin :refer :all]
    [metaprob.prelude :refer :all]
    [metaprob.distributions :refer :all]
    [metaprob.examples.gaussian :refer [gaussian]]
    [metaprob.examples.cgpm_utils :refer :all]
    [metaprob.inference :refer :all]
    [metaprob.interpreters :refer :all]))

; ----------------------
; STATISTICAL DATA TYPES
; ----------------------

; Constructor of real statistical type with support [low, high].
(define make-ranged-real-type
  (gen [low high]
      {:name (format "real[low=%s high=%s]" low high)
       :valid? number?
       :in-support? (gen [x] (and (< low x) (< x high)))
       :base-measure :continuous}))

; Constructor of real statistical type with support {low, ..., high}.
(define make-ranged-integer-type
  (gen [low high]
      {:name (format "integer[low=%s high=%s]" low high)
       :valid? number?
       :in-support? (gen [x] (and (int? x) (<= low x) (<= x high)))
       :base-measure :discrete}))

; Constructor of a nominal statistical type with the given categories.
(define make-nominal-type
  (gen [categories]
    {:name (format "nominal[set-of-values=%s]" categories)
     :valid? (gen [x] (clojure.core/contains? categories x))
     :in-support? (gen [x] (clojure.core/contains? categories x))
     :base-measure :discrete}))

; The real statistical type i.e. support in [-infinity, infinity].
(define real-type
  {:name "real"
   :valid? number?
   :in-support? number?
   :base-measure :continuous})

; The integer statistical type i.e. support {-infinity, ..., infinity}.
(define integer-type
  {:name "integer"
   :valid? number?
   :in-support? int?
   :base-measure :discrete})

; --------------
; CGPM INTERFACE
; --------------

;; INITIALIZE

(define make-cgpm
  (gen [proc
        output-addrs-types
        input-addrs-types
        output-address-map
        input-address-map]
    (define output-addrs (set (keys output-addrs-types)))
    (define input-addrs (set (keys input-addrs-types)))
    (assert-no-overlap output-addrs input-addrs :outputs :inputs)
    (assert-has-keys output-address-map output-addrs)
    (assert-has-keys input-address-map input-addrs)
    (assert-valid-output-address-map output-address-map)
    (assert-valid-input-address-map input-address-map)
    (assoc proc
      :output-addrs-types output-addrs-types
      :input-addrs-types input-addrs-types
      :output-address-map output-address-map
      :input-address-map input-address-map)))

;; LOGPDF

(define validate-cgpm-logpdf
  (gen [cgpm target-addrs-vals constraint-addrs-vals input-addrs-vals]
    ; Confirm addresses are valid and do not overlap.
    (define target-addrs (set (keys target-addrs-vals)))
    (define constraint-addrs (set (keys constraint-addrs-vals)))
    (define input-addrs (set (keys input-addrs-vals)))
    (assert-no-overlap target-addrs constraint-addrs :targets :constraints)
    (assert-has-keys (get cgpm :output-addrs-types) target-addrs)
    (assert-has-keys (get cgpm :output-addrs-types) constraint-addrs)
    (assert-has-keys (get cgpm :input-addrs-types) input-addrs)
    ; Confirm values match the statistical data types.
    (validate-row (get cgpm :output-addrs-types) target-addrs-vals false)
    (validate-row (get cgpm :output-addrs-types) constraint-addrs-vals false)
    (validate-row (get cgpm :input-addrs-types) input-addrs-vals true)))

(define cgpm-logpdf
  (gen [cgpm target-addrs-vals constraint-addrs-vals input-addrs-vals]
    ; Error checking on the arguments.
    (validate-cgpm-logpdf
      cgpm target-addrs-vals constraint-addrs-vals input-addrs-vals)
    ; Convert target, constraint, and input addresses from CGPM to inf.
    (define target-addrs-vals'
      (rekey-addrs-vals (get cgpm :output-address-map) target-addrs-vals))
    (define constraint-addrs-vals'
      (rekey-addrs-vals (get cgpm :output-address-map) constraint-addrs-vals))
    (define target-constraint-addrs-vals
      (merge target-addrs-vals' constraint-addrs-vals'))
    (define input-args
      (extract-input-list (get cgpm :input-address-map) input-addrs-vals))
    ; Run infer to obtain probabilities.
    (define [retval trace log-weight-numer]
            (infer :procedure cgpm
                   :inputs input-args
                   :target-trace target-constraint-addrs-vals))
    (define log-weight-denom
      (if (> (count constraint-addrs-vals') 0)
          ; There are constraints: find marginal probability of constraints.
          (block
            (define [retval trace weight]
                    (infer :procedure cgpm
                           :inputs input-args
                           :target-trace constraint-addrs-vals'))
            weight)
          ; There are no constraints: log weight is zero.
          0))
    (- log-weight-numer log-weight-denom)))

;; SIMULATE

(define validate-cgpm-simulate
  (gen [cgpm target-addrs constraint-addrs-vals input-addrs-vals]
    ; Confirm addresses are valid and do not overlap.
    (define constraint-addrs (set (keys constraint-addrs-vals)))
    (define input-addrs (set (keys input-addrs-vals)))
    (assert-no-overlap target-addrs constraint-addrs :targets :constraints)
    (assert-has-keys (get cgpm :output-addrs-types) (set target-addrs))
    (assert-has-keys (get cgpm :output-addrs-types) constraint-addrs)
    (assert-has-keys (get cgpm :input-addrs-types) input-addrs)
    ; Confirm values match the statistical data types.
    (validate-row (get cgpm :output-addrs-types) constraint-addrs-vals false)
    (validate-row (get cgpm :input-addrs-types) input-addrs-vals true)))

(define cgpm-simulate
  (gen [cgpm target-addrs constraint-addrs-vals input-addrs-vals num-samples]
    ; Error checking on the arguments.
    (validate-cgpm-simulate
      cgpm target-addrs constraint-addrs-vals input-addrs-vals)
    ; Convert target, constraint, and input addresses from CGPM to inf.
    (define target-addrs'
      (rekey-addrs (get cgpm :output-address-map) target-addrs))
    (define constraint-addrs-vals'
      (rekey-addrs-vals (get cgpm :output-address-map) constraint-addrs-vals))
    (define input-args
      (extract-input-list (get cgpm :input-address-map) input-addrs-vals))
    ; Run infer to obtain the samples.
    (repeatedly num-samples
      (gen []
        (define [retval trace log-weight-numer]
          (infer :procedure cgpm
                 :inputs input-args
                 :target-trace constraint-addrs-vals'))
        ; Extract and return the requested samples.
        (extract-samples-from-trace
          trace target-addrs (get cgpm :output-address-map))))))

;; MUTUAL INFORMATION

(define compute-mi
  (gen [cgpm
        target-addrs-0
        target-addrs-1
        constraint-addrs-vals
        input-addrs-vals
        num-samples]
    ; Obtain samples for simple Monte Carlo integration.
    (define samples
      (cgpm-simulate cgpm (into [] (concat target-addrs-0 target-addrs-1))
                          constraint-addrs-vals input-addrs-vals num-samples))
    ; Compute joint log probabilities.
    (define logp-joint
      (map (fn [sample] (cgpm-logpdf cgpm sample constraint-addrs-vals
                                     input-addrs-vals))
           samples))
    ; Compute marginal log probabilities.
    (define logp-marginal-0
      (map (fn [sample] (cgpm-logpdf cgpm (select-keys sample target-addrs-0)
                                     constraint-addrs-vals input-addrs-vals))
           samples))
    (define logp-marginal-1
      (map (fn [sample] (cgpm-logpdf cgpm (select-keys sample target-addrs-1)
                                     constraint-addrs-vals input-addrs-vals))
           samples))
    ; MI is average log joint minus the average sum of log marginals.
    (- (compute-avg logp-joint)
       (+ (compute-avg logp-marginal-0)
          (compute-avg logp-marginal-1)))))

(define cgpm-mutual-information
  (gen [cgpm
        target-addrs-0
        target-addrs-1
        controlling-addrs
        constraint-addrs-vals
        input-addrs-vals
        num-samples-inner
        num-samples-outer]
    ; Make sure that fixed and controlling constraints do not overlap.
    (assert-no-overlap (set controlling-addrs)
                       (set (keys constraint-addrs-vals))
                       :constraint-addrs
                       :constraint-addrs-vals)
    ; Determine whether to average over the controlling variables.
    (if (= (count controlling-addrs) 0)
      ; No controlling variables: compute and return the MI directly.
      (compute-mi cgpm target-addrs-0 target-addrs-1 constraint-addrs-vals
                  input-addrs-vals num-samples-inner)
      ; Controlling variables: compute MI by averaging over them.
      (block
        ; Obtain samples for simple Monte Carlo integration.
        (define samples (cgpm-simulate cgpm controlling-addrs
                                       constraint-addrs-vals
                                       input-addrs-vals num-samples-outer))
        ; Pool the sampled constraints with user-provided constraints.
        (define constraints-merged
          (map (fn [sample] (merge sample constraint-addrs-vals))
               samples))
        ; Compute the MI for each sample.
        (define mutinf-values
          (map (fn [constraints] (compute-mi cgpm target-addrs-0
                                             target-addrs-1
                                             constraints input-addrs-vals
                                             num-samples-inner))
               constraints-merged))
        ; Return the average MI value.
        (compute-avg mutinf-values)))))

;; KL DIVERGENCE.

(define validate-cgpm-kl-divergence
  (gen [cgpm target-addrs-0 target-addrs-1]
    ; Confirm target address have same length.
    (assert-same-length target-addrs-0 target-addrs-1
                        :target-addrs-0 :target-addrs-1)
    ; Confirm base measures agree.
    (define output-addrs-types (get cgpm :output-addrs-types))
    (define gbm (fn [t] (get (safe-get output-addrs-types t) :base-measure)))
    (define base-measures-0 (map gbm target-addrs-0))
    (define base-measures-1 (map gbm target-addrs-1))
    (assert (= base-measures-0 base-measures-1)
            (format "targets %s and %s must have same base measures"
                    target-addrs-0 target-addrs-1))))

(define cgpm-kl-divergence
  (gen [cgpm
        target-addrs-0
        target-addrs-1
        constraint-addrs-vals-0
        constraint-addrs-vals-1
        input-addrs-vals
        num-samples]
    ; Make sure the base measures match.
    (validate-cgpm-kl-divergence cgpm target-addrs-0 target-addrs-1)
    ; Obtain the p samples for simple Monte Carlo integration.
    (define samples-p
      (cgpm-simulate cgpm target-addrs-0 constraint-addrs-vals-0
                     input-addrs-vals num-samples))
    ; Obtain the q samples.
    (define keymap (zipmap target-addrs-0 target-addrs-1))
    (define samples-q
      (map (fn [sample] (rekey-dict keymap sample))
           samples-p))
    ; Compute the probabilities under p.
    (define logp-p
      (map (fn [sample] (cgpm-logpdf cgpm sample constraint-addrs-vals-0
                         input-addrs-vals))
           samples-p))
    ; Compute the probabilities under q.
    (define logp-q
      (map (fn [sample] (cgpm-logpdf cgpm sample constraint-addrs-vals-1
                         input-addrs-vals))
           samples-q))
    ; KL is average log ratio.
    (- (compute-avg logp-p) (compute-avg logp-q))))

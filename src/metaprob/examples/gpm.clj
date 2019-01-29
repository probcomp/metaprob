;; Selected nuggets from python-metaprob's src/inference.vnts file
;; See also figure 29 of the 7/17 chapter mss

(ns metaprob.examples.gpm
  (:refer-clojure :only [
    declare defn let format filter fn float? int?
    repeat set vals])
(:require
            [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all :exclude [flip]]
            [metaprob.examples.gaussian :refer [gaussian]]
            [metaprob.inference :refer :all]
            [metaprob.interpreters :refer :all]))

(define flip
  (assoc metaprob.distributions/flip :support '(true false)))


(define quake-env (make-top-level-env 'metaprob.examples.gpm))

;; Convert a tuple of booleans to an integer.
;; Tuple element 0 determines the highest order bit.

(define booleans-to-binary
  (opaque        ;Do not score
   (gen [qu]
     (define len (count qu))
     (define luup
       (gen [i n]
         (if (>= i len)
           n
           (luup (+ i 1) (+ (* 2 n)
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
      :overlay-densities '())))

;; ----------------------------------------------------------------------------
;; Calculate exact probabilities


;; Returns a list of output traces

(define joint-enumerate
  (gen [sites]
    (if (not (empty? sites))
      (block
        (define others (joint-enumerate (rest sites)))
        (define site (first sites))
        (print ["site:" site])
        (if (compound? site)
          (block (define oper-name (clojure.core/last site))
                 (define oper (top-level-lookup quake-env (clojure.core/symbol oper-name)))
                 (if (and (compound? oper)
                          (contains? oper :support))
                   (block
                    (define value-candidates
                      (get oper :support))
                    (define trace-lists
                      (map (gen [value]
                             (map (gen [t]
                                    (trace-set-value t site value))
                                  others))
                           value-candidates))
                    (concat trace-lists))
                   others))
          others))
      ; No sites to enumerate: only the empty trace is possible
      '({}))))

;; Returns list of [state score] where state is value returned by
;;  earthquake-bayesian-network

(define enumerate-executions
  (gen [proc inputs intervention-trace target-trace]
    (print [(count (addresses-of intervention-trace)) "interventions"])
    (define [_ one-run _]
      (infer :procedure proc
             :inputs inputs
             :intervention-trace intervention-trace))
    (define all-sites (addresses-of one-run))
    (print [(count all-sites) "sites"])
    (define free-sites
      (set-difference
       (set-difference all-sites (addresses-of intervention-trace))
       (addresses-of target-trace)))
    (print [(count free-sites) "free-sites"])
    (define candidates (joint-enumerate free-sites))
    (map (gen [candidate]
           ;; Returns [state nil score]
           (define [state _ score]
             (infer :procedure proc
                    :inputs inputs
                    :intervention-trace intervention-trace
                    :target-trace (trace-merge candidate target-trace)
                    :output-trace? false))
           [state score])
         candidates)))

;; Takes a list of [state score] and returns a list of samples.
;; A good multiplier is 12240.
;; The purpose is just so that we can easily reuse the histogram
;; plotting logic.

(define fake-samples-for-enumerated-executions
  (gen [state-and-score-list multiplier]
    (concat (map (gen [[state score]]
                   ;; state will be a number from 0 to 31
                   (define count (round (* (exp score) multiplier)))
                   (print [state score (exp score) count])
                   (map (gen [_] state)
                        (range count)))
                 state-and-score-list))))

;; ----------------------------------------------------------------------------
;; Sample from the prior

;; Each sample is an output trace.

(define prior-samples
  (gen [n-samples]
    (replicate n-samples
               (gen []
                 ((infer :procedure earthquake-bayesian-network) 1)))))

;; Test intervention

(define alarm-address '(3 "alarm" "flip"))
(define alarm-went-off (trace-set-value {} alarm-address true))

(define check-alarm-intervention
  (gen []
    (define [_ output _]
      (infer :procedure earthquake-bayesian-network))
    (assert (trace-has-value? output alarm-address)
            "check validity of alarm intervention")))

(define eq-rejection-assay
  (gen [number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (print "rejection sample") ;Progress meter
       (trace-value ; TODO: Why trace-value?
        (rejection-sampling earthquake-bayesian-network
                            []        ; inputs
                            alarm-went-off ;intervention
                            0)))))) ; log-bound

(define eq-importance-assay
  (gen [n-particles number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (trace-value (importance-resampling ; TODO: Why trace value?
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
      (enumerate-executions earthquake-bayesian-network [] {} {}))
    (define fake-samples
      (fake-samples-for-enumerated-executions exact-probabilities 12240))
    (earthquake-histogram "exact bayesnet prior probabilities"
                          fake-samples)

    (print "Exact alarm-went-off probabilities")
    (define exact-awo-probabilities
      (enumerate-executions earthquake-bayesian-network [] alarm-went-off {}))
    (define fake-awo-samples
      (fake-samples-for-enumerated-executions exact-awo-probabilities 12240))
    (earthquake-histogram "exact bayesnet alarm-went-off probabilities"
                          fake-awo-samples)

    (define number-of-samples 100)

    (print "bayesnet sampling from the prior")
    (earthquake-histogram "bayesnet sampled prior probabilities"
                          (prior-samples number-of-samples))

    (print "bayesnet rejection sampling")
    (earthquake-histogram "bayesnet samples from rejection sampling"
                          (eq-rejection-assay number-of-samples))

    (print "bayesnet importance sampling")
    (earthquake-histogram "bayesnet samples from importance sampling with 20 particles"
                          (eq-importance-assay 20 number-of-samples))))


; START OF GPM WORK.

; Statistical data types

(define safe-get
  (gen [collection item]
    (if (contains? collection item)
      (get collection item)
      (assert false (format "no such key %s in key set %s"
                            item (keys collection))))))

(define make-real-ranged-type
  (gen [low high]
      {:name (format "real[low=%s high=%s]" low high)
        :valid? (gen [x] (and (< low x) (< x high)))
        :base-measure :continuous}))

(define make-int-ranged-type
  (gen [low high]
      {:name (format "integer[low=%s high=%s]" low high)
        :valid? (gen [x] (and (int? x) (<= low x) (<= x high)))
        :base-measure :discrete}))

(define make-nominal-type
  (gen [categories]
    {:name (format "nominal[categories=%s]" categories)
      :valid? (gen [x] (clojure.core/contains? categories x))
      :base-measure :discrete}))

(define real-type
  {:name "real"
    :valid? (gen [x] (or (float? x) (int? x)))
    :base-measure :continuous})

(define integer-type
  {:name "integer"
    :valid? int?
    :base-measure :discrete})


(define validate-cell
  (gen [stattype value]
    (assert ((get stattype :valid?) value)
            (format "invalid value %s for stattype %s"
                    value (get stattype :name)))))
(define validate-row
  (gen [output-addrs-types addrs-vals]
    (define violations
        (filter
          (fn [[k v]] (validate-cell (safe-get output-addrs-types k) v))
          addrs-vals))
    (assert (= (count violations) 0)
               (format "invalid values %s for types %s"
                       violations output-addrs-types))))

; Initialize and return a GPM for the given Metaprob procedure.

(define assert-same-length
  (gen [set-a set-b name-a name-b]
    (assert (= (count set-a) (count set-b))
            (format "%s %s and %s %s must have same length"
                    name-a set-a name-b set-b))))

(define assert-no-overlap
  (gen [set-a set-b name-a name-b]
    (define overlap (clojure.set/intersection set-a set-b))
    (assert (= (count overlap) 0)
            (format "%s %s and %s %s must be disjoint"
                    name-a set-a name-b set-b))))

(define assert-has-keys
  (gen [collection items]
    (define missing (clojure.set/difference items (keys collection)))
    (assert (= (count missing) 0)
            (format "collection %s is missing keys %s" collection items))))

(define assert-valid-input-address-map
  (gen [input-address-map]
    (define invalid-address-values
      (filter
        (fn [[k v]] (not (int? v)))
        input-address-map))
    ; Make sure that the values are all integers.
    (assert (= (count invalid-address-values) 0)
            (format "input addresses must map to integers %s" input-address-map))
    ; Make sure that the integers are consecutive 0...n-1
    (define sorted-address-values (sort (vals input-address-map)))
    (define num-inputs (count sorted-address-values))
    (assert (= (range 0 num-inputs) sorted-address-values)
            (format "input addresses must map to consecutive integers %s"
                    input-address-map))))

(define make-cgpm
  (gen [proc output-addrs-types input-addrs-types
        output-address-map input-address-map]
    (define output-addrs (set (keys output-addrs-types)))
    (define input-addrs (set (keys input-addrs-types)))
    (assert-no-overlap output-addrs input-addrs :outputs :inputs)
    (assert-has-keys output-address-map output-addrs)
    (assert-has-keys input-address-map input-addrs)
    (assert-valid-input-address-map input-address-map)
    (assoc proc
      :outputs output-addrs-types
      :inputs input-addrs-types
      :output-address-map output-address-map
      :input-address-map input-address-map)))

; Define a minimal inf.

(define generate-dummy-row
  (gen [y]
    (with-explicit-tracer t
      (define x0 (t "x0" uniform-sample [1 2 3 4]))
      (define x1 (t "x1" uniform 9 199))
      (define x2 (t "x2" gaussian 0 10))
      (define x3 (t "x3" labeled-categorical ["foo" "bar" "baz"]
                                             [0.25 0.5 0.25]))
      [x0 x1 x2 x3])))

(defn -main [& args]
  (let [proc generate-dummy-row
        outputs {
          :x0 (make-nominal-type #{1 2 3 4})
          :x1 (make-int-ranged-type 9 199)
          :x2 real-type
          :x3 (make-nominal-type #{"foo" "bar" "baz"})}
        inputs {:y real-type}
        output-addr-map {:x0 "x0", :x1 "x1", :x2 "x2", :x3 "x3"}
        input-addr-map {:y 0}]
    (define gpm (make-cgpm proc outputs inputs output-addr-map input-addr-map))
    (print (gaussian 1 10))
    (define [retval trace weight]
            (infer :procedure generate-dummy-row :inputs [1]
                   :target-trace {"x3" {:value 10}}))
    (print trace)
    (validate-cell (make-real-ranged-type 0 10) 2)
    (validate-row outputs {:x0 3, :x3 "bar"})
    (validate-row inputs {:y 100})
    (print 1)))

; Reproduce the random polynomial example.

(defn pow [x n]
  (reduce * 1 (repeat n x)))

(define generate-curve
  (gen []
    (define degree (uniform-sample [1 2 3 4]))
    (define coeffs (replicate degree (gen [] (gaussian 0 1))))
    (gen [x] (reduce + 0 (map (gen [n] (* (nth coeffs n) (pow x n)))
                              (range degree))))))

(define add-noise-to-curve
  (gen [curve]
    (gen [x]
      (define mean (curve x))
      (define variance 0.1)
      (gaussian mean variance))))

(define curve-model
  (gen [xs]
    (map (add-noise-to-curve (generate-curve)) xs)))

; (defn -main [& args]
;   (define xs '(-0.5 -0.3 0.1 0.2 0.5))
;   (define ys (curve-model xs))
;   (define result (infer-apply curve-model [xs] nil {"map" {0 {"gaussian" {:value 0.06}}}} nil))
;   (print result))

;; Selected nuggets from python-metaprob's src/inference.vnts file
;; See also figure 29 of the 7/17 chapter mss

(ns metaprob.examples.gpm
  (:refer-clojure :only [
    declare defn into let format filter fn float? int?
    merge repeat set vals])
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
            (format "invalid value \"%s\" for stattype %s"
                    value (get stattype :name)))))

(define validate-row
  (gen [addrs-types addrs-vals check-all-exist?]
    (define violations
        (filter
          (fn [[k v]] (validate-cell (safe-get addrs-types k) v))
          addrs-vals))
    (assert (= (count violations) 0)
               (format "invalid values %s for types %s"
                        violations addrs-types))
    (if check-all-exist?
        (assert (= (set (keys addrs-types)) (set (keys addrs-vals)))
                (format "row %s must have values for %s"
                        addrs-vals addrs-types))
        nil)))

; Initialize and return a GPM for the given Metaprob procedure.

; Check whether set-a and set-b have the same number of items.
(define assert-same-length
  (gen [set-a set-b name-a name-b]
    (assert (= (count set-a) (count set-b))
            (format "%s %s and %s %s must have same length"
                    name-a set-a name-b set-b))))

; Check whether set-a and set-b have no overlapping items.
(define assert-no-overlap
  (gen [set-a set-b name-a name-b]
    (define overlap (clojure.set/intersection set-a set-b))
    (assert (= (count overlap) 0)
            (format "%s %s and %s %s must be disjoint"
                    name-a set-a name-b set-b))))

; Check whether all the items are keys of the given collection.
(define assert-has-keys
  (gen [collection items]
    (define collection-keys (set (keys collection)))
    (define invalid-items (clojure.set/difference items collection-keys))
    (assert (= (count invalid-items) 0)
            (format "key set %s does not have some of the keys in %s"
                     collection-keys items))))

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

(define assert-valid-output-address-map
  (gen [output-address-map]
    ; The values of the address map should be distinct.
    (define values (vals output-address-map))
    (assert (= (count (set values)) (count values))
            (format "addresses should have distinct values %s"
                    output-address-map))))

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

; Implement logpdf and simulate.

(define rekey-addrs-vals
  (gen [address-map addrs-vals]
    (define converter (fn [[k v]] [(safe-get address-map k) {:value v}]))
    (into {} (map converter addrs-vals))))

(define extract-input-list
  (gen [address-map addrs-vals]
    (define compr (fn [k1 k2] (< (get address-map k1) (get address-map k2))))
    (define ordered-keys (sort compr (keys addrs-vals)))
    (map (fn [k] (get addrs-vals k)) ordered-keys)))

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
                           :target-trace target-constraint-addrs-vals))
            weight)
          0))
          ; There are no constraints: log weight is zero.
    (- log-weight-numer log-weight-denom)))

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
        outputs-addrs-types {
          :x0 (make-nominal-type #{1 2 3 4})
          :x1 (make-int-ranged-type 9 199)
          :x2 real-type
          :x3 (make-nominal-type #{"foo" "bar" "baz"})}
        inputs-addrs-types {:y real-type}
        output-addr-map {:x0 "x0", :x1 "x1", :x2 "x2", :x3 "x3"}
        input-addr-map {:y 0}]
    (define gpm
      (make-cgpm proc outputs-addrs-types
                      inputs-addrs-types
                      output-addr-map
                      input-addr-map))
    (define [retval trace weight]
            (infer :procedure generate-dummy-row
                   :inputs [100]
                   :target-trace {"x1" {:value 200}}))
    (print weight)
    ; (validate-cell (make-real-ranged-type 0 10) 2)
    ; (validate-row outputs-addrs-types {:x0 3, :x3 "bar"} false)
    ; (validate-row inputs-addrs-types {:y 100} true)
    ; (print (cgpm-logpdf gpm {:x0 2} {} {:y 100}))
    ; (print (cgpm-logpdf gpm {:x1 120} {} {:y 100}))
    ; (print (cgpm-logpdf gpm {:x0 2 :x1 120} {} {:y 100}))
    (print "exit status 0")))

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

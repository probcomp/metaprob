;; Selected nuggets from python-metaprob's src/inference.vnts file
;; See also figure 29 of the 7/17 chapter mss

(ns metaprob.examples.gpm
  (:refer-clojure :only [
    declare defn into let format filter fn float? int?
    merge repeatedly set vals])
(:require
            [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all :exclude [flip]]
            [metaprob.examples.gaussian :refer [gaussian]]
            [metaprob.inference :refer :all]
            [metaprob.interpreters :refer :all]))


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

(define rekey-addrs
  (gen [address-map addrs]
    (define convert (fn [k] (safe-get address-map k)))
    (map convert addrs)))

(define extract-input-list
  (gen [address-map addrs-vals]
    (define compr (fn [k1 k2] (< (get address-map k1) (get address-map k2))))
    (define ordered-keys (sort compr (keys addrs-vals)))
    (map (fn [k] (get addrs-vals k)) ordered-keys)))

(define extract-samples-from-trace
  (gen [trace target-addrs output-addr-map]
    (define extract
      (fn [k]
        (let [result (get trace (safe-get output-addr-map k))]
          [k (safe-get result :value)])))
    (into {} (map extract target-addrs))))

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

(define validate-simulate
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
    (validate-simulate cgpm target-addrs constraint-addrs-vals input-addrs-vals)
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
    (print (cgpm-logpdf gpm {:x0 2} {} {:y 100}))
    (print (cgpm-logpdf gpm {:x1 120} {} {:y 100}))
    (print (cgpm-logpdf gpm {:x0 2 :x1 120} {} {:y 100}))
    (print (cgpm-simulate gpm [:x0 :x1 :x2] {} {:y 100} 10))
    (print (cgpm-simulate gpm [:x0 :x1 :x2] {:x3 "foo"} {:y 100} 10))
    (print "exit status 0")))

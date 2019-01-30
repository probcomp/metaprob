(ns metaprob.examples.cgpm_utils
  (:refer-clojure :only [
    int? into let filter fn format set vals])
(:require
            [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all :exclude [flip]]
            [metaprob.inference :refer :all]
            [metaprob.interpreters :refer :all]))

; ------------------------
; CGPM INTERFACE UTILITIES
; ------------------------

; Compute simple average of items.
(define compute-avg
  (gen [items]
    (/ (reduce + 0 items) (count items))))

; Check if collection contains item before invoking get.
; Throws an assertion error in item is not in collection (instead of nil).
(define safe-get
  (gen [collection item]
    (if (contains? collection item)
      (get collection item)
      (assert false (format "no such key %s in key set %s"
                            item (keys collection))))))

; Assert that value is valid for given statistical type.
(define validate-cell
  (gen [stattype value]
    (assert ((get stattype :valid?) value)
            (format "invalid value \"%s\" for stattype %s"
                    value (get stattype :name)))))

; Assert that row of values are valid for given statistical types.
; addrs-types is a dictionary from keys to statistical types.
; addrs-vals is a dictionary from keys to values.
; check-all-exist? boolean to assert keys(addrs-vals) == keys(addrs-types).
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

; Assert that set-a and set-b have the same number of items.
(define assert-same-length
  (gen [set-a set-b name-a name-b]
    (assert (= (count set-a) (count set-b))
            (format "%s %s and %s %s must have same length"
                    name-a set-a name-b set-b))))

; Assert that set-a and set-b have no overlapping items.
(define assert-no-overlap
  (gen [set-a set-b name-a name-b]
    (define overlap (clojure.set/intersection set-a set-b))
    (assert (= (count overlap) 0)
            (format "%s %s and %s %s must be disjoint"
                    name-a set-a name-b set-b))))

; Assert that items is a subset of the keys of the given collection.
(define assert-has-keys
  (gen [collection items]
    (define collection-keys (set (keys collection)))
    (define invalid-items (clojure.set/difference items collection-keys))
    (assert (= (count invalid-items) 0)
            (format "key set %s does not have some of the keys in %s"
                     collection-keys items))))

; Assert that the input-address-map for a CGPM is valid.
; The values of input-address-map must be contiguous integers starting at 0.
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

; Assert that the output-address-map for a GPM is valid.
; The values of output-address-map must be distinct.
(define assert-valid-output-address-map
  (gen [output-address-map]
    ; The values of the address map should be distinct.
    (define values (vals output-address-map))
    (assert (= (count (set values)) (count values))
            (format "addresses should have distinct values %s"
                    output-address-map))))

; Convert a CGPM row into a Metaprob target-trace.
(define rekey-addrs-vals
  (gen [address-map addrs-vals]
    (define converter (fn [[k v]] [(safe-get address-map k) {:value v}]))
    (into {} (map converter addrs-vals))))

; Convert a list of CGPM addresses into Metaprob addresses.
(define rekey-addrs
  (gen [address-map addrs]
    (define convert (fn [k] (safe-get address-map k)))
    (map convert addrs)))

; Convert a CGPM row into list of arguments of Metaprob gen.
(define extract-input-list
  (gen [address-map addrs-vals]
    (define compr (fn [k1 k2] (< (get address-map k1) (get address-map k2))))
    (define ordered-keys (sort compr (keys addrs-vals)))
    (map (fn [k] (get addrs-vals k)) ordered-keys)))

; Convert a Metaprob trace to a CGPM row.
(define extract-samples-from-trace
  (gen [trace target-addrs output-addr-map]
    (define extract
      (fn [k]
        (let [result (get trace (safe-get output-addr-map k))]
          [k (safe-get result :value)])))
    (into {} (map extract target-addrs))))

; Rewrite keys of dictionary according to keymap.
(define rekey-dict
  (gen [keymap dict]
    (into {} (map (fn [[k v]] [(safe-get keymap k) v]) dict))))


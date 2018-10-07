(ns metaprob.state.common)

(def rest-marker "rest")

(defn state? [val]
  (or (seq? val)                        ;Strings are not seqs
      (vector? val)
      (map? val)))

(defn empty-state? [state]
  (and (state? state)
       (empty? state)))

(defn value-only-trace?
  "Convert heterogeneous canonical clojure form to hash-map"
  [tr]
  (and (map? tr)
       (= (count tr) 1)
       (contains? tr :value)))

(defn state-to-map
  "Convert heterogeneous canonical clojure form to hash-map"
  [state]
  (cond (map? state) state

        (seq? state)
        (if (empty? state)
          {}
          {:value (first state) rest-marker (rest state)})

        (vector? state)
        (into {} (map (fn [i x] [i {:value x}])
                      (range (count state))
                      state))

        true (assert false ["not a state" state])))

(defn keys-sans-value [m]
  (let [ks (remove #{:value} (keys m))]
    (if (= ks nil)
      '()
      ks)))

;; Constructors

(defn empty-state [] {})    ; 'lein test' passes with () and [] here as well

(defn map-to-state
  "Convert hash-map to heterogeneous canonical clojure form."
  [m]
  ;; I'm sorry I failed to record the reason that the 'don't be lazy'
  ;; command is there; there must have been a failure at some point that
  ;; I attributed to laziness in these maps.
  (doseq [entry m] true)    ;Don't be lazy!
  (let [n (count m)]
    (if (= n 0)
      (empty-state)
      (let [value (get m :value :no-value)]
        (if (= value :no-value)
          ;; Has no value: could be a vector.
          (if (every? (fn [i] (value-only-trace? (get m i)))
                      (range n))
            (vec (for [i (range n)] (get (get m i) :value)))
            m)
          ;; Has value: could be a seq / list.
          (if (= n 2)
            (let [rest (get m rest-marker :no-value)]
              (if (= rest :no-value)
                m                 ;No "rest", so just an ordinary dict
                (if (empty-state? rest)
                  (cons value '())    ;Allow termination in () [] or {}
                  (if (seq? rest)
                    (cons value rest)
                    m))))
            m))))))

;; Trace states

(ns metaprob.state)

(declare map-to-state state-to-map keys-sans-value)

;; The `steady?` flag controls a choice between a simple but slower
;; implementation and an obscure but faster implementation of the
;; primitives.

(def steady? false)

(def rest-marker "rest")

(defn state? [val]
  (or (seq? val)                        ;Strings are not seqs
      (vector? val)
      (map? val)))

;; Basic trace operations

(defn has-value? [state]
  (if steady?
    ;; The two methods should be equivalent
    (contains? (state-to-map state))
    (cond (seq? state) (not (empty? state))
          (vector? state) false
          (map? state) (contains? state :value)
          :else (assert false ["not a state" state]))))

(defn value [state]
  (if steady?
    ;; The two methods should be equivalent
    (get (state-to-map state) :value)
    (cond (seq? state) (first state)
          (vector? state) (assert false "no value")
          (map? state)
          (do (assert (contains? state :value) ["state has no value" state])
              (get state :value))
          :else (assert false ["not a state" state]))))

(defn has-subtrace? [state key]
  (if steady?
    (contains? (state-to-map state) key)
    (cond (seq? state) (and (seq state)
                            (= key rest-marker))
          (vector? state) (and (integer? key)
                               (>= key 0)
                               (< key (count state)))
          (map? state) (contains? state key)
          true (assert false ["not a state" state]))))

(defn subtrace [state key]
  (let [val (if steady?
              (get (state-to-map state) key)
              (cond (seq? state) (rest state)
                    (vector? state) {:value (nth state key)}
                    (map? state) (get state key)
                    true (assert false ["not a state" state])))]
    (assert (some? val)
            ["no such subtrace" key state])
    val))

(defn state-keys [state]
  (if steady?
    (keys-sans-value (state-to-map state))
    (cond (seq? state) (if (empty? state) '() (list rest-marker))
          (vector? state) (range (count state))
          (map? state) (keys-sans-value state)
          true (assert false ["not a state" state]))))

(defn ^:private keys-sans-value [m]   ;aux for above
  (let [ks (remove #{:value} (keys m))]
    (if (nil? ks)
      '()
      ks)))

(defn subtrace-count [state]
  (if steady?
    (count (state-keys state))
    (cond (seq? state) (if (empty? state) 0 1)
          (vector? state) (count state)
          (map? state) (let [n (count state)]
                         (if-not (contains? state :value)
                           n
                           (dec n)))
          true (assert false ["not a state" state]))))

;; Constructors

(defn empty-state []
  '())

(defn set-value [state val]
  (map-to-state (assoc (state-to-map state) :value val)))

(defn clear-value [state]
  (map-to-state (dissoc (state-to-map state) :value)))

(defn set-subtrace [state key sub]
  ;; sub is a trace but not necessarily a sub
  ;(if (= sub '())
  ;  state
  (map-to-state (assoc (state-to-map state) key sub)))

(defn clear-subtrace [state key]
  (map-to-state (dissoc (state-to-map state) key)))



;; Convert heterogeneous canonical clojure form to hash-map

(defn state-to-map [state]
  (cond (map? state) state

        (seq? state)
        (if (empty? state)
          {}
          {:value (first state) rest-marker (rest state)})

        (vector? state)
        (into {} (map (fn [i x] [i {:value x}])
                      (range (count state))
                      state))

        true (assert false ["not a state" state])
        ))

(defn value-only-trace? [tr]
  (and (map? tr)
       (= (count tr) 1)
       (contains? tr :value)))

;; Convert hash-map to heterogeneous canonical clojure form

(defn map-to-state [m]
  (let [n (count m)]
    (cond (and (= n 2)
               (contains? m :value)
               (seq? (get m rest-marker)))
          (cons (get m :value)
                (get m rest-marker))

          (zero? n) '()                   ;Kludge to ensure seq-ness

          (every? (fn [n] (value-only-trace? (get m n :no-value))) (range n))
          (vec (for [i (range n)] (get (get m i) :value)))

          true (do (assert (map? m) ["expected a map" m])
                   (doseq [entry m] true)    ;Don't be lazy!
                   m))))

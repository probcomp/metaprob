;; Trace states

(ns metaprob.state)

(declare map-to-state state-to-map keys-sans-value)

;; The `steady?` flag controls a choice between a simple but slower
;; implementation and an obscure but faster implementation of the
;; primitives.

(def steady? false)

(def rest-marker "rest")

(defn state? [val]
  (or (seq? val)
      (vector? val)
      (map? val)))

;; Basic trace operations

(defn has-value? [state]
  (if steady?
    ;; The two methods should be equivalent
    (not (= (get (state-to-map state) :value :no-value) :no-value))
    (cond (seq? state) (not (empty? state))
          (vector? state) false
          (map? state) (not (= (get state :value :no-value) :no-value))
          true (assert false ["not a state" state]))))

(defn value [state]
  (if steady?
    ;; The two methods should be equivalent
    (get (state-to-map state) :value)
    (cond (seq? state) (first state)
          (vector? state) (assert false "no value")
          (map? state) (get state :value)
          true (assert false ["not a state" state]))))

(defn has-subtrace? [state key]
  (if steady?
    (not (= (get (state-to-map state) key :no-value) :no-value))
    (cond (seq? state) (= key rest-marker)
          (vector? state) (and (integer? key)
                               (>= key 0)
                               (< key (count state)))
          (map? state) (not (= (get state key :no-value) :no-value))
          true (assert false ["not a state" state]))))

(defn subtrace [state key]
  (if steady?
    (get (state-to-map state) key)
    (cond (seq? state) (rest state)
          (vector? state) {:value (nth state key)}
          (map? state) (get state key)
          true (assert false ["not a state" state]))))

(defn state-keys [state]
  (if steady?
    (keys-sans-value (state-to-map state))
    (cond (seq? state) (if (empty? state) '() (list rest-marker))
          (vector? state) (range (count state))
          (map? state) (keys-sans-value state)
          true (assert false ["not a state" state]))))

(defn ^:private keys-sans-value [m]   ;aux for above
  (let [ks (remove #{:value} (keys m))]
    (if (= ks nil)
      '()
      ks)))

(defn subtrace-count [state]
  (if steady?
    (count (state-keys state))
    (cond (seq? state) (if (empty? state) 0 1)
          (vector? state) (count state)
          (map? state) (let [n (count state)]
                         (if (= (get state :value :no-value) :no-value)
                           n
                           (- n 1)))
          true (assert false ["not a state" state]))))

;; Constructors

(defn empty-state []
  '())

(defn set-value [state val]
  (map-to-state (assoc (state-to-map state) :value val)))

(defn clear-value [state]
  (map-to-state (dissoc (state-to-map state) :value)))

(defn set-subtrace [state key sub]
  (map-to-state (assoc (state-to-map state) key sub)))

(defn clear-subtrace [state key]
  (map-to-state (dissoc state key)))



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
               (not (= (get m :value :no-value) :no-value))
               (seq? (get m rest-marker :no-value)))
          (cons (get m :value)
                (get m rest-marker))

          (= n 0) '()                   ;Kludge to ensure seq-ness

          (and (= (get m :value :no-value) :no-value)
               (value-only-trace? (get m 0 :no-value))
               (value-only-trace? (get m (- n 1) :no-value)))
          (vec (for [i (range n)] (get (get m i) :value)))

          true (do (assert (map? m) ["expected a map" m])
                   m))))

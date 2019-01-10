(ns metaprob.state
  "Trace states.")

(declare map-to-state state-to-map keys-sans-value)

;; The `steady?` flag controls a choice between a simple but slower
;; implementation and an obscure but faster implementation of the
;; primitives.
(def steady? false)

(def rest-marker "rest")

(defn state? [val]
  (or (seq? val)                        ; Strings are not seqs
      (vector? val)
      (map? val)))

(defn empty-state? [state]
  (and (state? state)
       (empty? state)))

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
          (map? state)
          (let [value (get state :value :no-value)]
            (assert (not (= value :no-value)) ["state has no value" state])
            value)
          true (assert false ["not a state" state]))))

(defn has-subtrace? [state key]
  (if steady?
    (contains? (state-to-map state) key)
    (cond (seq? state) (and (not (empty? state))
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
    (assert (not (= val nil))
            ["no such subtrace" key state])
    val))

(defn state-keys
  "Returns a seq of keys (without the :value marker)"
  [state]
  (if steady?
    (keys-sans-value (state-to-map state))
    (cond (seq? state) (if (empty? state) '() (list rest-marker))
          (vector? state) (range (count state))
          (map? state) (keys-sans-value state)
          true (assert false ["not a state" state]))))

(defn ^:private keys-sans-value [m]   ; aux for above
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

(defn empty-state [] {})    ; 'lein test' passes with () and [] here as well

(defn set-value [state val]
  (map-to-state (assoc (state-to-map state) :value val)))

(defn clear-value [state]
  (map-to-state (dissoc (state-to-map state) :value)))

(defn set-subtrace [state key sub]
  (map-to-state (assoc (state-to-map state) key sub)))

(defn clear-subtrace [state key]
  (map-to-state (dissoc (state-to-map state) key)))

(defn state-to-map
  "Convert heterogeneous canonical clojure form to hash-map."
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

(defn value-only-trace? [tr]
  (and (map? tr)
       (= (count tr) 1)
       (contains? tr :value)))


(defn map-to-state
  "Convert hash-map to heterogeneous canonical clojure form.

  I'm sorry I failed to record the reason that the 'don't be lazy'
  command is there; there must have been a failure at some point that
  I attributed to laziness in these maps."
  [m]
  (doseq [entry m] true)    ; Don't be lazy!
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
                m                 ; No "rest", so just an ordinary dict
                (if (empty-state? rest)
                  (cons value '())    ; Allow termination in () [] or {}
                  (if (seq? rest)
                    (cons value rest)
                    m))))
            m))))))

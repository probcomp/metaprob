(ns metaprob.trace)

(defn trace-subtrace [tr adr]
  ((if (seq? adr) get-in get) tr adr))

(defn trace-has-value?
  ([tr] (contains? tr :value))
  ([tr adr] (contains? (trace-subtrace tr adr) :value)))

(defn trace-value
  ([tr] (get tr :value))
  ([tr adr] (get (trace-subtrace tr adr) :value)))

(defn trace-has-subtrace? [tr adr]
  (if (seq? adr)
    (if (empty? adr)
      true
      (if (contains? tr (first adr))
        (recur (get tr (first adr)) (rest adr))))
  (contains? tr adr)))

(defn trace-keys [tr]
  (filter (fn [x] (not= x :value)) (keys tr)))

(defn subtrace-count [tr]
  (- (count tr) (if (trace-has-value? tr) 1 0)))

(defn trace-set-subtrace [tr adr sub]
  (if (seq? adr)
    (if (empty? adr)
      sub
      (assoc tr (first adr) (trace-set-subtrace (get tr (first adr)) (rest adr) sub)))
    (assoc tr adr sub)))

(defn trace-set-value
  ([tr val] (assoc tr :value val))
  ([tr adr val]
   (trace-set-subtrace tr adr (trace-set-value (trace-subtrace tr adr) val))))
; TODO: Only traverse once?

(defn trace-clear-value
  ([tr] (dissoc tr :value))
  ([tr adr] (trace-set-subtrace tr adr (trace-clear-value (trace-subtrace tr adr)))))

(declare trace-clear-subtrace)
(defn maybe-set-subtrace
  [output adr suboutput]
  (if (empty? suboutput)
    (trace-clear-subtrace output adr)
    (trace-set-subtrace output adr suboutput)))

(defn trace-clear-subtrace [tr adr]
  (if (seq? adr)
    (if (empty? adr)
      {}
      (if (empty? (rest adr))
        (dissoc tr (first adr))
        (maybe-set-subtrace
         tr
         (first adr)
         (trace-clear-subtrace (trace-subtrace tr (first adr)) (rest adr)))))
    (dissoc tr adr)))

(defn value-only-trace? [tr]
  (and (trace-has-value? tr) (= (count tr) 1)))

;; Recursively walks the entire state to check it's valid
(defn trace? [s]
  (map? s))

(defn valid-trace? [s]
  (and
   (map? s)
   (every?
    (fn [k] (trace? (get s k)))
    (trace-keys s))))

;; Marco's merge operator (+).  Commutative and idempotent.
;;
;; (trace-merge small large) - when calling, try to make tr1 smaller than tr2,
;; because it will be tr1 that is traversed.

;; Compare states of two values that might or might not be traces.

(defn trace-merge [tr1 tr2]
  (let
    [merged
    (into tr1
        (for [key (trace-keys tr2)]
          [key (if (trace-has-subtrace? tr1 key)
                 (trace-merge (trace-subtrace tr1 key)
                              (trace-subtrace tr2 key))
                 (trace-subtrace tr2 key))]))]
    (if (trace-has-value? merged)
      (do (if (trace-has-value? tr2)
            (assert (= (trace-value tr1) (trace-value tr2))
                    ["incompatible trace values" tr1 tr2]))
          merged)
      (if (trace-has-value? tr2)
        (trace-set-value merged (trace-value tr2))
        merged))))

(defn maybe-subtrace
  [tr adr]
  (or (trace-subtrace tr adr) {}))

(defn merge-subtrace
  [trace addr subtrace]
  (trace-merge trace (maybe-set-subtrace {} addr subtrace)))

(defn addresses-of [tr]
  (letfn [(get-sites [tr]
            ;; returns a seq of traces
            (let [site-list
                  (mapcat (fn [key]
                            (map (fn [site]
                                   (cons key site))
                                 (get-sites (trace-subtrace tr key))))
                          (trace-keys tr))]
              (if (trace-has-value? tr)
                (cons '() site-list)
                site-list)))]
    (let [s (get-sites tr)]
      (doseq [site s]
        (assert (trace-has-value? tr site) ["missing value at" site]))
      s)))


(defn copy-addresses
  [src dst paths]
  "Copy values from a source trace to a destination trace, at the given paths."
  (reduce #(trace-set-value %1 %2 (trace-value src %2))
          dst paths))

(defn partition-trace
  [trace paths]
  (let [path-set (into #{} (map #(if (not (seq? %)) (list %) %) paths))
        addresses (into #{} (addresses-of trace))
        all-addresses (group-by #(clojure.core/contains? path-set %) addresses)]
    [(copy-addresses trace {} (get all-addresses true))
     (copy-addresses trace {} (get all-addresses false))]))

(defn address-contains? [addr elem]
  (some #{elem} addr))

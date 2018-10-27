(ns metaprob.trace
  (:refer-clojure :exclude [get contains? get-in assoc dissoc empty? keys])
  (:require [metaprob.compound :refer :all]))

(defn trace-has-value?
  ([tr] (contains? tr :value))
  ([tr adr] (contains? (get-in tr adr) :value)))

(defn trace-value
  ([tr] (trace-value tr '()))
  ([tr adr]
    (let [subtrace (get-in tr adr)]
      (if (contains? subtrace :value)
        (get subtrace :value)
        (throw (IndexOutOfBoundsException. "No value at this address in trace."))))))

(defn trace-has-subtrace? [tr adr]
  (if (seq? adr)
    (if (empty? adr)
      true
      (if (contains? tr (first adr))
        (recur (get tr (first adr)) (rest adr))))
  (contains? tr adr)))

(defn trace-subtrace [tr adr]
  (get-in tr adr))

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

(defn trace-clear-value
  ([tr] (dissoc tr :value))
  ([tr adr] (trace-set-subtrace tr adr (trace-clear-value (trace-subtrace tr adr)))))

(defn trace-clear-subtrace [tr adr]
  (if (seq? adr)
    (if (empty? adr)
      {}
      (trace-set-subtrace
        tr
        (first adr)
        (trace-clear-subtrace (trace-subtrace tr (first adr)) (rest adr))))
    (dissoc tr adr)))

(defn value-only-trace? [tr]
  (and (trace-has-value? tr) (= (count tr) 1)))

; Recursively walks the entire state to check it's valid
(defn trace? [s]
  (map? s))

(defn valid-trace? [s]
  (and
    (compound? s)
    (= (representation s) :map)
    (every?
      (fn [k] (trace? (get s k)))
      (trace-keys s))))

(defn top-level-environment? [x]
  (instance? clojure.lang.Namespace x))

;; Marco's merge operator (+).  Commutative and idempotent.
;;
;; (trace-merge small large) - when calling, try to make tr1 smaller than tr2,
;; because it will be tr1 that is traversed.

;; Compare states of two values that might or might not be traces.

(defn trace-merge [tr1 tr2]
  (let
    [merged
    (into (unbox tr1)
        (for [key (trace-keys tr2)]
          [key (if (trace-has-subtrace? tr1 key)
                 (trace-merge (trace-subtrace tr1 key)
                              (trace-subtrace tr2 key))
                 (trace-subtrace tr2 key))]))]
    (if (trace-has-value? merged)
      (do (if (trace-has-value? tr2)
            (assert (= (trace-value tr1) (trace-value tr2)) ["incompatible trace values" tr1 tr2]))
          merged)
      (if (trace-has-value? tr2)
        (trace-set-value merged (trace-value tr2))
        merged))))

;; -----------------------------------------------------------------------------
;; Lexicographic ordering on traces.  Used by prettyprinter.

(declare compare-keys)

(defn compare-key-lists [x-keys y-keys]
  (if (empty? x-keys)
    (if (empty? y-keys) 0 -1)
    (if (empty? y-keys)
      1
      (let [q (compare-keys (first x-keys) (first y-keys))]
        (if (= q 0)
          (compare-key-lists (rest x-keys) (rest y-keys))
          q)))))

(defn compare-traces [x y]
  (let [w (if (trace-has-value? x)
            (if (trace-has-value? y)
              (compare-keys (trace-value x) (trace-value y))
              -1)
            (if (trace-has-value? y)
              -1
              0))]
    (if (= w 0)
      (letfn [(lup [x-keys y-keys]
                (if (empty? x-keys)
                  (if (empty? y-keys)
                    0
                    -1)
                  (if (empty? y-keys)
                    1
                    (let [j (compare-keys (first x-keys) (first y-keys))]
                      (if (= j 0)
                        (let [q (compare-keys (trace-value x (first x-keys))
                                              (trace-value y (first y-keys)))]
                          (if (= q 0)
                            (lup (rest x-keys) (rest y-keys))
                            q))
                        j)))))]
        (lup (sort compare-keys (trace-keys x))
             (sort compare-keys (trace-keys y))))
      w)))

(defn compare-keys [x y]
  (cond (number? x)
        ;; Numbers come before everything else
        (if (number? y) (compare x y) -1)
        (number? y) 1

        (string? x)
        (if (string? y) (compare x y) -1)
        (string? y) 1

        (boolean? x)
        (if (boolean? y) (compare x y) -1)
        (boolean? y) 1

        (trace? x)
        (if (trace? y) (compare-traces x y) -1)
        (trace? y) 1

        true (compare x y)))

;; -----------------------------------------------------------------------------
;; Prettyprint

(defn metaprob-newline
  ([] (newline))
  ([out] (.write out "\n")))

(declare pprint-indented)

(defn  ^:private princ [x out]
  (.write out (if (string? x) x (format "%s" x))))

;; Print {...} trace over multiple lines

(defn pprint-general-trace [tr indent out]
  (let [keys (trace-keys tr)]
    ;; If it has a value, clojure-print the value
    (if (trace-has-value? tr)
      (pr (trace-value tr))
      ;; If no value and no subtraces, print as {} (shouldn't happen)
      (if (empty? keys) (princ "{}" out)))

    ;; Now print the subtraces
    (let [indent (str indent "  ")]
      (doseq [key (sort compare-keys keys)]
        (metaprob-newline out)
        (princ indent out)
        (if (string? key)
          (princ key out)
          (pr key))
        (princ ": " out)
        (pprint-indented (trace-subtrace tr key) indent out)))))

;; Indent gives indentation to use on lines after the first.

(defn pprint-indented [x indent out]
  (if (trace? x)
    (let [state (unbox-all x)]
      (cond (empty? state)
            (princ "{}" out)
            true
            (pprint-general-trace state indent out)))
    (pr x))
  (.flush out))

;!!
(defn metaprob-pprint
  ([x] (metaprob-pprint x *out*))
  ([x out]
   (pprint-indented x "" out)
   (metaprob-newline out)
   (.flush out)))
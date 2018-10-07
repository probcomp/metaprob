(ns metaprob.state.steady
  "Simple but slower implementation of state primitives."
  (:require [metaprob.state.common :as common]))

(defn has-value? [state]
  (not (= (get (common/state-to-map state) :value :no-value) :no-value)))

(defn value [state]
  (get (common/state-to-map state) :value))

(defn has-subtrace? [state key]
  (contains? (common/state-to-map state) key))

(defn subtrace [state key]
  (let [val (get (common/state-to-map state) key)]
    (assert (not (= val nil))
            ["no such subtrace" key state])
    val))

(defn state-keys
  "Returns a seq of keys (without the :value marker)"
  [state]
  (common/keys-sans-value (common/state-to-map state)))

(defn subtrace-count [state]
  (count (state-keys state)))

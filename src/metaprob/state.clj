(ns metaprob.state
  "Trace states"
  (:require [metaprob.state.common :as common]
            ;; The `impl` alias controls a choice between a simple but slower
            ;; implementation and an obscure but faster implementation of the
            ;; primitives.
            [metaprob.state.unsteady :as impl]))

(def rest-marker common/rest-marker)

;; Basic trace operations

(def state?         common/state?)
(def empty-state?   common/empty-state?)
(def has-value?     impl/has-value?)
(def value          impl/value)
(def has-subtrace?  impl/has-subtrace?)
(def subtrace       impl/subtrace)
(def state-keys     impl/state-keys)
(def subtrace-count impl/subtrace-count)

;; Converters

(def map-to-state common/map-to-state)
(def state-to-map common/state-to-map)

;; Constructors

(def empty-state common/empty-state)

(defn set-value [state val]
  (map-to-state (assoc (state-to-map state) :value val)))

(defn clear-value [state]
  (map-to-state (dissoc (state-to-map state) :value)))

(defn set-subtrace [state key sub]
  (map-to-state (assoc (state-to-map state) key sub)))

(defn clear-subtrace [state key]
  (map-to-state (dissoc (state-to-map state) key)))

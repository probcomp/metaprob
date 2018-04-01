(ns metaprob.basic-trace
  (:require [clojure.string :as string]))

(def no-value '**no-value**)

(defprotocol ITrace
  "A prefix tree"
  (has-value? [_])
  (value [_] "The value stored for this trie (python: get)")
  (set-value! [_ val] "Store a value at root of this trie (python: set)")
  (clear-value! [_] "Remove any value")

  (has-subtrace? [_ key] "True iff this trie has a direct subtrace under the given key (python: has_key)")
  (subtrace [_ key] "The subtrace of this trie specified by the given key (python: _subtrace, sort of)")
  (set-subtrace! [_ key subtrace] "Splice a subtree as a child of this trie")

  (trace-keys [_])                      ;Return a seq
  (trace-count [_])                     ;Number of subtraces

  (make-locative [_ adr])

  (debug [_]))

(defn basic-trace? [x]
  (satisfies? ITrace x))

; Concrete implementation of the above interface

(declare mutable-trace really-make-locative trie?)

(deftype Trie
    ;; Should probably be one or two refs instead
    [^:volatile-mutable the-value
     ^:volatile-mutable subtraces]    ; hash-map

  ; Implements the ITrace interface
  ITrace

  (has-value? [_]
    (not (= the-value no-value)))
  (value [_]
    (assert (not (= the-value no-value)) "no value")
    the-value)
  (set-value! [_ val]
    (assert (not (= val no-value)) "storing no value")
    (set! the-value val))
  (clear-value! [_]
    ;; Something fishy about this; if doing this causes the trie to become empty
    ;; then shouldn't the trie go away entirely?  Well, we don't have parent 
    ;; pointers, so nothing we can do about this.
    (set! the-value no-value))

  ;; Direct children
  (has-subtrace? [_ key]
    ;; python has_key, trace_has_key
    (contains? subtraces key))
  (subtrace [_ key]
    (get subtraces key))
  (set-subtrace! [_ key sub]
    (assert trie? sub)
    (set! subtraces (assoc subtraces key sub))
    nil)

  (trace-keys [_] 
    (let [ks (keys subtraces)]
      (if (= ks nil)
        '()
        ks)))
  (trace-count [_]
    (count subtraces))

  (make-locative [_ adr]
    (really-make-locative _ adr))

  (debug [_] :trie))

;; Not clear whether this is the most idiomatic / best approach.
(defn trie? [x]
  (instance? Trie x)
  ;; (= (type x) Trie)
  )

(defn mutable-trace
  ([]
   (Trie. no-value (hash-map)))
  ([val]
   (Trie. val (hash-map))))

;; Utilities

(defn trie-from-map
  ([maap] (Trie. no-value maap))
  ([maap val] (Trie. val maap)))

;; ----------------------------------------------------------------------------
;; Locatives!

(defn ^:private subtrace-at [tr adr]
  (if (empty? adr)
    tr
    (let [[head & tail] adr]
      (if (has-subtrace? tr head)
        ;; Snap the link?
        (subtrace-at (subtrace tr head) tail)
        :none))))

(defn ^:private ensure-subtrace-at [tr adr]
  (if (empty? adr)
    tr
    (let [[head & tail] adr]
      ;; Snap the link?
      (ensure-subtrace-at (if (has-subtrace? tr head)
                            (subtrace tr head)
                            (let [novo (mutable-trace)]
                              (set-subtrace! tr head novo)
                              novo))
                          tail))))

(deftype Locative
    [trace adr]
  ITrace

  (has-value? [tr]
    (let [sub (subtrace-at trace adr)]
      (if (= sub :none)
        false
        (has-value? sub))))
  (value [tr]
    (value (subtrace-at trace adr)))
  (set-value! [tr val]
    (set-value! (ensure-subtrace-at trace adr) val))
  (clear-value! [tr]
    (set-value! (ensure-subtrace-at trace adr) no-value))

  (has-subtrace? [tr key]
    (not (= (subtrace-at trace adr) :none)))
  (subtrace [tr key]
    (let [sub (subtrace-at trace adr)]
      (assert (not (= sub :none)))
      sub))
  (set-subtrace! [tr key val]
    (set-subtrace! (ensure-subtrace-at trace adr) key val))

  (trace-keys [tr]
    (let [sub (subtrace-at trace adr)]
      (if (= sub :none)
        '()
        (trace-keys sub))))
  (trace-count [tr]
    (trace-count (subtrace-at trace adr)))

  (make-locative [_ more-adr]
    (really-make-locative trace (concat adr more-adr)))
  )

(defn really-make-locative [tr adr]
  (assert (trie? tr) ["trace in make-locative must be mutable" tr adr])
  (assert (seq? adr) ["address in make-locative must be a seq" tr adr])
  (Locative. tr adr))


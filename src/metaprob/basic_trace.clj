(ns metaprob.basic-trace
  (:require [clojure.string :as string]))

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

  (get-state [_])
  (make-locative [_ adr]))

(defn basic-trace? [x]
  (satisfies? ITrace x))

;; Concrete implementation of the above interface

(declare mutable-trace really-make-locative trie?)

;; Atoms?  Refs + dosync?  Something else?
;; I wish I understand clojure side effects.  Seems like a total hodgepodge

(defn clobber [a x]
  (compare-and-set! a (deref a) x))  ;; AWFUL AWFUL KLUDGE

(deftype Trie
    ;; Should probably be one or two refs instead
    [state-ref]

  ; Implements the ITrace interface
  ITrace

  (has-value? [_]
    (contains? (deref state-ref) :value))
  (value [_]
    (let [probe (get (deref state-ref) :value :not-present)]
      (assert (not (= probe :not-present)) ["no value" _ :value])
      probe))
  (set-value! [_ val]
    (clobber state-ref (assoc (deref state-ref) :value val))
    nil)
  (clear-value! [_]
    ;; Something fishy about this; if doing this causes the trie to become empty
    ;; then shouldn't the trie go away entirely?  Well, we don't have parent 
    ;; pointers, so nothing we can do about this.
    (clobber state-ref (dissoc (deref state-ref) :value))
    nil)

  ;; Direct children
  (has-subtrace? [_ key]
    ;; python has_key, trace_has_key
    (contains? (deref state-ref) key))
  (subtrace [_ key]
    (let [probe (get (deref state-ref) key :not-present)]
      (assert (not (= probe :not-present)) ["no such subtrace" _ key])
      probe))
  (set-subtrace! [_ key sub]
    ;; N.b. sub might an immutable trace.
    (clobber state-ref (assoc (deref state-ref) key sub))
    nil)

  (trace-keys [_] 
    (let [ks (keys (deref state-ref))]
      (if (= ks nil)
        '()
        (remove #{:value} ks))))
  (trace-count [_]
    (let [subs (deref state-ref)]
      (if (contains? subs :value)
        (- (count subs) 1)
        (count subs))))

  (get-state [_] (deref state-ref))

  (make-locative [_ adr]
    (really-make-locative _ adr)))

;; Not clear whether this is the most idiomatic / best approach.
(defn trie? [x]
  (instance? Trie x)
  ;; (= (type x) Trie)
  )

(defn make-trace [maap]
  (Trie. (atom maap)))

(defn mutable-trace
  ([]
   (make-trace (hash-map)))
  ([val]
   (make-trace (assoc (hash-map) :value val))))

;; Utilities

(defn trie-from-map
  ([maap] (make-trace maap))
  ([maap val] (make-trace (assoc maap :value val))))

;; ----------------------------------------------------------------------------
;; Locatives!

(defn ^:private subtrace-at [tr adr]
  (if (empty? adr)
    tr
    (let [head (first adr)
          tail (rest adr)]              ;don't use [head & tail]
      (if (has-subtrace? tr head)
        ;; Snap the link?
        (subtrace-at (subtrace tr head) tail)
        :none))))

(defn ^:private ensure-subtrace-at [tr adr]
  (if (empty? adr)
    tr
    (let [head (first adr)
          tail (rest adr)]              ;don't use [head & tail]
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

  (has-value? [_]
    (let [sub (subtrace-at trace adr)]
      (if (= sub :none)
        false
        (has-value? sub))))
  (value [_]
    (value (subtrace-at trace adr)))
  (set-value! [_ val]
    (set-value! (ensure-subtrace-at trace adr) val))
  (clear-value! [_]
    (clear-value! (ensure-subtrace-at trace adr)))

  (has-subtrace? [_ key]
    (not (= (subtrace-at trace adr) :none)))
  (subtrace [_ key]
    (let [sub (subtrace-at trace adr)]
      (assert (not (= sub :none)))
      sub))
  (set-subtrace! [_ key val]
    (set-subtrace! (ensure-subtrace-at trace adr) key val))

  (trace-keys [_]
    (let [sub (subtrace-at trace adr)]
      (if (= sub :none)
        '()
        (trace-keys sub))))
  (trace-count [_]
    (trace-count (subtrace-at trace adr)))

  (get-state [_] {})                    ;WRONG, FIX

  (make-locative [_ more-adr]
    (really-make-locative trace (concat adr more-adr)))
  )

(defn really-make-locative [tr adr]
  (assert (trie? tr) ["trace in make-locative must be mutable" tr adr])
  (assert (seq? adr) ["address in make-locative must be a seq" tr adr])
  (if (empty? adr)
    tr
    (let [head (first adr)
          tail (rest adr)]              ;don't use [head & tail]
      (if (has-subtrace? tr head)
        (really-make-locative (subtrace tr head) tail)
        (Locative. tr adr)))))


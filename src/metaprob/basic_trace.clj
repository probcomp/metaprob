(ns metaprob.basic-trace)

(defprotocol ITrace
  "A mutable trace"
  (get-state [_])
  (ensure-state [_] "foo")

  (has-value? [_])
  (value [_] "The value stored for this trie (python: get)")
  (set-value! [_ val] "Store a value at root of this trie (python: set)")
  (clear-value! [_] "Remove any value")

  (has-subtrace? [_ key] "True iff this trie has a direct subtrace under the given key (python: has_key)")
  (subtrace [_ key] "The subtrace of this trie specified by the given key (python: _subtrace, sort of)")
  (set-subtrace! [_ key subtrace] "Splice a subtree as a child of this trie")

  (trace-keys [_])                      ;Return a seq
  (trace-count [_]))                     ;Number of subtraces

(defn basic-trace? [x]
  (satisfies? ITrace x))

;; Concrete implementation of the above interface

(declare mutable-trace trie?)

;; This is useful (re mutation in clojure):
;;  https://gist.github.com/beatngu13/44ac58d9b38125528352d3bf835208a5

(deftype Trie
    ;; Should probably be one or two refs instead
    [state-ref]

  ; Implements the ITrace interface
  ITrace

  (get-state [_] (deref state-ref))
  (ensure-state [_] (deref state-ref))

  (has-value? [_]
    (contains? (deref state-ref) :value))
  (value [_]
    (let [probe (get (deref state-ref) :value :not-present)]
      (assert (not (= probe :not-present)) ["no value" _ :value])
      probe))
  (set-value! [_ val]
    (swap! state-ref assoc :value val)
    nil)
  (clear-value! [_]
    ;; Something fishy about this; if doing this causes the trie to become empty
    ;; then shouldn't the trie go away entirely?  Well, we don't have parent 
    ;; pointers, so nothing we can do about this.
    (swap! state-ref dissoc :value val)
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
    (swap! state-ref assoc key sub)
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
        (count subs)))))

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

(declare subtrace-at)

(deftype Locative
    [state-ref trace key]
  ITrace

  (get-state [_]
    (let [probe (deref state-ref)]
      (if (= probe :none)
        (if (has-subtrace? trace key)
          (let [state (subtrace trace key)]
            ;; Cache the subtrace
            (swap! state-ref (fn [none state] state) state)
            state)
          {})
        ;; Use cached value
        probe)))

  (ensure-state [_]
    ;; Ensure that trace has subkey key
    (if (not (has-subtrace? trace key))
      (set-subtrace! trace key (mutable-trace)))
    (get-state _))

  (has-value? [_]
    (let [state (get-state _)]
      (and (basic-trace? state)
           (has-value? state))))
  (value [_]
    (value (get-state _)))
  (set-value! [_ val]
    (set-value! (ensure-state _) val))
  (clear-value! [_]
    (let [state (get-state _)]
      (if (has-value? state)
        (clear-value! state))))

  (has-subtrace? [_ key]
    (let [state (get-state _)]
      (and (basic-trace? state)
           (has-subtrace? (get-state _) key))))
  (subtrace [_ key]
    (subtrace (get-state _) key))
  (set-subtrace! [_ key val]
    (set-subtrace! (ensure-state _) key val))

  (trace-keys [_]
    (trace-keys (get-state _)))
  (trace-count [_]
    (trace-count (get-state _))))

(defn make-locative [tr key]
  (assert (basic-trace? tr) ["expected a basic-trace here" tr key])
  (Locative. (atom :none) tr key))

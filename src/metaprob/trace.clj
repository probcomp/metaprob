(ns metaprob.trace
  (:require [metaprob.environment :refer :all]))

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
  (subtrace-location [_ key] "Returns locative if necessary")

  (has-value-at? [_ addr])
  (value-at [_ addr])
  (set-value-at! [_ addr val])

  (has-subtrace-at? [_ addr])
  (subtrace-at [_ addr] "(python: subtrace_at ??)")
  (set-subtrace-at! [_ addr subtrace] "(python: set_subtrace_at)")
  (subtrace-location-at [_ addr] "returns locative or trie as appropriate")

  (trace-keys [_])
  (trace-count [_])  ;Number of subtries

  (maybe-normalize [_])    ;If there's a trie corresponding to this trace, return it, else nil
  (blaze [_]))

(defn trace? [x]
  (satisfies? ITrace x))

;; Should this throw an error if x is not a trace?  I don't know.

(defn proper-trace [x]
  (and (trace? x)
       (maybe-normalize x)))

(defn normalize
  ([tr]
   (normalize tr nil))
  ([tr info]
   (let [n (maybe-normalize tr)]
     (assert n ["failed to find this node" info])
     n)))

; Concrete implementation of the above interface

(declare ensure-subtrie-at)
(declare new-trace)
(declare trie?)
(declare new-locative trace-from-map)

(defn address? [x] (seqable? x))

(defn metaprob-value? [val]
  (or (number? val)
      (string? val)
      (boolean? val)
      (= val nil)     ; needed?
      (trace? val)                      ;possibly a locative
      (environment? val)
      (and (instance? clojure.lang.IFn val)
           ;; ugh
           (not (seq? val))
           (not (map? val))
           (not (symbol? val)))))

(defn acceptable? [key sub]
  ;; Really stupid type system.  Functions should be stored, and only
  ;; be stored, under the "executable" property.
  (if (trie? sub)
    (if (has-value? sub)
      (let [val (value sub)]
        (if (instance? clojure.lang.IFn val)
          (or (= key "executable")
              (meta val))
          (metaprob-value? val)))
      true)
    false))

(defn reason-unacceptable [key sub]
  (if (trie? sub)
    (if (has-value? sub)
      (let [val (value sub)]
        (if (= key "executable")
          (or (instance? clojure.lang.IFn val)
              ["exec non-IFn" key sub val])
          (if (instance? clojure.lang.IFn val)
            (or (meta val)
                ["non-exec non-meta IFn" key sub val])    ;incl. seq, map
            (if (metaprob-value? val)
              true
              ["not a metaprob value" key sub val]))))
      true)
    ["non-trie" key sub (type sub)]))

(deftype Trie
  [^:volatile-mutable the-value
   ^:volatile-mutable subtries]    ; hash-map

  ; Implements the ITrace interface
  ITrace

  (has-value? [_]
    (not (= the-value no-value)))
  (value [_]
    (assert (not (= the-value no-value)) "no value")
    the-value)
  (set-value! [_ val]
    (assert (not (= val no-value)) "storing no value")
    (assert (metaprob-value? val) ["storing non-metaprob value" val])
    (set! the-value val))
  (clear-value! [_]
    ;; Something fishy about this; if doing this causes the trie to become empty
    ;; then shouldn't the trie go away entirely?  Well, we don't have parent 
    ;; pointers, so nothing we can do about this.
    (set! the-value no-value))

  ;; Direct children
  (has-subtrace? [_ key]
    ;; python has_key, trace_has_key
    (contains? subtries key))
  (subtrace [_ key]
    (let [sub (get subtries key)]
      (assert (trie? sub) ["no such subtrie" key (trace-keys _) the-value])
      sub))
  (set-subtrace! [_ key sub]
    (assert trie? sub)
    (assert (acceptable? key sub)
            ["unacceptable assignment" _ (reason-unacceptable key sub)])
    (set! subtries (assoc subtries key sub))
    nil)

  (subtrace-location [_ key]
    (if (has-subtrace? _ key)
      (subtrace _ key)
      (new-locative _ key)))

  ;; Value at address
  (has-value-at? [_ addr]
    (assert (address? addr) addr)
    (if (empty? addr)
      (has-value? _)
      (let [[head & tail] addr]
        (and (has-subtrace? _ head)
             (has-value-at? (subtrace _ head) tail)))))
  (value-at [_ addr]
    (value (subtrace-at _ addr)))
  (set-value-at! [_ addr val]
    (assert (address? addr) addr)
    (set-value! (ensure-subtrie-at _ addr) val))

  ;; Descendant subtrie at address
  (has-subtrace-at? [_ addr]
    (assert (address? addr) addr)
    (if (empty? addr)
      true
      (let [[head & tail] addr]
        (and (has-subtrace? _ head)
             (has-subtrace-at? (subtrace _ head) tail)))))
  (subtrace-at [_ addr]
    ;; Assert: addr is a list (of symbols?)
    ;; Returns subtrie at address if it's there.
    ;; Fails (or nil) if no such subtree??  Maybe should soft-fail (return nil).
    (assert (address? addr) addr)
    (if (empty? addr)
      _
      (let [[head & tail] addr]
        (subtrace-at (subtrace _ head) tail))))
  (set-subtrace-at! [_ addr subtrie]
    ;; TBD: deal with case where subtrie is a locative
    (assert (trie? subtrie) subtrie)
    (assert (address? addr) addr)
    (let [[head & tail] addr]
      (if (empty? tail)
        (set-subtrace! _ head subtrie)
        (if (has-subtrace? _ head)
          (set-subtrace-at! (subtrace _ head) tail subtrie)
          (let [novo (new-trace)]
            (set-subtrace! _ head novo)
            (set-subtrace-at! novo tail subtrie))))))

  (subtrace-location-at [_ addr]
    (assert (address? addr) addr)
    (letfn [(re [_ addr]
              (if (empty? addr)
                _
                (let [[head & tail] addr]
                  (re (subtrace-location _ head) tail))))]
      (re _ addr)))

  (trace-keys [_] 
    (let [ks (keys subtries)]
      (if (= ks nil)
        '()
        ks)))
  (trace-count [_]
    (count subtries))

  (maybe-normalize [_] _)

  (blaze [_] _))

;; Not clear whether this is the most idiomatic / best approach.
(defn trie? [x]
  (instance? Trie x)
  ;; (= (type x) Trie)
  )

(defn new-trace
  ([]
   (Trie. no-value (hash-map)))
  ([val]
   (assert (metaprob-value? val) ["initial value is non-metaprob" val (environment? val) (type val)])
   (Trie. val (hash-map))))

(defn ensure-subtrie [tr key]
  (assert trie? tr)
  (if (has-subtrace? tr key)
    (subtrace tr key)
    (let [novo (new-trace)]
      (set-subtrace! tr key novo)
      novo)))

(defn ensure-subtrie-at [tr addr]
  ;; Assert: addr is a list (of strings, typically)
  ;; Similar to python subtrace_at or lookup ?
  ;; This should be called immediately before storing a value or subtrace.
  (assert trie? tr)
  (assert (address? addr) addr)
  (if (empty? addr)
    tr
    (let [[head & tail] addr]
      (ensure-subtrie-at (ensure-subtrie tr head)
                         tail))))

(deftype Locative
  [trie-or-locative this-key]

  ; Implements the ITrace interface
  ITrace

  (has-value? [_]
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (has-value? n)
        false)))
  (value [_]
    (value (normalize _ this-key)))
  (set-value! [_ val]
    (set-value! (blaze _) val))
  (clear-value! [_]
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (clear-value! n)
        nil)))

  ;; Direct children
  (has-subtrace? [_ key]
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (has-value? n)
        false)))
  (subtrace [_ key]
    (subtrace (normalize _ this-key) key))
  (set-subtrace! [_ key subtrie]
    (set-subtrace! (blaze _) key subtrie))

  (subtrace-location [_ key]
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (subtrace-location n key)
        (new-locative _ key))))

  ;; Value at address
  (has-value-at? [_ addr]
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (has-value-at? n addr)
        false)))
  (value-at [_ addr]
    (value-at (normalize _ this-key) addr))
  (set-value-at! [_ addr val]
    (set-value-at! (blaze _) addr val))

  ;; Descendant subtrie at address
  (has-subtrace-at? [_ addr]
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (has-subtrace-at? n addr)
        false)))
  (subtrace-at [_ addr]
    (subtrace-at (normalize _ this-key) addr))
  (set-subtrace-at! [_ addr subtrie]
    (set-subtrace-at! (blaze _) addr subtrie))

  (subtrace-location-at [_ addr]
    (letfn [(re [n addr]
              (if (empty? addr)
                n
                (let [[head & tail] addr]
                  (re (if (trie? n)
                        (subtrace-location n head)
                        (new-locative _ head))
                      tail))))]
      (re (or (maybe-normalize _) _) addr)))

  (trace-keys [_] 
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (trace-keys n)
        '())))
  (trace-count [_]
    (let [n (maybe-normalize _)]
      (if (trie? n)
        (trace-count n)
        0)))

  ;; Returns trie or nil
  (maybe-normalize [_]
    (let [n (maybe-normalize trie-or-locative)]
      (if (trie? n)
        (if (has-subtrace? n this-key)
          (subtrace n this-key)
          nil)
        nil)))

  ;; Force the creation of a trie corresponding to this locative.
  (blaze [_]
    (let [tr (blaze trie-or-locative)]    ;Up one level
      (ensure-subtrie tr this-key))))

(defn new-locative [tl key]
  (Locative. tl key))

(defn locative? [x]
  (= (type x) Locative))

;; Utilities

(defn trie-from-map [val maap]
  (doseq [[key sub] (seq maap)]
    (assert (acceptable? key sub)
            ["bad subtrie assignment" (reason-unacceptable key sub)]))
  (Trie. val maap))

; thanks https://stuartsierra.com/2015/06/01/clojure-donts-optional-arguments-with-varargs
(defn trace-from-map
  ([maap]
   (trie-from-map no-value maap))
  ([maap val]
   (assert (metaprob-value? val) ["starting value is a non-metaprob value" val])
   (trie-from-map val maap)))

; Returns a trie whose subtries are the members of the clojure sequence tlist

(defn trace-from-seq
  ([tlist]
   (trace-from-map (zipmap (range (count tlist))
                          tlist)))
  ([tlist val]
   (trace-from-map (zipmap (range (count tlist))
                          tlist)
                  val)))

; Returns a clojure seq of the numbered subtries of the trie tr

(defn subtraces-to-seq [tr]
  (assert trie? tr)
  (for [i (range (trace-count tr))]
    (subtrace tr i)))

; Returns a seq of the values of the numbered subtries of tr

(defn subtrace-values-to-seq [tr]
  (assert trie? tr)
  (for [i (range (trace-count tr))]
    (value (subtrace tr i))))



;; From python trace.py
  ;; def subtrace(self, _key): yield self
  ;; def reify(self): pass
  ;; def dereify(self): pass
  ;; def has(self): return False
  ;; def get(self): assert False, "Cannot get from a NullTrace"
  ;; def set(self, _value): pass
  ;; def clear(self): pass
  ;; def has_key(self, _key): return False
  ;; def update(self, _trace): pass
  ;; def subkeys(self): return []
  ;; def sites(self): return []
  ;; def lookup(self, _addr): return self
  ;; def get_at(self, _key): assert False, "Cannot get from a NullTrace"
  ;; def set_at(self, _key, _val): return self
  ;; def set_subtrace_at(self, _addr, _trace): return self
  ;; def equalSameType(self, other): return self is other
  ;; def asData(self): return (None, [])
  ;; def subtrace_at(self, _keys): yield self

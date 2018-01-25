(ns dontknow.trie)

(def no-value '**no-value**)

(defprotocol ITrace
  "A prefix tree"
  (has-value? [_])
  (value [_] "The value stored for this trie (python: get)")
  (set-value! [_ val] "Store a value at root of this trie (python: set)")

  (has-subtrie? [_ key] "True iff this trie has a direct subtrie under the given key (python: has_key)")
  (subtrie [_ key] "The subtrie of this trie specified by the given key (python: _subtrace, sort of)")
  (set-subtrie! [_ key subtrie] "Splice a subtree as a child of this trie")
  (subtrace [_ key])

  (has-value-at? [_ addr])
  (value-at [_ addr])
  (set-value-at! [_ addr val])

  (has-subtrie-at? [_ addr])
  (subtrie-at [_ addr] "(python: subtrace_at ??)")
  (set-subtrie-at! [_ addr subtrie] "(python: set_subtrace_at)")
  (subtrace-at [_ addr] "returns locative or trie as appropriate")

  (trie-keys [_])
  (trie-count [_])  ;Number of subtries

  (narrow [_])    ;If there's a already trie corresponding to this trace, return it
  (blaze [_]))

(defn trace? [x]
  (satisfies? ITrace x))

; Concrete implementation of the above interface

(declare ensure-subtrie-at)
(declare new-trie)
(declare trie?)
(declare new-locative)

(defn address? [x] (seqable? x))

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
    (set! the-value val))

  ;; Direct children
  (has-subtrie? [_ key]
    ;; python has_key, trace_has_key
    (contains? subtries key))
  (subtrie [_ key]
    (let [sub (get subtries key)]
      (assert (trie? sub) ["no such subtrie" key (trie-keys _)])
      sub))
  (set-subtrie! [_ key subtrie]
    (set! subtries (assoc subtries key subtrie)))

  (subtrace [_ key]
    (if (has-subtrie? _ key)
      (subtrie _ key)
      (new-locative _ key)))

  ;; Value at address
  (has-value-at? [_ addr]
    (assert (address? addr) addr)
    (if (empty? addr)
      (has-value? _)
      (let [[head & tail] addr]
        (and (has-subtrie? _ head)
             (has-value-at? (subtrie _ head) tail)))))
  (value-at [_ addr]
    (value (subtrie-at _ addr)))
  (set-value-at! [_ addr val]
    (assert (address? addr) addr)
    (set-value! (ensure-subtrie-at _ addr) val))

  ;; Descendant subtrie at address
  (has-subtrie-at? [_ addr]
    (assert (address? addr) addr)
    (if (empty? addr)
      true
      (let [[head & tail] addr]
        (and (has-subtrie? _ head)
             (has-subtrie-at? (subtrie _ head) tail)))))
  (subtrie-at [_ addr]
    ;; Assert: addr is a list (of symbols?)
    ;; Returns subtrie at address if it's there.
    ;; Fails (or nil) if no such subtree??  Maybe should soft-fail (return nil).
    (assert (address? addr) addr)
    (if (empty? addr)
      _
      (let [[head & tail] addr]
        (subtrie-at (subtrie _ head) tail))))
  (set-subtrie-at! [_ addr subtrie]
    (assert (address? addr) addr)
    (let [[head & tail] addr]
      (if (empty? tail)
        (set-subtrie! _ head subtrie)
        (if (has-subtrie? _ head)
          (set-subtrie-at! (subtrie _ head) tail subtrie)
          (let [novo (new-trie)]
            (set-subtrie! _ head novo)
            (set-subtrie-at! novo tail subtrie))))))

  (subtrace-at [_ addr]
    (assert (address? addr) addr)
    (if (empty? addr)
      _
      (let [[head & tail] addr]
        (subtrace-at (subtrace _ head) tail))))

  (trie-keys [_] 
    (let [ks (keys subtries)]
      (if (= ks nil)
        '()
        ks)))
  (trie-count [_]
    (count subtries))

  (narrow [_] _)

  (blaze [_] _))


; Not clear whether this is the most idiomatic / best approach.
(defn trie? [x]
  (= (type x) Trie))

(defn new-trie
  ([]
   (Trie. no-value (hash-map)))
  ([val]
   (assert (not (= val no-value)) "no value")
   (Trie. val (hash-map))))

; thanks https://stuartsierra.com/2015/06/01/clojure-donts-optional-arguments-with-varargs
(defn trie-from-map
  ([maap]
   (assert (every? trie? (vals maap)) ["x1" maap])
   (Trie. no-value maap))
  ([maap val]
   (assert (every? trie? (vals maap)) ["xs" maap])
   (assert (not (= val no-value)) "no value")
   (Trie. val maap)))

; Returns a trie whose subtries are the members of the clojure sequence tlist

(defn trie-from-seq
  ([tlist]
   (trie-from-map (zipmap (range (count tlist))
                          tlist)))
  ([tlist val]
   (assert (every? trie? tlist))
   (trie-from-map (zipmap (range (count tlist))
                          tlist)
                  val)))

; Returns a clojure seq of the numbered subtries of the trie tr

(defn subtries-to-seq [tr]
  (assert trie? tr)
  (for [i (range (trie-count tr))]
    (subtrie tr i)))

; Returns a seq of the values of the numbered subtries of tr

(defn subtrie-values-to-seq [tr]
  (assert trie? tr)
  (for [i (range (trie-count tr))]
    (value (subtrie tr i))))

(defn ensure-subtrie [tr key]
  (assert trie? tr)
  (if (has-subtrie? tr key)
    (subtrie tr key)
    (let [novo (new-trie)]
      (set-subtrie! tr key novo)
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
      (if (has-subtrie? tr head)
        (subtrie tr head)
        (ensure-subtrie-at (ensure-subtrie tr head)
                           tail)))))


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

(deftype Locative
  [trie-or-locative this-key]

  ; Implements the ITrace interface
  ITrace

  (has-value? [_]
    (let [n (narrow _)]
      (if (trie? n)
        (has-value? n)
        false)))
  (value [_]
    (let [n (narrow _)]
      (assert (trie? n) "unrealized locatives don't have values")
      (value n)))
  (set-value! [_ val]
    (set-value! (blaze _) val))

  ;; Direct children
  (has-subtrie? [_ key]
    (let [n (narrow _)]
      (if (trie? n)
        (has-value? n)
        false)))
  (subtrie [_ key]
    (let [n (narrow _)]
      (assert (trie? n) "unresolved locatives don't have subtries")
      (subtrie n key)))
  (set-subtrie! [_ key subtrie]
    (set-subtrie! (blaze _) key subtrie))

  (subtrace [_ key]
    (let [n (narrow _)]
      (if (trie? n)
        (subtrace n key)
        (new-locative _ key))))

  ;; Value at address
  (has-value-at? [_ addr]
    (let [n (narrow _)]
      (if (trie? n)
        (has-value-at? n addr)
        false)))
  (value-at [_ addr]
    (let [n (narrow _)]
      (assert (trie? n) "unresolved locatives don't have subtries")
      (value-at n addr)))
  (set-value-at! [_ addr val]
    (set-value-at! (blaze _) addr val))

  ;; Descendant subtrie at address
  (has-subtrie-at? [_ addr]
    (let [n (narrow _)]
      (if (trie? n)
        (has-subtrie-at? n addr)
        false)))
  (subtrie-at [_ addr]
    (let [n (narrow _)]
      (assert (trie? n) "unresolved locatives don't have subtries")
      (subtrie-at _ addr)))
  (set-subtrie-at! [_ addr subtrie]
    (set-subtrie-at! (blaze _) addr subtrie))

  (subtrace-at [_ addr]
    (let [n (narrow _)]
      (if (trie? n)
        (subtrie-at n addr)
        (if (empty? addr)
          _
          (let [[head & tail] addr]
            (new-locative (subtrace-at _ tail) head))))))


  (trie-keys [_] 
    (let [n (narrow _)]
      (if (trie? n)
        (trie-keys n)
        '())))
  (trie-count [_]
    (let [n (narrow _)]
      (if (trie? n)
        (trie-count n)
        0)))

  (narrow [_]
    (if (trie? trie-or-locative)
      (if (has-subtrie? trie-or-locative this-key)
        (subtrie trie-or-locative this-key)
        _)
      (let [n (narrow trie-or-locative)]
        (if (trie? n)
          (new-locative n this-key)
          _))))

  ;; Force the creation of a trie corresponding to this locative.
  (blaze [_]
    (let [tr (blaze trie-or-locative)]    ;Up one level
      (ensure-subtrie tr this-key))))

(defn new-locative [tl key]
  (Locative. tl key))

(defn locative? [x]
  (= (type x) Locative))

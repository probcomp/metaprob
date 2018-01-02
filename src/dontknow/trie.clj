(ns dontknow.trie)

(def no-value '**no-value**)

(defprotocol ITrie
  "A prefix tree"
  (has-value? [_])
  (value [_] "The value stored for this trie (python: get)")
  (set-value! [_ val] "Store a value at root of this trie (python: set)")

  (has-subtrie? [_ key] "True iff this trie has a direct subtrie under the given key (python: has_key)")
  (subtrie [_ key] "The subtrie of this trie specified by the given key (python: _subtrace, sort of)")
  (set-subtrie! [_ key subtrie] "Splice a subtree as a child of this trie")

  (has-value-at? [_ addr])
  (value-at [_ addr])
  (set-value-at! [_ addr val])

  (has-subtrie-at? [_ addr])
  (subtrie-at [_ addr] "(python: subtrace_at ??)")
  (set-subtrie-at! [_ addr subtrie] "(python: set_subtrace_at)")

  (trie-keys [_])
  (trie-count [_]))

; Concrete implementation of the above interface

(declare ensure-subtrie-at)
(declare new-trie)
(declare trie?)

(deftype Trie
  [^:volatile-mutable the-value
   ^:volatile-mutable subtries]    ; hash-map

  ; Implements the ITrie interface
  ITrie

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
      (assert (trie? sub) (list "no such subtrie" key (trie-keys _)))
      sub))
  (set-subtrie! [_ key subtrie]
    (set! subtries (assoc subtries key subtrie)))

  ;; Value at address
  (has-value-at? [_ addr]
    (if (empty? addr)
      (has-value? _)
      (let [[head & tail] addr]
        (and (has-subtrie? _ head)
             (has-value-at? (subtrie _ head) tail)))))
  (value-at [_ addr]
    (value (subtrie-at _ addr)))
  (set-value-at! [_ addr val]
    (set-value! (ensure-subtrie-at _ addr) val))

  ;; Descendant subtrie at address
  (has-subtrie-at? [_ addr]
    (if (empty? addr)
      true
      (let [[head & tail] addr]
        (and (has-subtrie? _ head)
             (has-subtrie-at? (subtrie _ head) tail)))))
  (subtrie-at [_ addr]
    ;; Assert: addr is a list (of symbols?)
    ;; Returns subtrie at address if it's there.
    ;; Fails (or nil) if no such subtree??  Maybe should soft-fail (return nil).
    (if (empty? addr)
      _
      (let [[head & tail] addr]
        (subtrie-at (subtrie _ head) tail))))
  (set-subtrie-at! [_ [head & tail] subtrie]
    (if (empty? tail)
      (set-subtrie! _ head subtrie)
      (if (has-subtrie? _ head)
        (set-subtrie-at! (subtrie _ head) tail subtrie)
        (let [novo (new-trie)]
          (set-subtrie! _ head novo)
          (set-subtrie-at! novo tail subtrie)))))

  (trie-keys [_] 
    (let [ks (keys subtries)]
      (if (= ks nil)
        '()
        ks)))
  (trie-count [_]
    (count subtries)))

; Not clear whether this is the most idiomatic / best approach.
(defn trie? [x]
  (= (type x) Trie))

(defn new-trie
  ([]
   (Trie. no-value (hash-map)))
  ([val]
   (Trie. val (hash-map))))

; thanks https://stuartsierra.com/2015/06/01/clojure-donts-optional-arguments-with-varargs
(defn trie-from-map
  ([hm]
   (assert (every? trie? (for [[k v] hm] v)))
   (Trie. no-value hm))
  ([hm val]
   (assert (every? trie? (for [[k v] hm] v)))
   (Trie. val hm)))

(defn ensure-subtrie-at [_ addr]
    ;; Assert: addr is a list (of symbols?)
    ;; Similar to python subtrace_at or lookup ?
    ;; This should be called immediately before storing a value or subtrace.
    (if (empty? addr)
      _
      (let [[head & tail] addr]
        (if (has-subtrie? _ head)
          (subtrie _ head)
          (let [novo (new-trie)]
            (set-subtrie! _ head novo)
            (ensure-subtrie-at novo tail))))))



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

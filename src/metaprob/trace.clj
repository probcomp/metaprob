;; Trace interface - work in progress

;; Concrete types that are made to follow the trace interface:
;;   basic-trace (trie, locative)
;;   nil
;;   seq
;;   vector
;;   map (with or without :value property)
;;   IFn (sometimes: when the meta has a :trace)

;; It would be nice if immutability were hereditary; can that be made to
;; work?

(ns metaprob.trace
  (:require [clojure.string :as string])
  (:require [metaprob.basic-trace :as mut]))

(def ^:private rest-marker "rest")

;; ----------------------------------------------------------------------------
;; We can turn any clojure IFn into a trace by adding `meta` information with a :trace property.

(declare trace? empty-trace?)

(defn fn-qua-trace [ifn tr]
  (assert (trace? tr))
  (with-meta ifn {:trace tr}))

(defn fn-qua-trace? [x]
  (let [m (meta x)]
    (and (map? m) (contains? m :trace))))

(defn fn-qua-trace-trace [x]
  (get (meta x) :trace))

;; ----------------------------------------------------------------------------
;; Trace types

(defn mutable-trace? [x]
  (or (mut/basic-trace? x)
      (and (fn-qua-trace? x)
           (mutable-trace? (fn-qua-trace-trace x)))))

(defn immutable-trace? [x]
  (or (seq? x)     ;; clojure lists are seqs
      (vector? x)
      (map? x)))

(defn trace? [x]
  (or (mutable-trace? x)
      (immutable-trace? x)))

;; Useable as a subtrace key?

(defn ok-key? [val]
  (or (number? val)
      (string? val)
      (boolean? val)
      (= val nil)      ; needed?
      (immutable-trace? val)))

;; Storable as a value

(def ^:local the-ns-type (type *ns*))
(defn environment? [x] (= (type x) the-ns-type))  ;this is awful

(defn foreign? [x]
  (and (instance? clojure.lang.IFn x)
       (not (symbol? val))))

(defn metaprob-value? [val]
  (or (ok-key? val)
      (mutable-trace? val)                      ;possibly a locative
      (foreign? val)
      (environment? val)))

;; ----------------------------------------------------------------------------

(defn ^:private strip-ifn [x]
  (if (fn-qua-trace? x)
    (strip-ifn (fn-qua-trace-trace x))
    x))

;; Convert a mutable trace to a basic trace for access, or return false
;; if the trace isn't mutable.

(defn ^:private basic-trace [x]
  (let [x (strip-ifn x)]
    (if (mut/basic-trace? x)
      x
      false)))

(declare metaprob-sequence-to-seq purify)
(declare metaprob-first metaprob-rest metaprob-pair?)

;; Convert an address-like thing to an address.

(defn addrify [addr]
  (if (trace? addr)
    (map purify (metaprob-sequence-to-seq addr))
    (list addr)))

;; OK.  Now the generic trace accessors.

(defn ^:private trace-has-value? [tr]
  (let [b (basic-trace tr)]
    (cond b (mut/has-value? b)
          (seq? tr) (not (empty? tr))
          (vector? tr) false
          (map? tr) (if (get tr :value) true false)
          (= tr nil) false
          true (assert false ["expected a trace" tr]))))

;; Special case of trace-get.

(defn ^:private trace-value [tr]
  (let [b (basic-trace tr)]
    (cond b (mut/value b)
          (seq? tr) (metaprob-first tr)     ; *e  
          (vector? tr) (assert false ["vectors do not have trace values" tr])
          (map? tr) (get tr :value)
          (trace? tr) (assert false ["trace has no value" tr])    ;locative?
          true (assert false ["expected a trace" tr]))))

;; This is a special case of lookup for one step of a hereditarily immutable trace.
;; Assumes key has already been purified.

(defn ^:private trace-subtrace [tr key]
  (let [b (basic-trace tr)]
    (cond b (mut/subtrace tr key)
          (seq? tr) (if (= key rest-marker)
                      (rest tr)
                      (assert false ["only subtrace of a seq is rest" tr key]))
          (vector? tr) {:value (nth tr key)}
          (map? tr) (get tr key)
          true (assert false ["expected a trace" tr key]))))

;; Assume for now that addr is a seq or nil

(defn lookup [tr addr]                  ; e[e]
  (let [addr (addrify addr)]             ;converts to seq
    (let [b (strip-ifn tr)]
      (if (mut/basic-trace? b)    ;could be a locative
        (mut/subtrace-location-at b addr)
        (letfn [(re [tr addr]
                  (if (empty? addr)
                    tr
                    (re (trace-subtrace tr (first addr))
                        (rest addr))))]
          (re b addr))))))

;; Returns a seq

(defn trace-keys [tr]
  (let [b (basic-trace tr)]
    (if b
      (mut/trace-keys tr)
      (cond (empty? tr) '()
            (seq? tr) (list rest-marker)
            (vector? tr) (range (count tr))
            (map? tr) (remove #{:value} (keys tr))
            true (assert false)))))

;; Neither VKM nor JAR can remember to write (trace-get x (lookup ...))
;; rather than just (trace-get x ...)

(defn trace-get
  ([tr] (trace-value tr))
  ([tr addr]
   (trace-get (lookup tr addr))))

(defn trace-has?
  ([tr] (trace-has-value? tr))
  ([tr addr]
   (if (= tr nil)
     false                              ;KLUDGE, fix
     (trace-has-value? (lookup tr addr)))))

;; ----------------------------------------------------------------------------
;; Side effects.

(defn ^:private trace-set-value! [tr val]
  (let [b (basic-trace tr)]
    (if b
      (mut/set-value! b val)
      (assert false ["expected a mutable trace" tr val]))))

(defn trace-set
  ([tr val] (trace-set-value! tr val))
  ([tr addr val] (trace-set-value! (lookup tr addr) val)))

(defn trace-clear [tr]
  (let [b (basic-trace tr)]
    (if b
      (mut/clear-value! tr)
      (assert false ["expected a mutable trace" tr]))))

(defn trace-delete
  ([tr] (trace-clear tr))
  ([tr addr] (trace-clear (lookup tr addr))))

(defn trace-set-subtrace-at [tr addr sub]
  (let [b (basic-trace tr)]
    (if b
      (mut/set-subtrace-at! tr (addrify addr) sub)
      (assert false ["expected a mutable trace" tr addr sub]))))

;; ----------------------------------------------------------------------------
;; Trace constructors

(def new-trace mut/mutable-trace)
(def empty-trace mut/mutable-trace)

;; Creates a mutable trace from a map whose values are traces.
;; TBD: Check well-formedness of map.

(defn trace-from-map
  ([maap]
   (mut/trie-from-map mut/no-value maap))
  ([maap val]
   (mut/trie-from-map val maap)))

;; Returns a mutable trace whose subtraces are the members of the clojure sequence tlist
;; Used by: syntax.clj

(defn trace-from-subtrace-seq
  ([tlist]
   (trace-from-map (zipmap (range (count tlist))
                           tlist)))
  ([tlist val]
   (trace-from-map (zipmap (range (count tlist))
                           tlist)
                   val)))

;; ----------------------------------------------------------------------------
;; Metaprob pairs / lists

;; We can interpret some traces as sequences, in either of two ways:
;;   - as linked lists
;;   - as tuples

(defn metaprob-pair? [x]
  (let [b (basic-trace x)]
    (if b
      (and (mut/has-value? b)
           (mut/has-subtrace? b rest-marker)
           (= (mut/trace-count b) 1))
      (and (seq? x) (not (empty? x))))))

(defn diagnose-nonpair [x]
  (let [b (basic-trace x)]
    (if b
      (if (mut/has-value? b)
        (if (mut/has-subtrace? b rest-marker)
          "ok"
          ["no rest marker" (mut/trace-keys x)])
        ["no value" (mut/trace-keys x)])
      (cond (seq? x) "ok"
            (empty? x) ["it's empty" x]
            (vector? x) ["it's a vector, not a pair" x]
            false ["not a trace" x]))))

(defn metaprob-first [mp-list]
  (let [b (basic-trace mp-list)]
    (if b
      (mut/value b)
      (if (seq? mp-list)
        (first mp-list)
        (assert false (diagnose-nonpair mp-list))))))

(defn metaprob-rest [mp-list]
  (let [b (basic-trace mp-list)]
    (if b
      (mut/subtrace b rest-marker)
      (if (seq? mp-list)              ;e.g. an address
        (rest mp-list)
        (assert false (diagnose-nonpair mp-list))))))

(defn pair [thing mp-list]
  (let [b (basic-trace mp-list)]
    (if b    ;; TBD: more stringent checks?
      (trace-from-map {rest-marker mp-list} thing)
      (cons thing mp-list))))

;; empty-trace? - is this trace a metaprob representation of an empty tuple/list?

(defn empty-trace? [x]
  (let [b (basic-trace x)]
    (if b 
      (and (= (mut/trace-count b) 0)
           (not (mut/has-value? b)))
      (empty? x))))

;; Tuple
;; Maybe rename this to just `tuple` instead of `metaprob-tuple` ?

(defn metaprob-tuple? [x]
  (let [b (basic-trace x)]    ; nil if not a trace with either value or subtrace
    (if b
      (let [n (mut/trace-count b)]
        (or (= n 0)
            (and (mut/has-subtrace? b 0)
                 (mut/has-subtrace? b (- n 1)))))
      (vector? x))))

;; metaprob-sequence-to-seq - convert metaprob sequence to clojure seq.
(declare subtrace-values-to-seq)

(defn metaprob-tuple-to-seq [tup]
  (let [x (basic-trace tup)]
    (if x
      (subtrace-values-to-seq x)
      (do (assert (vector? tup))
          tup))))

(defn metaprob-list-to-seq [things]
  (let [b (basic-trace things)]
    (if b
      (letfn [(re [things]
                (if (metaprob-pair? things)
                  (cons (metaprob-first things)
                        (re (metaprob-rest things)))
                  (do (assert (empty-trace? things))
                      '())))]
        (re b))
      (do (assert (seq? things))
          things))))

(defn metaprob-sequence-to-seq [things]
  (let [b (basic-trace things)]
    (if b
      (cond (empty-trace? b) '()
            (metaprob-pair? b)
              (metaprob-list-to-seq b)
            (metaprob-tuple? b)
              (metaprob-tuple-to-seq b))
      (cond (vector? things)
              things
            (seq? things)
              things
            true
              (assert false
                      ["Metaprob-sequence is neither trace, seq, not vector" things])))))

;; length - overrides original prelude (performance + generalization)

(defn length [tr]
  (let [b (basic-trace tr)]
    (if b
      (if (empty-trace? b)
        0
        (if (metaprob-pair? b)
          (letfn [(scan [b]
                    (if (metaprob-pair? b)
                      (+ 1 (scan (metaprob-rest b)))
                      0))]
            (scan b))
          (do (assert (metaprob-tuple? b))
              (mut/trace-count b))))
      (cond (= tr nil) 0
            (seq? tr) (count tr)
            (vector? tr) (count tr)
            true (assert false ["doesn't have a length" tr])))))

;; Returns a clojure seq of the numbered subtries of the trie tr.
;; Used in: to-clojure and related

(defn subtraces-to-seq [tr]
  (assert (mut/basic-trace? tr))
  (for [i (range (mut/trace-count tr))]
    (mut/subtrace tr i)))

;; Returns a clojure seq of the values of the numbered subtries of tr.

(defn ^:private subtrace-values-to-seq [tr]
  (assert (trace? tr))
  (for [i (range (mut/trace-count tr))]
    (mut/value (mut/subtrace tr i))))

;; Convert a trace to be used as a key to a pure clojure value (seq,
;; vector, map) so that hash and = will work on it.
;; TBD: Check for IFn+meta case

(defn purify [x]
  (let [b (basic-trace x)]
    (if b
      (cond (empty-trace? b)
              '()
            (metaprob-pair? b)
              (map purify (metaprob-list-to-seq b)) ;cf. metaprob-value?
            (metaprob-tuple? b)
              (vec (map purify (metaprob-tuple-to-seq b)))
            true
            (let [keys (mut/trace-keys b)
                  maap (into {} (for [key keys] [key (purify (mut/subtrace b key))]))]
              (if (mut/has-value? b)
                (assoc maap :value (purify (mut/value b)))
                maap)))
      x)))


;;    (assert (metaprob-value? val) ["storing non-metaprob value" val])

;;   (assert (metaprob-value? val)
;;           ["initial value is non-metaprob" val (environment? val) (type val)])

;;   (assert (metaprob-value? val) ["starting value is a non-metaprob value" val])

;;    (assert (acceptable? key sub)
;;            ["unacceptable assignment" _ (reason-unacceptable key sub)])

;; trie-from-map
;;  (doseq [[key sub] (seq maap)]
;;    (assert (acceptable? key sub)
;;            ["bad subtrie assignment" (reason-unacceptable key sub)]))

;; Is the combination of key and sub acceptable (sub as the subtrace
;; value of key) in a trace?

(defn acceptable? [key sub]
  ;; Really stupid type system.  Functions should be stored, and only
  ;; be stored, under the "foreign-generate" or "foreign-query" property.
  (if (ok-key? key)
    (if (mut/trie? sub)
      (if (mut/has-value? sub)
        (let [val (mut/value sub)]
          (if (string/starts-with? key "foreign-")
            (or (instance? clojure.lang.IFn val)
                (meta val))
            (metaprob-value? val)))
        true)
      false)
    false))

;; This is for debugging

(defn reason-unacceptable [key sub]
  (if (ok-key? key)
    (if (mut/trie? sub)
      (if (mut/has-value? sub)
        (let [val (mut/value sub)]
          (if (string/starts-with? key "foreign-")
            (if (instance? clojure.lang.IFn val)
              ["acceptable - executable IFn" key sub val]
              (if (meta val)
                ["acceptable - executable meta" key sub val]
                ["not IFn and not meta" key sub val]))
            (if (metaprob-value? val)
              ["acceptable" key sub val]
              ["not a metaprob value" key sub val])))
        ["acceptable - no sub-value" key sub])
      ["subtrie is a non-trie" key sub (type sub)])
    ["not to be used as a key" key sub]))

;; ----------------------------------------------------------------------------

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

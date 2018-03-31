;; Trace interface - work in progress

;; Concrete types that are made to follow the trace interface:
;;   basic-trace (trie, locative)
;;   '()
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
;; Procedures are of four kinds:
;;    compiled / interpreted
;;    generate / infer
;; The compiled+generate variety has two representations:
;;   1. as a clojure 'function' (these are *not* traces), or
;;   2. as a clojure function associated with a trace (these *are* treated 
;;      as traces).
;; The other three kinds have only representation #2.

(defn trace-as-procedure? [x]
  (not (= (get (meta x) :trace :not-found) :not-found)))

(defn foreign-procedure? [x]
  (and (instance? clojure.lang.IFn x)
       (not (seq? x))
       (not (vector? x))
       (not (symbol? x))
       (not (keyword? x))
       (not (trace-as-procedure? x))))

;; We can add properties to a procedure by adding a `meta` table
;; with a :trace property whose value is a trace (which can then 
;; receive those properties).

(declare trace? empty-trace? trace-has? trace-get)

(defn trace-as-procedure [tr ifn]
  (do (assert (instance? clojure.lang.IFn ifn) ifn)
      (assert (trace? tr) tr)
      (with-meta ifn {:trace tr})))

;; For most things, this function returns nil, which is fine.

(defn trace-as-procedure-trace [x]
  (get (meta x) :trace))

;; Strip off ifn, returning trace
;; Private to this file, but used in tests

(defn strip [x]
  (let [tr (get (meta x) :trace :not-found)]
    (if (= tr :not-found)
      x
      tr)))

;; ----------------------------------------------------------------------------
;; Trace types

(defn mutable-trace? [x]
  (mut/basic-trace? (strip x)))

(defn immutable-trace? [x]
  (let [x (strip x)]
    (or (and (seq? x)     ;; clojure lists are seqs
             (not (string? x)))
        (vector? x)
        (map? x))))

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

(def ^:local the-ns-type clojure.lang.Namespace)  ;was (type *ns*)
(defn ^:local top-level-environment? [x] (= (type x) the-ns-type))  ;this is awful

(defn ok-value? [val]
  (or (ok-key? val)
      (trace? val)                      ;possibly a locative
      (top-level-environment? val)
      (foreign-procedure? val)))

;; ----------------------------------------------------------------------------

(declare metaprob-sequence-to-seq purify)
(declare metaprob-first metaprob-rest metaprob-pair?)

;; Convert an address-like thing to an address.
;; An address is either a (pure) seq, or ().

(defn addrify [addr]
  (if (trace? addr)
    (map purify (metaprob-sequence-to-seq addr))
    (list (purify addr))))

;; OK.  Now the generic trace accessors.

(defn ^:private trace-has-value? [tr]
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr) (mut/has-value? tr)
          (seq? tr) (not (empty? tr))
          (vector? tr) false
          (map? tr) (not (= (get tr :value :not-found) :not-found))
          true (assert false ["expected a trace" tr]))))

;; Special case of trace-get.

(defn ^:private trace-value [tr]
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr) (mut/value tr)
          (seq? tr) (first tr)     ; *e  
          (vector? tr) (assert false ["vectors do not have trace values" tr])
          (map? tr) (get tr :value)
          true (assert false ["expected a trace" tr]))))

;; This is a special case of lookup for one step of a hereditarily immutable trace.
;; Assumes key has already been purified.

(defn trace-subtrace [tr key]
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr) (mut/subtrace tr key)
          (seq? tr) (if (= key rest-marker)
                      (rest tr)
                      (assert false ["only subtrace of a seq is rest" tr key]))
          (vector? tr) {:value (nth tr key)}
          (map? tr) (get tr key)
          true (assert false ["expected a trace" tr key]))))

(defn trace-has-subtrace? [tr key]
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr) (mut/has-subtrace? tr key)
          ;; empty seqs don't answer true to seq?
          (seq? tr) (and (= key rest-marker)
                         (not (empty? tr)))
          (vector? tr) (and (integer? key)
                            (>= key 0)
                            (< key (count tr)))
          (map? tr) (not (= (get tr key :not-present) :not-present))
          true (assert false ["expected a trace" tr key]))))

;; Assume for now that addr is a seq or ()

(defn lookup [tr addr]                  ; e[e]
  (let [addr (addrify addr)             ;converts to seq
        tr (strip tr)]
    (if (mut/basic-trace? tr)    ;could be a locative
      (mut/subtrace-location-at tr addr)
      (if (empty? addr)
        tr
        (lookup (trace-subtrace tr (first addr))
                (rest addr))))))

;; Returns a seq (n.b. clojure `keys` can return nil which is not a seq)

(defn trace-keys [tr]
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr) (mut/trace-keys tr)
          (seq? tr) (if (empty? tr)
                      '()
                      (list rest-marker))
          (vector? tr) (range (count tr))
          (map? tr) (let [ks (remove #{:value} (keys tr))]
                      (if (= ks nil)
                        '()
                        ks))
          true (assert false "non-trace"))))

;; Neither VKM nor JAR can remember to write (trace-get (lookup x key))
;; rather than just (trace-get x key).  So allow both

(defn trace-get
  ([tr] (trace-value tr))
  ([tr addr]
   (trace-get (lookup tr addr))))

(defn trace-has?
  ([tr] (trace-has-value? tr))
  ([tr addr]
   (let [addr (addrify addr)]
     (if (empty? addr)
       (trace-has-value? tr)
       (and (trace-has-subtrace? tr (first addr))
            (trace-has? (trace-subtrace tr (first addr))
                        (rest addr)))))))

;; ----------------------------------------------------------------------------
;; Side effects.

(defn ^:private trace-set-value! [tr val]
  (let [tr (strip tr)]
    (if (mut/basic-trace? tr)
      (mut/set-value! tr val)
      (assert false ["expected a mutable trace" tr val]))))

(defn trace-set
  ([tr val] (trace-set-value! tr val))
  ([tr addr val] (trace-set-value! (lookup tr addr) val)))

(defn trace-clear [tr]
  (let [tr (strip tr)]
    (if (mut/basic-trace? tr)
      (mut/clear-value! tr)
      (assert false ["expected a mutable trace" tr]))))

(defn trace-delete
  ([tr] (trace-clear tr))
  ([tr addr] (trace-clear (lookup tr addr))))

(defn trace-set-subtrace-at [tr addr sub]
  (let [tr (strip tr)]
    (if (mut/basic-trace? tr)
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
  (let [x (strip x)]
    (if (mut/basic-trace? x)
      (and (mut/has-value? x)
           (mut/has-subtrace? x rest-marker)
           (= (mut/trace-count x) 1))
      (and (seq? x) (not (empty? x))))))

(defn diagnose-nonpair [x]
  (if (trace-has-value? x)
    (if (trace-has-subtrace? x rest-marker)
      (if (= (count (trace-keys x)) 1)
        "ok"
        ["not a pair because extra keys" (mut/trace-keys x)])
      ["not a pair because no rest marker" (mut/trace-keys x)])
    ["not a pair because no value" x]))

(defn metaprob-first [mp-list]
  (if (metaprob-pair? mp-list)
    (trace-get mp-list)
    (assert false (diagnose-nonpair mp-list))))

(defn metaprob-rest [mp-list]
  (if (metaprob-pair? mp-list)
    (trace-subtrace mp-list rest-marker)
    (assert false (diagnose-nonpair mp-list))))

(defn pair [thing mp-list]
  (let [mp-list (strip mp-list)]
    (if (or (metaprob-pair? mp-list)
            (empty-trace? mp-list))
      (if (mut/basic-trace? mp-list)
        ;; Mutability contagion
        (trace-from-map {rest-marker mp-list} thing)
        ;; Make a clojure seq
        (cons thing mp-list))
      ;; This isn't quite right
      (assert false (diagnose-nonpair mp-list)))))

;; empty-trace? - is this trace a metaprob representation of an empty tuple/list?

(defn empty-trace? [x]
  (let [x (strip x)]
    (if (mut/basic-trace? x)
      (and (= (mut/trace-count x) 0)
           (not (mut/has-value? x)))
      ;; Could be [], {}, or ()
      (and (or (seq? x) (vector? x) (map? x))
           (empty? x)))))

;; Tuple
;; Maybe rename this to just `tuple` instead of `metaprob-tuple` ?

(defn metaprob-tuple? [x]
  (let [x (strip x)]
    (if (mut/basic-trace? x)
      (let [n (mut/trace-count x)]
        (or (= n 0)
            (and (mut/has-subtrace? x 0)
                 (mut/has-subtrace? x (- n 1)))))
      (vector? x))))

;; metaprob-sequence-to-seq - convert metaprob sequence (list or tuple) to clojure seq (list).
(declare subtrace-values-to-seq)

(defn metaprob-tuple-to-seq [tup]
  (if (mut/basic-trace? tup)
    (subtrace-values-to-seq tup)
    (seq tup)))

(defn metaprob-list-to-seq [things]
  (let [things (strip things)]
    (if (mut/basic-trace? things)
      (letfn [(re [things]
                (if (metaprob-pair? things)
                  (cons (metaprob-first things)
                        (re (metaprob-rest things)))
                  (do (assert (empty-trace? things))
                      '())))]
        (re things))
      (do (assert (seq? things))    ;seq = immutable mp list
          things))))

(defn metaprob-sequence-to-seq [things]
  (let [things (strip things)]
    (if (mut/basic-trace? things)
      (cond (empty-trace? things) '()
            (metaprob-pair? things)
              (metaprob-list-to-seq things)
            (metaprob-tuple? things)
              (metaprob-tuple-to-seq things)
            true
              (assert false
                      ["expected a metaprob-sequence" things]))
      (cond (vector? things)
              things
            (seq? things)
              things
            true
              (assert false
                      ["expected a metaprob-sequence" things])))))

;; length - overrides original prelude (performance + generalization)

(defn length [tr]
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr)
          (if (empty-trace? tr)
            0
            (if (metaprob-pair? tr)
              (letfn [(scan [b]
                        (if (metaprob-pair? b)
                          (+ 1 (scan (metaprob-rest b)))
                          0))]
                (scan tr))
              (do (assert (metaprob-tuple? tr))
                  (mut/trace-count tr))))
          (seq? tr) (count tr)
          (vector? tr) (count tr)
          true (assert false ["doesn't have a length" tr]))))

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

(defn metaprob-sequence-to-seq [mp-seq]
  (let [mp-seq (strip mp-seq)]
    (cond (mut/basic-trace? mp-seq)
          (cond (metaprob-pair? mp-seq)
                (cons (metaprob-first mp-seq) (metaprob-sequence-to-seq (metaprob-rest mp-seq)))
                (metaprob-tuple? mp-seq)
                (subtrace-values-to-seq mp-seq)
                true
                (assert false ["not a metaprob-sequence" mp-seq]))
          (or (seq? mp-seq) (vector? mp-seq) (empty? mp-seq))
          mp-seq
          false
          (assert false ["not a metaprob-sequence" mp-seq]))))

;; Convert a trace to be used as a key to a pure clojure value (seq,
;; vector, map) so that hash and = will work on it.
;; TBD: Check for IFn+meta case

(defn purify [x]
  (let [x (strip x)]
    (if (mut/basic-trace? x)
      (cond (empty-trace? x)
              '()
            (metaprob-pair? x)
              (map purify (metaprob-list-to-seq x)) ;cf. ok-value?
            (metaprob-tuple? x)
              (vec (map purify (metaprob-tuple-to-seq x)))
            true
            (let [keys (mut/trace-keys x)
                  maap (into {} (for [key keys] [key (purify (mut/subtrace x key))]))]
              (if (mut/has-value? x)
                (assoc maap :value (purify (mut/value x)))
                maap)))
      x)))


;;    (assert (ok-value? val) ["storing non-metaprob value" val])

;;   (assert (ok-value? val)
;;           ["initial value is non-metaprob" val (top-level-environment? val) (type val)])

;;   (assert (ok-value? val) ["starting value is a non-metaprob value" val])

;;    (assert (acceptable? key sub)
;;            ["unacceptable assignment" _ (reason-unacceptable key sub)])

;; trie-from-map
;;  (doseq [[key sub] (seq maap)]
;;    (assert (acceptable? key sub)
;;            ["bad subtrie assignment" (reason-unacceptable key sub)]))

;; Is the combination of key and sub acceptable (sub as the subtrace
;; value of key) in a trace?

(defn acceptable? [key sub]
  (if (ok-key? key)
    (if (mut/trie? sub)
      (if (mut/has-value? sub)
        (ok-value? (mut/value sub))
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
            (if (ok-value? val)
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

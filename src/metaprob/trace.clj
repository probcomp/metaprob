;; Trace interface - work in progress

;; Concrete types that are made to follow the trace interface:
;;   basic-trace (mutable)
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

(def rest-marker "rest")

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

(defn procedure? [x]
  (and (instance? clojure.lang.IFn x)
       (not (seq? x))
       (not (vector? x))
       (not (symbol? x))
       (not (keyword? x))))

(defn foreign-procedure? [x]
  (and (procedure? x)
       (not (trace-as-procedure? x))))

;; We can add properties to a procedure by adding a `meta` table
;; with a :trace property whose value is a trace (which can then 
;; receive those properties).

(declare trace? empty-trace? trace-has? trace-get empty-trace frozen?
         make-immutable make-mutable)

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
    (if (seq? x)     ;; clojure lists are seqs
      (not (string? x))
      (or (vector? x)
          (map? x)))))

(defn trace? [x]
  (or (mutable-trace? x)
      (immutable-trace? x)))

;; Useable as a subtrace key?

(defn ok-key? [val]
  (or (number? val)
      (string? val)
      (boolean? val)
      (= val nil)))      ; needed?

;; Storable as a value
;; *** How about the keyword :value, used as argument to (trace ...)?

(defn ^:local top-level-environment? [x]
  (instance? clojure.lang.Namespace x))

(defn ok-value? [val]
  (or (ok-key? val)
      (trace? val)
      (keyword? val)
      (top-level-environment? val)
      (procedure? val)))

;; ----------------------------------------------------------------------------

(declare metaprob-sequence-to-seq immutable-trace)
(declare metaprob-first metaprob-rest metaprob-pair?)
(declare trace-has-subtrace?)

;; Coerce an address-like thing to an address.
;; A non-seq is coerced to a singleton seq.
;; An address is a seq whose members are immutable values.

(defn addrify [adr]
  (let [adr (if (trace? adr)
              (metaprob-sequence-to-seq adr)
              (list adr))]
    (assert (every? ok-key? adr) adr)
    adr))

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

;; Does it have a subtrace for a particular key?

(defn trace-has-direct-subtrace? [tr key]
  (assert (ok-key? key))
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

;; Get the subtrace for a particular key

(defn trace-direct-subtrace [tr key]
  (assert (ok-key? key))
  (assert (trace-has-direct-subtrace? tr key) ["no such subtrace" tr key (keys tr)])
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr) (mut/subtrace tr key)
          (seq? tr) (if (= key rest-marker)
                      (rest tr)
                      (assert false ["only subtrace of a seq is rest" tr key]))
          (vector? tr) {:value (nth tr key)}
          (map? tr) (let [probe (get tr key :not-present)]
                      (assert (not (= probe :not-present)) ["no such subtrace" tr key])
                      probe)
          true (assert false ["expected a trace" tr key]))))

;; Generalize trace-has-direct-subtrace to addresses

(defn trace-has-subtrace? [tr adr]
  (let [adr (addrify adr)]
    (if (empty? adr)
      true
      (and (trace-has-direct-subtrace? tr (first adr))
           (trace-has-subtrace? (trace-direct-subtrace tr (first adr)) (rest adr))))))

;; Generalize trace-direct-subtrace to addresses

(defn trace-subtrace [tr adr]           ; e[e]
  (let [adr (addrify adr)]              ;converts to seq
    (if (empty? adr)
      tr
      (trace-subtrace (trace-direct-subtrace tr (first adr))
                      (rest adr)))))

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
          true (assert false ["non-trace" tr]))))

;; Neither VKM nor JAR can remember to write (trace-get (lookup x key))
;; rather than just (trace-get x key).  So allow both

(defn trace-get
  ([tr] (trace-value tr))
  ([tr adr]
   (trace-get (trace-subtrace tr adr))))

(defn ^:private trace-has-value-at? [tr adr]
  (let [adr (addrify adr)]
    (if (empty? adr)
      (trace-has-value? tr)
      (and (trace-has-direct-subtrace? tr (first adr))
           (trace-has? (trace-direct-subtrace tr (first adr))
                       (rest adr))))))

(defn trace-has?
  ([tr] (trace-has-value? tr))
  ([tr adr] (trace-has-value-at? tr adr)))

;; Like trace-subtrace, but creates a locative if not found

(defn lookup [tr adr]                  ; e[e]
  (let [adr (addrify adr)             ;converts to seq
        tr (strip tr)]
    (if (empty? adr)
      tr
      (if (trace-has-direct-subtrace? tr (first adr))
        (lookup (trace-direct-subtrace tr (first adr))
                (rest adr))
        (lookup (mut/make-locative tr (first adr))
                (rest adr))))))

;; ----------------------------------------------------------------------------
;; Side effects.

(defn ^:private trace-set-direct-subtrace! [tr key sub]
  (assert (ok-key? key))
  (assert (trace? sub))
  (let [tr (strip tr)]
    (if (mut/basic-trace? tr)
      (mut/set-subtrace! tr key sub)
      (assert false ["expected a mutable trace" tr key sub]))))

(defn trace-set-subtrace [tr adr sub]
  (let [adr (addrify adr)
        tr (strip tr)]
    (assert (not (empty? adr)) adr)
    (let [[head & tail] adr]
      (if (empty? tail)
        (trace-set-direct-subtrace! tr head sub)
        (let [more (if (trace-has-direct-subtrace? tr head)
                     (trace-direct-subtrace tr head)
                     (let [novo (empty-trace)]
                       (trace-set-direct-subtrace! tr head novo)
                       novo))]
          (trace-set-subtrace more tail sub))))))

(defn ^:private trace-set-value! [tr val]
  (let [tr (strip tr)]
    (if (mut/basic-trace? tr)
      (mut/set-value! tr val)
      (assert false ["expected a mutable trace" tr val]))))

(defn ^:private trace-set-value-at! [tr adr val]
  (if (empty? adr)
    (trace-set-value! tr val)
    (let [key (first adr)
          sub (if (trace-has-direct-subtrace? tr key)
                (trace-direct-subtrace tr key)
                (let [novo (empty-trace)]
                  (trace-set-direct-subtrace! tr key novo)
                  novo))]
      (trace-set-value-at! sub (rest adr) val))))

(defn trace-set
  ([tr val] (trace-set-value! tr val))
  ([tr adr val]
   (trace-set-value-at! tr (addrify adr) val)))

(defn trace-clear [tr]
  (let [tr (strip tr)]
    (if (mut/basic-trace? tr)
      (mut/clear-value! tr)
      (assert false ["expected a mutable trace" tr]))))

(defn trace-delete
  ([tr] (trace-clear tr))
  ([tr adr] (trace-clear (trace-subtrace tr adr))))

;; ----------------------------------------------------------------------------
;; Trace constructors

(def empty-trace mut/mutable-trace)

(defn new-trace [val]
  (assert (ok-value? val))
  (mut/mutable-trace val))

;; Creates a mutable trace from a map whose values are traces.
;; TBD: Check well-formedness of map.
;; TBD: Currently the subtraces all have to be mutable.  Fix.

(def trace-from-map mut/trie-from-map)

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
    (if (trace-has-direct-subtrace? x rest-marker)
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
    (trace-direct-subtrace mp-list rest-marker)
    (assert false (diagnose-nonpair mp-list))))

(defn pair [thing mp-list]
  (assert (ok-value? thing))
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

(defn tuple [& inputs] (vec inputs))

(defn metaprob-tuple? [x]
  (let [x (strip x)]
    (if (mut/basic-trace? x)
      (and (not (mut/has-value? x))
           (let [n (mut/trace-count x)]
             (or (= n 0)
                 (and (mut/has-subtrace? x 0)
                      (mut/has-value? (mut/subtrace x 0))
                      (mut/has-subtrace? x (- n 1))
                      (mut/has-value? (mut/subtrace x (- n 1)))))))
      (vector? x))))                     ;??? (empty? x) maybe

;; metaprob-sequence-to-seq - convert metaprob sequence (list or tuple) to clojure seq (list).
(declare subtrace-values-to-seq)

(defn metaprob-tuple-to-seq [tup]
  (if (mut/basic-trace? tup)
    (subtrace-values-to-seq tup)
    (seq tup)))

(defn metaprob-list-to-seq [things]
  (if (mut/basic-trace? things)
    (letfn [(re [things]
              (if (metaprob-pair? things)
                (cons (metaprob-first things)
                      (re (metaprob-rest things)))
                (do (assert (empty-trace? things) "badly terminated list")
                    '())))]
      (re things))
    (do (assert (seq? things))    ;seq = immutable mp list
        things)))

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

;; Total number of subtraces

(defn trace-count [tr]
  (let [tr (strip tr)]
    (cond (mut/basic-trace? tr) (mut/trace-count tr)
          (empty? tr) 0
          (seq? tr) 1
          (vector? tr) (count tr)
          (map? tr) (if (contains? tr :value)
                      (- (count tr) 1)
                      (count tr))
          true (assert false ["doesn't have a count" tr]))))

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
                  (trace-count tr))))
          (seq? tr) (count tr)
          (vector? tr) (count tr)
          true (assert false ["doesn't have a length" tr]))))

;; Returns a clojure seq of the numbered subtraces of the trace tr.
;; Used in: to-clojure and related

(defn subtraces-to-seq [tr]
  (for [i (range (trace-count tr))] (trace-subtrace tr i)))

;; Returns a clojure seq of the values of the numbered subtraces of tr.
;; (there might be a value, so tr isn't necessarily a vector)

(defn ^:private subtrace-values-to-seq [tr]
  (let [r (range (trace-count tr))]
  (map (fn [i]
         (assert (trace-has-direct-subtrace? tr i)
                 ["missing index" 
                  (if (mutable-trace? tr)
                    (mut/get-state tr)
                    tr)
                  i
                  (trace-count tr)
                  (trace-keys tr)
                  r])
         (assert (trace-has? tr i)
                 ["missing value" 
                  (if (mutable-trace? tr)
                    (mut/get-state tr)
                    tr)
                  i
                  (trace-count tr)
                  (trace-keys tr)
                  r])
         (trace-get tr i))
       r)))

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

;; -----------------------------------------------------------------------------
;; Convert a mutable trace to an immutable trace,
;; nonrecursively.

(defn make-immutable [x]
  (if (mutable-trace? x)
    (let [x (strip x)]
      (if true
        (mut/get-state x)               ;NEW FEATURE
        (cond (empty-trace? x)
                '()                         ;or [] or {} - which?
              (metaprob-pair? x)
                (cons (metaprob-first x)
                      ;; necessary, and only slightly recursive
                      (make-immutable (metaprob-rest x)))
              (metaprob-tuple? x)
                (vec (metaprob-tuple-to-seq x))
              true
                (let [maap (into {}
                                 (for [key (mut/trace-keys x)]
                                   [key (mut/subtrace x key)]))]
                  (if (mut/has-value? x)
                    (assoc maap :value (mut/value x))
                    maap)))))
    x))

;; Convert an immutable trace to a mutable trace,
;; nonrecursively.

(defn make-mutable [x]
  (if (mutable-trace? x)
    x
    (do (assert (immutable-trace? x) "cannot make this mutable")
        (let [result (empty-trace)]
          (doseq [key (trace-keys x)]
            (trace-set-direct-subtrace! result key (trace-direct-subtrace x key)))
          (if (trace-has-value? x)
            ;; should we thaw the value too?
            (trace-set result (trace-get x)))
          result))))

;; Recursive copy

(defn trace-copy [x]
  (if (trace? x)
    (let [x (strip x)]
      (let [keys (mut/trace-keys x)
            result (into {} (for [key keys] [key (trace-copy (mut/subtrace x key))]))]
        (trace-from-map
         (if (mut/has-value? x)
           (assoc result :value (mut/value x))
           result))))
    x))

;; Alexey's version (in python-metaprob) has an broader optimization
;; than the one that's here.  I don't know how important it is.

(defn trace-update [self tr]
  (if (trace-has? tr)
    (trace-set self (trace-get tr)))
  (if (> (trace-count self) 0)          ;Do I have any subtraces?
    (doseq [key (trace-keys tr)]
      (if (trace-has? self key)
        (trace-update (trace-subtrace self key)
                      (trace-subtrace tr key))
        (trace-set-subtrace self key (trace-subtrace tr key))))))


;; DEPRECATED, DO NOT USE.
;; Like make-immutable, but recursive.

(defn freeze [x]
  (if (trace? x)
    (let [x (strip x)]
      (cond (empty-trace? x)
            '()
            (metaprob-pair? x)
            (map freeze (metaprob-list-to-seq x)) ;cf. ok-value?
            (metaprob-tuple? x)
            (vec (map freeze (metaprob-tuple-to-seq x)))
            true
            (let [keys (trace-keys x)
                  result (into {} (for [key keys] [key (freeze (trace-subtrace x key))]))]
              (if (trace-has-value? x)
                (assoc result :value (freeze (trace-get x)))
                result))))
    x))

;; DEPRECATED, DO NOT USE.

(defn frozen? [x]
  (if (trace? x)
    (if (mutable-trace? x)
      false
      (and (or (not (trace-has? x))
               (frozen? (trace-get x)))
           (every? (fn [key] (frozen? (trace-direct-subtrace x key)))
                   (trace-keys x))))
    true))

;; Convert an immutable trace to a mutable trace

(defn thaw [x]
  (if (immutable-trace? x)
    (let [tr (empty-trace)]
      (if (trace-has-value? x)
        ;; should we thaw the value too?
        (trace-set tr (trace-get x)))
      (doseq [key (trace-keys x)]
        (trace-set-direct-subtrace! tr key (thaw (trace-direct-subtrace tr key))))
      tr)
    x))

;; -----------------------------------------------------------------------------
;; Immutable trace construction feature
;; Cf. book chapter mss figure 7

(defn ^:private splice? [x]
  (and (map? x) (contains? x :splice)))

(defn kv-pairs-to-trace [kvps]
  (if (empty? kvps)
    {}
    (do (assert (not (empty? (rest kvps))) "odd number of args to trace")
        (let [key (first kvps)
              val (first (rest kvps))
              more (kv-pairs-to-trace (rest (rest kvps)))]
          (if (= key :value)
            (assoc more key val)
            (if (splice? val)
              (assoc more key (get val :splice))
              (assoc more key {:value val})))))))

;; (trace :value 1, "z" 2, "a" (** subtrace), "c" (** (trace "d" 8)))

(defn immutable-trace [& key-value-pairs]
  (kv-pairs-to-trace key-value-pairs))

(defn ** [tr]
  (assert (trace? tr) "**: expected a trace")
  {:splice tr})

(defn trace [& key-value-pairs]
  (make-mutable (kv-pairs-to-trace key-value-pairs)))

;;
;;   (assert (ok-value? val)
;;           ["initial value is non-metaprob" val (top-level-environment? val) (type val)])

;;   (assert (ok-value? val) ["starting value is a non-metaprob value" val])

;;    (assert (acceptable? key sub)
;;            ["unacceptable assignment" _ (reason-unacceptable key sub)])

;; trace-from-map
;;  (doseq [[key sub] (seq maap)]
;;    (assert (acceptable? key sub)
;;            ["bad subtrace assignment" (reason-unacceptable key sub)]))

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

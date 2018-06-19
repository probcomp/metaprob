(ns metaprob.trace
  (:require [metaprob.state :as state]))

;; ----------------------------------------------------------------------------
;; Types

(defn cell? [x]
  (instance? clojure.lang.Atom x))

(defn make-cell [x] (atom x))

(defn trace? [tr]
  (or (get (meta tr) :trace)
      (cell? tr)
      (state/state? tr)))

(defn immutable-trace? [x]
  (or (state/state? x)
      (and (meta x) (immutable-trace? (get (meta x) :trace)))))

(defn mutable-trace? [x]
  (or (cell? x)
      (and (meta x) (mutable-trace? (get (meta x) :trace)))))

;; Clojure function ~= Metaprob procedure

(defn procedure? [x]
  (and (instance? clojure.lang.IFn x)
       (not (seq? x))
       (not (vector? x))
       (not (symbol? x))
       (not (keyword? x))))

;; Metaprob top-level variable environment = clojure namespace

(defn ^:local top-level-environment? [x]
  (instance? clojure.lang.Namespace x))

;; Is a clojure value useable as a Metaprob subtrace key?

(defn ok-key? [val]
  (or (number? val)
      (string? val)
      (boolean? val)
      (= val nil)))      ; needed?

;; Is a clojure value storeable in a Metaprob  trace?

(defn ok-value? [val]
  (or (ok-key? val)
      (trace? val)
      (keyword? val)
      (top-level-environment? val)
      (procedure? val)))

;; ----------------------------------------------------------------------------

(declare trace-deref)

;; Coerce trace to something we can access (its state)

(defn trace-state [tr]
  (let [met (get (meta tr) :trace)]
    (if met
      (trace-state met)
      (if (cell? tr)
        (trace-state (trace-deref tr))
        (if (state/state? tr)
          tr
          (assert (state/state? tr) ["trace-state wta" tr]))))))

;; Coerce trace to something we can update (an cell whose contents is
;; a state)

(defn trace-cell [tr]
  (let [met (get (meta tr) :trace)]
    (if met
      (trace-cell met)
      (if (cell? tr)
        (let [d (trace-deref tr)]
          (if (state/state? d)
            tr
            (trace-cell d)))
        (assert (state/state? tr)
                ["expected a mutable trace" tr])))))

;; ----------------------------------------------------------------------------
;; Fetch and store the state of an cell.
;; Need to deal with locatives; a bit of a kludge.
;; Hoping that locatives will just go away pretty soon.

(declare trace-set-direct-subtrace!
         trace-has-direct-subtrace?
         trace-direct-subtrace)

(defn trace-deref [tr]    ; tr is an cell
  ;; It's not documented, but swap! returns the value stored.
  (swap! tr (fn [contents]
              (if (map? contents)
                (let [parent (get contents :parent-trace)]
                  ;; We were supposed to be the key subnode of parent,
                  ;; but somebody else got there first.
                  (if parent
                    (let [key (get contents :key)]
                      (if (trace-has-direct-subtrace? parent key)
                        ;; It's ready now - forward this cell
                        ;; to the already-created cell.
                        ;; N.b. we might replace cell contents with an atom,
                        ;;  ergo atom->atom->state.
                        (trace-direct-subtrace parent key)
                        contents))
                    contents))
                contents))))

;; Utility for setting the state of a mutable trace, snapping locative
;; links as needed.

(defn ^:private trace-swap! [tr swapper] ;tr is an cell
  (let [tr (trace-cell tr)]
    (swap! tr
           (fn [state]
             (if (map? state)
               (let [parent (get state :parent-trace)]
                 (if parent
                   (let [key (get state :key)]
                     ;; Might be better if this were transactional...
                     (trace-set-direct-subtrace! parent key tr)
                     (swapper {}))
                   (swapper state)))
               (swapper state))))))

(defn make-locative [parent key]
  (make-cell {:parent-trace parent :key key}))


;; ----------------------------------------------------------------------------
;; Procedures are of four kinds:
;;    compiled / interpreted
;;    generative / inference
;; The compiled+generative variety has two representations:
;;   1. as a clojure 'function' (these are *not* traces), or
;;   2. as a clojure function associated with a trace (these *are* treated 
;;      as traces).
;; The other three kinds have only representation #2.

(defn trace-as-procedure? [x]
  (get (meta x) :trace))

(defn trace-as-procedure-trace [x]
  (get (meta x) :trace))

(defn foreign-procedure? [x]
  (and (procedure? x)
       (not (trace-as-procedure? x))))

(defn trace-as-procedure [tr ifn]
  (do (assert (instance? clojure.lang.IFn ifn) ifn)
      (assert (trace? tr) tr)
      (with-meta ifn {:trace tr})))

;; ----------------------------------------------------------------------------
;; Subtrace

(defn ^:private trace-has-value? [tr]
  (state/has-value? (trace-state tr)))

(defn ^:private trace-value [tr]
  (state/value (trace-state tr)))

(defn ^:private trace-has-direct-subtrace? [tr key]
  (state/has-subtrace? (trace-state tr) key))

(defn ^:private trace-direct-subtrace [tr key]
  (state/subtrace (trace-state tr) key))

(defn trace-keys [tr]
  (state/state-keys (trace-state tr)))

(defn trace-count [tr]
  (state/subtrace-count (trace-state tr)))

(defn trace-has-subtrace? [tr adr]
  (if (seq? adr)
    (loop [tr tr adr adr]
      (if (empty? adr)
        true
        (if (trace-has-direct-subtrace? tr (first adr))
          (recur (trace-direct-subtrace tr (first adr))
                 (rest adr))
          false)))
    (trace-has-direct-subtrace? tr adr)))

(defn trace-subtrace [tr adr]
  (if (seq? adr)
    (loop [tr tr adr adr]
      (if (empty? adr)
        tr
        (recur (trace-direct-subtrace tr (first adr))
               (rest adr))))
    (trace-direct-subtrace tr adr)))
    
(defn trace-has?
  ([tr] (trace-has-value? tr))
  ([tr adr]
   (if (seq? adr)
     (loop [tr tr adr adr]
       (if (empty? adr)
         (trace-has-value? tr)
         (if (trace-has-direct-subtrace? tr (first adr))
           (recur (trace-direct-subtrace tr (first adr)) (rest adr))
           false)))
     (and (trace-has-direct-subtrace? tr adr)
          (trace-has? (trace-direct-subtrace tr adr))))))

;; Neither VKM nor JAR can remember to write (trace-get (lookup x key))
;; rather than just (trace-get x key).  So allow both

(defn trace-get
  ([tr] (trace-value tr))
  ([tr adr]
   (trace-value (trace-subtrace tr adr))))

;; For output trace

(defn lookup [tr adr]                  ; e[e]
  (if (seq? adr)
    (loop [tr tr adr adr]
      (if (empty? adr)
        tr
        (if (trace-has-direct-subtrace? tr (first adr))
          (recur (trace-direct-subtrace tr (first adr))
                 (rest adr))
          (recur (make-locative tr (first adr))
                 (rest adr)))))
    (if (trace-has-direct-subtrace? tr adr)
      (trace-direct-subtrace tr adr)
      (make-locative tr adr))))

;; ----------------------------------------------------------------------------
;; Side effects.

(defn make-mutable-trace [initial-state]
  (let [state (if (map? initial-state)
                (state/map-to-state initial-state)
                initial-state)]
    (assert (state/state? state))
    (make-cell state)))

(defn ^:private trace-set-direct-subtrace! [tr key sub]
  (trace-swap! tr
               (fn [state]
                 (state/set-subtrace state key sub))))

(defn trace-set-subtrace! [tr adr sub]
  (assert (trace? sub))
  (if (seq? adr)
    (loop [tr tr adr adr]
      (let [[head & tail] adr]
        (if (empty? tail)
          (trace-set-direct-subtrace! tr head sub)
          (let [more (if (trace-has-direct-subtrace? tr head)
                       (trace-direct-subtrace tr head)
                       (let [novo (make-mutable-trace {})]
                         (trace-set-direct-subtrace! tr head novo)
                         novo))]
            (recur more tail)))))
    (trace-set-direct-subtrace! tr adr sub)))

(def trace-set-subtrace trace-set-subtrace!) ;Name undecided... 
;; ^ this name should be used for the non-mutating version.

(defn ^:private trace-set-value! [tr val]
  (assert (ok-value? val))
  (trace-swap! tr
               (fn [state]
                 (state/set-value state val))))

(defn ^:private trace-set-value-at! [tr adr val] ;cf. trace-get
  (let [adr (if (seq? adr) adr (list adr))]
    (loop [tr tr adr adr]
      (if (empty? adr)
        (trace-set-value! tr val)
        (let [[head & tail] adr]
          (let [more (if (trace-has-direct-subtrace? tr head)
                       (trace-direct-subtrace tr head)
                       (let [novo (make-mutable-trace {})]
                         (trace-set-direct-subtrace! tr head novo)
                         novo))]
            (recur more tail)))))))

(defn trace-set!
  ([tr val] (trace-set-value! tr val))
  ([tr adr val]
   (trace-set-value-at! tr adr val)))

(def trace-set trace-set!)

(defn trace-clear! [tr]
  (trace-swap! tr (fn [state] (state/clear-value state))))

(def trace-clear trace-clear!)

(defn trace-delete!
  ([tr] (trace-clear tr))
  ([tr adr] (trace-clear (trace-subtrace tr adr))))

(def trace-delete trace-delete!)

(defn empty-trace? [x]
  (empty? (trace-state x)))

;; ----------------------------------------------------------------------------
;; Trace constructors

(defn empty-trace
  ([] (make-mutable-trace {}))
  ([val]
   (assert (ok-value? val))
   (make-mutable-trace {:value val})))

(def new-trace empty-trace)

;; Creates a mutable trace from a map whose values are traces.
;; TBD: Check well-formedness of map.
;; TBD: Currently the subtraces all have to be mutable.  Fix.

(defn trace-from-map
  ([maap] (make-mutable-trace maap))
  ([maap val] (make-mutable-trace (assoc maap :value val))))

(defn trace-from-subtrace-seq
  ([tlist]
   (make-mutable-trace (vec tlist)))
  ([tlist val]
   (trace-from-map (zipmap (range (count tlist))
                           tlist)
                   val)))

;; ----------------------------------------------------------------------------
;; Metaprob pairs / lists

(defn empty-list [] '())

(defn pair-as-map? [state]
  (and (map? state)
       (= (count state) 2)
       (contains? state :value)
       (contains? state state/rest-marker)))

(defn metaprob-pair? [x]
  (and (trace? x)
       (let [state (trace-state x)]
         (if (seq? state)
           (not (empty? state))
           (pair-as-map? state)))))

(defn metaprob-first [mp-list]
  (assert (metaprob-pair? mp-list))
  (state/value (trace-state mp-list)))

(defn metaprob-rest [mp-list]
  (assert (metaprob-pair? mp-list))
  (state/subtrace (trace-state mp-list) state/rest-marker))

(defn pair [thing mp-list]              ;NEEDS ERROR CHECKING
  (assert (ok-value? thing) ["wta" thing])
  (assert (or (empty-trace? mp-list)
              (metaprob-pair? mp-list))
          ["wanted empty or pair" mp-list])
  (if (seq? mp-list)
    (cons thing mp-list)                ;Keep it immutable
    (make-mutable-trace {:value thing state/rest-marker mp-list})))

(defn metaprob-list-to-seq [things]
  ;; TBD: deal with non-seq lists?
  (let [state (trace-state things)]
    (assert (seq? state))
    state))

;; ----------------------------------------------------------------------------
;; Metaprob tuples (implemented as Clojure vectors)

(defn tuple [& inputs]
  (vec (map (fn [val]
              (assert (ok-value? val))
              val)
            inputs)))

(defn metaprob-tuple? [x]
  (vector? (trace-state x)))

(declare subtrace-values-to-seq)

(defn metaprob-tuple-to-seq [tup]
  (let [state (trace-state tup)]
    (assert (vector? state))
    (seq state)))

;; ----------------------------------------------------------------------------
;; Metaprob sequences (lists or tuples)

;; Length of list or tuple

(defn length [tr]
  (let [state (trace-state tr)]
    (cond (seq? state) (count state)
          (vector? state) (count state)
          (map? state) (if (pair-as-map? state)
                         (+ 1 (length (get state state/rest-marker)))
                         (assert false ["not a sequence" state]))
          true (assert false ["length wta" tr state]))))

;; metaprob-sequence-to-seq - convert metaprob sequence (list or tuple) to clojure seq.

(defn metaprob-sequence-to-seq [things]
  (let [state (trace-state things)]
    (cond (seq? state) state
          (vector? state) (seq state)
          (pair-as-map? state) (cons (get state :value)
                                     (metaprob-sequence-to-seq (get state state/rest-marker)))
          true (assert false ["metaprob-sequence-to-seq wta" things state]))))

;; ----------------------------------------------------------------------------
;; Coerce to clojure seq ...

;; Returns a clojure seq of the numbered subtraces of the trace tr.
;; Used in: to-clojure and related

(defn subtraces-to-seq [tr]
  (for [i (range (trace-count tr))] (trace-subtrace tr i)))

;; Returns a clojure seq of the values of the numbered subtraces of tr.
;; (there might be a value, so tr isn't necessarily a vector)

(defn ^:private subtrace-values-to-seq [tr]
  (map (fn [i]
         (assert (trace-has-direct-subtrace? tr i)
                 ["missing index" 
                  (trace-state tr)
                  i
                  (trace-count tr)
                  (trace-keys tr)])
         (assert (trace-has? tr i)
                 ["missing value" 
                  (trace-state tr)
                  i
                  (trace-count tr)
                  (trace-keys tr)])
         (trace-get tr i))
       (trace-count tr)))

;; -----------------------------------------------------------------------------
;; Convert a mutable trace to an immutable trace,
;; nonrecursively.

(defn make-immutable [x]
  (trace-state x))

(defn make-mutable [x]
  (if (mutable-trace? x)
    x
    (make-mutable-trace (trace-state x))))

;; Recursive copy, mutable result... hmm...
;; see earthquake example...

(defn trace-copy [x]
  (if (trace? x)
    (let [keys (trace-keys x)
          result (into {} (for [key keys] [key (trace-copy (trace-get x key))]))]
      (if (trace-has-value? x)
        (make-mutable-trace (state/set-value result (trace-get x)))
        (make-mutable-trace result)))
    x))

;; Alexey's version (in python-metaprob) has a broader optimization
;; than the one that's here.  I don't know how important it is.

(defn trace-update [self tr]
  (if (trace-has? tr)
    (trace-set-value! self (trace-get tr)))
  (if (> (trace-count self) 0)          ;Do I have any subtraces?
    (doseq [key (trace-keys tr)]
      (if (trace-has? self key)
        (trace-update (trace-subtrace self key)
                      (trace-subtrace tr key))
        (trace-set-subtrace! self key (trace-subtrace tr key))))))

;; DEPRECATED, DO NOT USE.
;; well, foo. it's used once, in builtin-impl somewhere

(defn frozen? [x]
  (if (trace? x)
    (if (mutable-trace? x)
      false
      (and (or (not (trace-has? x))
               (frozen? (trace-get x)))
           (every? (fn [key] (frozen? (trace-direct-subtrace x key)))
                   (trace-keys x))))
    true))

;; -----------------------------------------------------------------------------
;; Immutable trace construction feature
;; Cf. book chapter mss figure 7

;; (trace :value 1, "z" 2, "a" (** subtrace), "c" (** (trace "d" 8)))

(defn ^:private splice? [x]
  (and (map? x) (contains? x :splice)))

(defn kv-pairs-to-map [kvps]
  (if (empty? kvps)
    {}
    (do (assert (not (empty? (rest kvps))) "odd number of args to trace")
        (let [key (first kvps)
              val (first (rest kvps))
              more (kv-pairs-to-map (rest (rest kvps)))]
          (if (= key :value)
            (do (assert (ok-value? val))
                (assoc more key val))
            (do (assert (ok-key? key))
                (if (splice? val)
                  (assoc more key (get val :splice))
                  (do (assert (ok-value? val))
                      (assoc more key {:value val})))))))))

(defn ** [tr]
  (assert (trace? tr) "**: expected a trace")
  {:splice tr})

(defn immutable-trace [& key-value-pairs]
  (state/map-to-state
   (kv-pairs-to-map key-value-pairs)))

(defn trace [& key-value-pairs]
  (make-mutable (state/map-to-state
                 (kv-pairs-to-map key-value-pairs))))

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


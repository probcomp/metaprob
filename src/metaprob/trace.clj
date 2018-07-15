(ns metaprob.trace
  (:require [metaprob.state :as state]))

;; There are three kinds of generic traces:
;;   1. trace-state (immutable)
;;   2. cell (containing a trace)
;;   3. clojure object with (meta ...) having :trace property
;;        (usually a procedure)

;; ----------------------------------------------------------------------------
;; Metaprob type predicates

;; Clojure function ~= Metaprob procedure

(defn proper-function? [x]
  (and (instance? clojure.lang.IFn x)
       (not (seq? x))
       (not (vector? x))
       (not (map? x))
       (not (symbol? x))
       (not (keyword? x))))

;; Metaprob top-level variable environment = clojure namespace

(defn top-level-environment? [x]
  (instance? clojure.lang.Namespace x))

;; Is a clojure value useable as a Metaprob subtrace key?

(defn ok-key? [val]
  (or (number? val)
      (string? val)
      (boolean? val)
      (= val nil)))      ; needed?

;; Generic traces and their subtypes

(defn make-cell [x] (atom x))

(defn cell? [x]
  (instance? clojure.lang.Atom x))

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

;; Is a clojure value storeable in a Metaprob trace?

(defn ok-value? [val]
  (or (ok-key? val)
      (trace? val)
      (keyword? val)
      (top-level-environment? val)
      (proper-function? val)))

;; ----------------------------------------------------------------------------
;; Utilities implementing mutable traces

(declare trace-set-direct-subtrace!
         trace-has-direct-subtrace?
         trace-direct-subtrace
         snap-link-if-any!)

;; Coerce trace to something we can use (its state)

(defn trace-state [tr]
  (let [met (get (meta tr) :trace)]
    (if met
      (trace-state met)
      (if (cell? tr)
        (let [contents (snap-link-if-any! tr)]
          (if (and (map? contents)
                   (contains? contents :parent-trace))
            (do (print ["failing locative" (get contents :key)])
                (state/empty-state))
            (trace-state contents)))
        (if (state/state? tr)
          tr
          (assert false ["trace-state wta" tr]))))))

;; Returns new contents of cell

(defn snap-link-if-any! [cell]
  (swap! cell
         (fn [d]
           (if (and (map? d)
                    (contains? d :parent-trace))
             (let [parent (get d :parent-trace) key (get d :key)]
               (if (trace-has-direct-subtrace? parent key)
                 (do (print ["forwarding" key])
                     ;; I think maybe this case does not occur:
                     (trace-direct-subtrace parent key))
                 (do (trace-set-direct-subtrace! parent key cell)
                     (state/empty-state))))
             ;; A proper trace (cell, meta or state) - leave unchanged
             d))))

;; Coerce trace to something we can update (an cell whose contents is
;; a state).
;; Cells are used for two purposes: mutable traces; and locatives.
;; An assumption is that if we are trying to get the cell, it's because
;; we're planning to update it.

(defn trace-cell [tr]
  (let [met (get (meta tr) :trace)]
    (if met
      (trace-cell met)
      (if (cell? tr)
        (let [d (snap-link-if-any! tr)]
          (if (state/state? d)
            tr                          ;Success - cell containing a state
            (trace-cell d)))
        (assert false
                ["expected a mutable trace" tr])))))

;; Create a cell referring to future subtrace of a trace

(defn ^:private make-locative [parent key]
  (assert (ok-key? key) key)
  (if (trace-has-direct-subtrace? parent key)
    (let [sub (trace-direct-subtrace parent key)]
      (assert (mutable-trace? sub) [sub key])
      sub)
    (make-cell {:parent-trace parent :key key})))

;; Fetch and store the state of an cell.
;; Need to deal with locatives; a bit of a kludge.
;; Hoping that locatives will just go away pretty soon.

;; Utility for setting the state of a mutable trace.
;; It's not documented, but swap! returns the value stored.

;; Swapper maps states to states (not traces to traces).
;; If locative, then until now, the parent has not pointed to the child.
;; But now that the child is legitimate, it's OK to link it in.

(defn ^:private trace-swap! [tr swapper] ;tr is an cell
  (let [cell (trace-cell tr)]
    (swap! cell swapper)
    (assert (state/state? (deref cell))
            ["stored non-state into cell" (deref cell)])
    "value of trace-swap!"))

;; -----------------------------------------------------------------------------
;; Utilities implementing the trace-as-procedure trace type

;; Procedures are of four kinds:
;;    compiled / interpreted
;;    generative / inference
;; The compiled+generative variety has two representations:
;;   1. as a clojure 'function' (these are *not* traces), or
;;   2. as a clojure function associated with a trace (these *are* treated 
;;      as traces).
;; The other three kinds have only representation #2.

(defn trace-as-procedure? [x]
  (if (get (meta x) :trace) true false))

(defn trace-as-procedure-trace [x]
  (get (meta x) :trace))

;; Formerly (and (not (trace-as-procedure? x))) ...
;;  see infer-apply

(defn foreign-procedure? [x]
  (proper-function? x))

(defn trace-as-procedure [tr ifn]
  (do (assert (instance? clojure.lang.IFn ifn) ifn)
      (assert (trace? tr) tr)
      (with-meta ifn {:trace tr})))

;; I don't think this is used, but it's documentational

(declare trace-get trace-has?)

(defn procedure? [val]
  (or (proper-function? val)
      (and (trace? val)
           (trace-has? val)
           (= (trace-get val) "prob prog"))))

;; ----------------------------------------------------------------------------
;; Read operations on generic traces

(defn ^:private trace-has-value? [tr]
  (state/has-value? (trace-state tr)))

(defn ^:private trace-value [tr]
  (state/value (trace-state tr)))

(defn trace-keys [tr]
  (state/state-keys (trace-state tr)))

(defn trace-count [tr]
  (state/subtrace-count (trace-state tr)))

;; Subtrace

(defn ^:private trace-has-direct-subtrace? [tr key]
  (state/has-subtrace? (trace-state tr) key))

(defn ^:private trace-direct-subtrace [tr key]
  (state/subtrace (trace-state tr) key))

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
    
(defn trace-has?                        ;Does it have a value?
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

;; Formerly called 'lookup'

(defn trace-get
  ([tr] (trace-value tr))
  ([tr adr]
   (trace-value (trace-subtrace tr adr))))

(defn empty-trace? [x]
  (and (trace? x)
       (empty? (trace-state x))))

;; Special hack for output trace management - phase out.
;; The result is always going to be mutated, so should be a cell.

(defn lookup [tr adr]                  ; e[e]
  (if (= tr nil)
    nil
    (let [adr0 adr                                               ;REMOVE
          sub
          (if (seq? adr)
            ;; adr is a seq of keys
            (loop [tr tr adr adr]
              (if (empty? adr)
                tr
                (recur (make-locative tr (first adr))
                       (rest adr))))
            ;; adr is a key
            (make-locative tr adr))]
      (assert (mutable-trace? sub) ["lookup result" adr sub])
      sub)))

;; Returns a clojure seq of the numbered subtraces of the trace tr.
;; Used in: to-clojure and related

(defn subtraces-to-seq [tr]
  (for [i (range (trace-count tr))] (trace-subtrace tr i)))

;; ----------------------------------------------------------------------------
;; Trace constructors.
;; A motley bunch.  This can be cleaned up - we ought to be able to
;; significantly reduce the number of constructors, and make the
;; naming more regular.

;; The values in state-or-map should be subtraces (except for :value value).

(defn canonical-trace-state
  ([state-or-map]
   ;; TBD: check types of subtraces, at least, and maybe check ok-key? etc.
   (if (map? state-or-map)
     (state/map-to-state state-or-map)
     state-or-map))
  ([state-or-map val]
   (assert (ok-value? val))
   (let [state (canonical-trace-state state-or-map)]
     (state/set-value state val))))

(defn make-mutable-trace
  ([]
   (make-cell (state/empty-state)))
  ([state-or-map]
   (make-cell (canonical-trace-state state-or-map)))
  ([state-or-map val]
   (make-cell (canonical-trace-state state-or-map val))))

;; Creates a mutable trace from a map whose values are traces.
;; Same as make-mutable-trace, for now.

(defn trace-from-map
  ([maap] (make-mutable-trace maap))
  ([maap val] (make-mutable-trace maap val)))

;; mk_nil in the python version

(defn empty-trace
  ([] (make-mutable-trace (state/empty-state)))
  ([val] (make-mutable-trace (state/empty-state) val)))

(def new-trace empty-trace)

;; tlist is a seq of traces (either mutable or immutable)

(defn trace-from-subtrace-seq
  ([tlist]
   (make-mutable-trace tlist))
  ([tlist val]
   ;; zipmap returns a map
   (trace-from-map (zipmap (range (count tlist))
                           tlist)
                   val)))

;; Convert a mutable object to an immutable one, nonrecursively.

(defn to-immutable [x]
  (if (trace? x)
    (trace-state x)
    x))

;; Convert an immutable trace to a mutable trace, nonrecursively.
;; Not sure about this.

(defn to-mutable [x]
  (if (mutable-trace? x)
    x
    (if (trace? x)
      (make-cell (trace-state x))
      ;; somewhat DWIMmish.
      (make-cell (state/set-value (state/empty-state) x)))))

;; Recursive copy, mutable result... hmm... maybe should copy mutability as well?
;; see earthquake example...

(defn trace-copy [x]
  (if (trace? x)
    (let [keys (trace-keys x)
          result (into {} (for [key keys] [key (trace-copy (trace-get x key))]))]
      (if (trace-has-value? x)
        (make-mutable-trace (state/set-value result (trace-get x)))
        (make-mutable-trace result)))
    x))

(declare same-states? trace-set-value)

;; Marco's merge operator (+).  Commutative and idempotent.
;;
;; (trace-merge small large) - when calling, try to make tr1 smaller than tr2,
;; because it will be tr1 that is traversed.

(defn trace-merge
  ([tr1 tr2] (trace-merge tr1 tr2 trace-merge))
  ([tr1 tr2 submerge]
   (let [s1 (trace-state tr1)
         s2 (trace-state tr2)
         s (state/map-to-state
            (into (state/state-to-map s1)
                  (for [key (state/state-keys s2)]
                    [key (if (state/has-subtrace? s1 key)
                           (submerge (state/subtrace s1 key)
                                     (state/subtrace s2 key)
                                     submerge)
                           (state/subtrace s2 key))])))]
     (if (state/has-value? s)
       (do (if (state/has-value? s2)
             (assert (same-states? (state/value s) (state/value s2))
                     ["incompatible trace states" s s2]))
           s)
       (if (state/has-value? s2)
         (state/set-value s (state/value s2))
         s)))))

;; see also: freeze

;; -----------------------------------------------------------------------------
;; Effectless versions of operators that are ordinarily effectful

(defn ^:private trace-subtrace-maybe [tr key]
  (if (trace-has-direct-subtrace? tr key)
    (trace-direct-subtrace tr key)
    (state/empty-state)))

(defn trace-set-direct-subtrace [tr key sub]
  (assert (ok-key? key))
  (assert (trace? sub))
  (state/set-subtrace (trace-state tr) key sub))

(defn trace-set-subtrace [tr adr sub]
  (if (seq? adr)
    (if (empty? adr)
      sub
      (trace-set-direct-subtrace tr
                                 (first adr)
                                 (trace-set-subtrace (trace-subtrace-maybe tr (first adr))
                                                     (rest adr)
                                                     sub)))
    (trace-set-direct-subtrace tr adr sub)))

(defn trace-set-value [tr val]
  (state/set-value (trace-state tr) val))

(defn trace-set-value-at [tr adr val]
  (if (seq? adr)
    (if (empty? adr)
      (trace-set-value tr val)
      (trace-set-direct-subtrace tr
                                 (first adr)
                                 (trace-set-value-at (trace-subtrace-maybe tr (first adr))
                                                     (rest adr)
                                                     val)))
    (trace-set-direct-subtrace tr
                               adr
                               (trace-set-value (trace-subtrace-maybe tr adr) val))))

(defn trace-set
  ([tr val] (trace-set-value tr val))
  ([tr adr val] (trace-set-value-at tr adr val)))

(defn trace-delete
  ([tr] (state/clear-value (trace-state tr)))
  ([tr adr]
   (if (seq? adr)
     (if (empty? adr)
       (state/clear-value (trace-state tr))
       (trace-set-direct-subtrace tr
                                  (first adr)
                                  (trace-delete (trace-subtrace-maybe tr (first adr)) (rest adr))))
     (trace-set-direct-subtrace tr
                                adr
                                (trace-delete (trace-subtrace-maybe tr adr))))))

;; -----------------------------------------------------------------------------
;; Side effects.

(defn ^:private trace-set-direct-subtrace! [tr key sub]
  (assert (mutable-trace? sub) sub)
  (trace-swap! tr
               (fn [state]
                 (trace-set-direct-subtrace state key sub))))

(defn trace-set-subtrace! [tr adr sub]
  (assert (mutable-trace? sub) sub)
  (if (seq? adr)
    (loop [tr tr adr adr]
      (let [[head & tail] adr]
        (if (empty? tail)
          (trace-set-direct-subtrace! tr head sub)
          (let [more (if (trace-has-direct-subtrace? tr head)
                       (trace-direct-subtrace tr head)
                       (let [novo (make-mutable-trace)]
                         (trace-set-direct-subtrace! tr head novo)
                         novo))]
            (if (mutable-trace? more)
              (recur more tail)
              (trace-swap! tr (fn [state]
                                (trace-set-subtrace state adr val))))))))
    (trace-set-direct-subtrace! tr adr sub)))

(defn ^:private trace-set-value! [tr val]
  (assert (mutable-trace? tr) tr)
  (assert (ok-value? val) val)            ;REMOVE
  (trace-swap! tr
               (fn [state]
                 (state/set-value state val))))

(defn ^:private trace-set-value-at! [tr adr val] ;cf. trace-get
  (assert (mutable-trace? tr) tr)
  (let [adr (if (seq? adr) adr (list adr))]
    (loop [tr tr adr adr]
      (if (empty? adr)
        (trace-set-value! tr val)
        (let [[head & tail] adr]
          (let [more (if (trace-has-direct-subtrace? tr head)
                       (trace-direct-subtrace tr head)
                       (let [novo (make-mutable-trace)]
                         (trace-set-direct-subtrace! tr head novo)
                         novo))]
            (if (mutable-trace? more)
              (recur more tail)
              (trace-swap! tr (fn [state]
                                (trace-set-value-at state adr val))))))))))

(defn trace-set!
  ([tr val] (trace-set-value! tr val))
  ([tr adr val]
   (trace-set-value-at! tr adr val)))

(defn ^:private trace-clear! [tr]
  (trace-swap! tr (fn [state] (state/clear-value state))))

 (defn trace-delete!
   ([tr] (trace-clear! tr))
   ([tr adr]
    ;; (trace-swap! tr (fn [state] (trace-delete state adr)))
    (trace-clear! (trace-subtrace tr adr))))

;; Needs review

(defn trace-delete!-loser
  ([tr] (trace-clear! tr))
  ([tr adr]
   (let [adr (if (seq? adr) adr (list adr))]
     (if (empty? adr)
       (trace-clear! tr)
       (let [[head & tail] adr]
         ;; Keep following adr as long as the trace is mutable
         (if (trace-has-direct-subtrace? tr head)
           (if (or (immutable-trace? tr)
                   (mutable-trace? (trace-get tr head)))
             (trace-delete!-loser (trace-get tr head) tail)
             (trace-swap! tr (fn [state]
                               (trace-state (trace-delete state adr)))))
           ;; Did not delete, it wasn't there in the first place
           ))))))

(defn trace-set!
  ([tr val] (trace-set-value! tr val))
  ([tr adr val]
   (trace-set-value-at! tr adr val)))

(defn trace-merge!-maybe [mutable tr]
  (if (mutable-trace? mutable)
    (do (trace-swap! mutable
                     (fn [s1] 
                       (trace-merge s1 tr trace-merge!-maybe)))
        mutable)
    (trace-merge mutable tr trace-merge!-maybe)))

(defn trace-merge! [mutable tr]
  (assert (mutable-trace? mutable) mutable)
  (trace-merge!-maybe mutable tr))

;; -----------------------------------------------------------------------------
;; Thaw - convert partially immutable to fully immutable, by
;; introducing new cells as needed.

(defn trace-thaw! [tr]
  (assert (mutable-trace? tr))
  (trace-swap! tr
               (fn [state]
                 (state/map-to-state
                  (into {} (map (fn [[key sub]]
                                  (if (= key :value)
                                    [key sub]
                                    (let [new-sub (to-mutable sub)]
                                      (trace-thaw! new-sub)    ;Idempotent
                                      [key new-sub])))
                                (state/state-to-map state))))))
  tr)

;; -----------------------------------------------------------------------------
;; Zip and unzip are inverses, for valueless traces
;; The value is treated as a keyed entry with key :value (all the other entries are traces)

(defn trace-zip [tr]
  (seq (state/state-to-map tr)))

(defn trace-unzip [keyval-seq]
  (into {} (filter (fn [[key sub]]
                     (or (= key :value)
                         (if (= sub nil)
                           false
                           (do (assert (trace? sub))
                               true))))
                   keyval-seq)))

;; -----------------------------------------------------------------------------
;; User-friendly trace construction feature
;; inspired by python-metaprob

;; (trace :value 1, "z" 2, "a" (** subtrace), "c" (** (trace "d" 8)))

(defn ^:private splice? [x]
  (and (map? x) (contains? x :subtrace)))

(defn kv-pairs-to-map [kvps]
  (if (empty? kvps)
    (state/empty-state)
    (do (assert (not (empty? (rest kvps))) "odd number of args to trace")
        (let [key (first kvps)
              val (first (rest kvps))
              more (kv-pairs-to-map (rest (rest kvps)))]
          (if (= key :value)
            (do (assert (ok-value? val))
                (trace-set-value more val))
            (do (assert (ok-key? key))
                (trace-set-direct-subtrace
                 more
                 key
                 (if (splice? val)
                   (get val :subtrace)
                   (do (assert (ok-value? val))
                       (trace-set-value (state/empty-state) val))))))))))

(defn ** [tr]
  (assert (trace? tr) "**: expected a trace")
  {:subtrace tr})

(defn trace [& key-value-pairs]
  (state/map-to-state
   (kv-pairs-to-map key-value-pairs)))

(def immutable-trace trace)    ;for completeness

(defn mutable-trace [& key-value-pairs]
  (to-mutable (state/map-to-state
                 (kv-pairs-to-map key-value-pairs))))

;; -----------------------------------------------------------------------------
;; Lexicographic ordering on traces.  Used by prettyprinter.

(declare compare-keys)

(defn compare-key-lists [x-keys y-keys]
  (if (empty? x-keys)
    (if (empty? y-keys) 0 -1)
    (if (empty? y-keys)
      1
      (let [q (compare-keys (first x-keys) (first y-keys))]
        (if (= q 0)
          (compare-key-lists (rest x-keys) (rest y-keys))
          q)))))

(defn compare-traces [x y]
  (let [w (if (trace-has? x)
            (if (trace-has? y)
              (compare-keys (trace-get x) (trace-get y))
              -1)
            (if (trace-has? y)
              -1
              0))]
    (if (= w 0)
      (letfn [(lup [x-keys y-keys]
                (if (empty? x-keys)
                  (if (empty? y-keys)
                    0
                    -1)
                  (if (empty? y-keys)
                    1
                    (let [j (compare-keys (first x-keys) (first y-keys))]
                      (if (= j 0)
                        (let [q (compare-keys (trace-get x (first x-keys))
                                              (trace-get y (first y-keys)))]
                          (if (= q 0)
                            (lup (rest x-keys) (rest y-keys))
                            q))
                        j)))))]
        (lup (sort compare-keys (trace-keys x))
             (sort compare-keys (trace-keys y))))
      w)))

(defn compare-keys [x y]
  (cond (number? x)
        ;; Numbers come before everything else
        (if (number? y) (compare x y) -1)
        (number? y) 1

        (string? x)
        (if (string? y) (compare x y) -1)
        (string? y) 1

        (boolean? x)
        (if (boolean? y) (compare x y) -1)
        (boolean? y) 1

        (trace? x)
        (if (trace? y) (compare-traces x y) -1)
        (trace? y) 1

        true (compare x y)))

;; -----------------------------------------------------------------------------

(declare same-states?)

;; Compare states of two traces.

(defn same-trace-states? [trace1 trace2]
  (let [trace1 (trace-state trace1)
        trace2 (trace-state trace2)]
    (or (identical? trace1 trace2)
        (and (let [h1 (trace-has? trace1)
                   h2 (trace-has? trace2)]
               (and (= h1 h2)
                    (or (not h1)
                        (same-states? (trace-get trace1) (trace-get trace2)))))
             (let [keys1 (set (trace-keys trace1))
                   keys2 (set (trace-keys trace2))]
               (and (= keys1 keys2)
                    (every? (fn [key]
                              (same-trace-states? (trace-subtrace trace1 key)
                                                  (trace-subtrace trace2 key)))
                            keys1)))))))

;; Compare states of two values that might or might not be traces.

(defn same-states? [value1 value2]
  (or (identical? value1 value2)
      (if (trace? value1)
        (if (trace? value2)
          (same-trace-states? value1 value2)
          false)
        (if (trace? value2)
          false
          (= value1 value2)))))

;; Repeat rest-marker here in order to avoid importing the state module

(def rest-marker state/rest-marker)

;; -----------------------------------------------------------------------------
;; Prettyprint

(defn metaprob-newline
  ([] (newline))
  ([out] (.write out "\n")))

(declare pprint-indented)

(defn  ^:private princ [x out]
  (.write out (if (string? x) x (format "%s" x))))

;; Print a key or a value, on one line

(defn pprint-value [x out]
  (if (mutable-trace? x)
    (let [keyseq (trace-keys x)]
      (if (trace-has? x)
        (if (empty? keyseq)
          (princ (format "{:value %s}" (trace-get x))
                 out)    ;should pprint-value
          (princ (format "{:value %s, %s: ...}}" (trace-get x) (first keyseq))
                 out))
        (if (empty? keyseq)
          (princ "{}" out)
          (princ (format "{%s: ...}" (first keyseq)) out))))
    (pr x)))

;; x is a seq

(defn pprint-seq [x indent open close out]
  (princ open out)
  (let [vertical? (some trace? x)
        indent (str indent " ")]
    (loop [x x first? true]
      (if (not (empty? x))
        (do (if (not first?)
              (if vertical?
                (do (metaprob-newline out)
                    (princ indent out))
                (princ " " out)))
            (pprint-indented (first x) indent out)
            (recur (rest x) false)))))
  (princ close out))

;; Print {...} trace over multiple lines

(defn pprint-general-trace [tr indent out]
  (let [keys (trace-keys tr)]
    ;; If it has a value, clojure-print the value
    (if (trace-has? tr)
      (pprint-value (trace-get tr) out)
      ;; If no value and no subtraces, print as {} (shouldn't happen)
      (if (empty? keys) (princ "{}" out)))

    ;; Now print the subtraces
    (let [indent (str indent "  ")]
      (doseq [key (sort compare-keys keys)]
        (metaprob-newline out)
        (princ indent out)
        (if (string? key)
          (princ key out)
          (pprint-value key out))
        (princ ": " out)
        (pprint-indented (trace-subtrace tr key) indent out)))))

;; Indent gives indentation to use on lines after the first.

(defn pprint-indented [x indent out]
  (if (trace? x)
    (do (if (proper-function? x) (princ "COMPILED " out))
        (if (mutable-trace? x) (princ "!" out))
        (let [state (trace-state x)]
          (cond (empty? state)
                (princ "{}" out)

                (seq? state)
                (pprint-seq state indent "(" ")" out)

                (vector? state)
                (pprint-seq (seq state) indent "[" "]" out)

                true
                (pprint-general-trace state indent out))))
    (pprint-value x out))
  (.flush out))

;!!
(defn metaprob-pprint
  ([x] (metaprob-pprint x *out*))
  ([x out]
   (pprint-indented x "" out)
   (metaprob-newline out)
   (.flush out)))


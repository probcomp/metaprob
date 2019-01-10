(ns metaprob.sequence
  "Metaprob sequences (lists and tuples). These are distinct from Clojure seq."
  (:require [clojure.set :as set]
            [metaprob.state :as state]
            [metaprob.trace :as trace]))

;; 1. Metaprob pairs / lists

(defn empty-list [] '())

(defn pair-as-map? [state]
  (and (map? state)
       (= (count state) 2)
       (contains? state :value)
       (contains? state state/rest-marker)))

(defn metaprob-pair? [x]
  (and (trace/trace? x)
       (let [state (trace/trace-state x)]
         (if (seq? state)
           (not (empty? state))
           (pair-as-map? state)))))

(defn metaprob-first [mp-list]
  (assert (metaprob-pair? mp-list) ["first" mp-list])
  (trace/trace-get mp-list))

(defn metaprob-rest [mp-list]
  (assert (metaprob-pair? mp-list) ["rest" mp-list])
  (trace/trace-subtrace mp-list state/rest-marker))

(defn pair [thing mp-list]              ; NEEDS ERROR CHECKING
  (assert (trace/ok-value? thing) ["wta" thing])
  (assert (or (trace/empty-trace? mp-list)
              (metaprob-pair? mp-list))
          ["wanted empty or pair" mp-list])
  (if (seq? mp-list)
    (cons thing mp-list)                ; Keep it immutable
    (trace/make-mutable-trace {:value thing state/rest-marker mp-list})))

;; 2. Metaprob tuples (implemented as Clojure vectors)

(defn tuple [& inputs]
  (vec (map (fn [val]
              (assert (trace/ok-value? val))
              val)
            inputs)))

(defn tuple? [x]
  (and (trace/trace? x)
       (let [state (trace/trace-state x)]
         ;; [] can end up getting represented as () or {}
         (or (vector? state) (state/empty-state? state)))))

(defn sequence-to-seq
  "Convert metaprob sequence (list or tuple) to clojure seq."
  [things]
  (let [state (trace/trace-state things)]
    (cond (seq? state) state
          (vector? state) (seq state)
          (map? state) (if (pair-as-map? state)
                         (cons (get state :value)
                               (sequence-to-seq (get state state/rest-marker)))
                         (if (state/empty-state? state)
                           '()
                           (assert false ["not a sequence" state])))
          true (assert false ["sequence-to-seq wta" things state]))))

(defn length
  "Length of list or tuple"
  [tr]
  (let [state (trace/trace-state tr)]
    (cond (seq? state) (count state)
          (vector? state) (count state)
          (map? state) (if (pair-as-map? state)
                         (+ 1 (length (get state state/rest-marker)))
                         (if (state/empty-state? state)
                           0
                           (assert false ["not a sequence" state])))
          true (assert false ["length wta" tr state]))))


;; ----------------------------------------------------------------------
;; Sequences

(declare metaprob-nth append)

(defn seq-to-mutable-tuple
  "Convert clojure seq to metaprob tuple."
  [things]
  (trace/trace-from-subtrace-seq (map trace/new-trace things)))

(defn seq-to-mutable-list
  "Convert clojure sequence to metaprob list."
  [things]
  (let [things (seq things)]
    (if (empty? things)
      (trace/empty-trace)
      (pair (first things)
            (seq-to-mutable-list (rest things))))))

;; It is in principle possible to create traces that look like lists
;; but aren't (i.e. terminate in a nonempty, non-pair value).
;; The `pair` function rules this out on creation, but a list tail
;; can be clobbered by side effect to be a non-list.
;; Let's ignore this possibility.

(defn metaprob-list? [x]
  (or (trace/empty-trace? x)
      (metaprob-pair? x)))

(defn to-tuple
  "Convert metaprob list to metaprob tuple"
  [x]
  (if (tuple? x)
    x
    (if (metaprob-list? x)
      (vec (sequence-to-seq x))
      (assert false ["Expected a list or tuple" x]))))

(defn metaprob-list
  "builtin. Returns a list-qua-trace."
  [& things]
  ;; (seq-to-mutable-list things)
  (if (= things nil)
    (empty-list)
    things))

(defn to-list
  "builtin. Convert metaprob sequence to metaprob seq / list."
  [x]
  (cond (metaprob-list? x)
        x
        (tuple? x)
        (letfn [(scan [i]
                  (if (trace/trace-has? x i)
                    ;; Cons always returns a clojure seq and seqs are
                    ;; interpreted as metaprob lists
                    (cons (trace/trace-get x i) (scan (+ i 1)))
                    '()))]
          (scan 0))
        ;; This is a kludge but it helps in dealing with [& foo]
        ;; (where if there are no args, foo is nil instead of ())
        (= x nil)
        '()
        true
        (assert false ["Expected a tuple or list" x])))

(defn metaprob-last
  "Overrides original prelude (performance + generalization)."
  [mp-list]
  (let [more (metaprob-rest mp-list)]
    (if (metaprob-pair? more)
      (metaprob-last more)
      (metaprob-first mp-list))))

 (defn metaprob-nth
  "Overrides original prelude (performance + generalization)."
  [thing i]
  (if (trace/mutable-trace? thing)
    (if (metaprob-pair? thing)
      (letfn [(re [l i]
                (if (metaprob-pair? l)
                  (if (= i 0)
                    (metaprob-first l)
                    (re (metaprob-rest l) (- i 1)))
                  (assert false [l i (length thing)])))]
        (re thing (int i)))
      (trace/trace-get thing i))
    (nth thing i)))

;; prelude has: reverse, propose1, iterate, replicate, repeat

(defn metaprob-range
  "Returns an immutable trace"
  [n]
  (range n))

(defn append
  "Overrides original prelude (for performance, generality). Currently
  only handles lists; could extend to tuples. Mutability is
  contagious."
  [x y]
  ;; Mutable or immutable?
  (if (or (trace/mutable-trace? x) (trace/mutable-trace? y))
    (if (trace/empty-trace? y)
      x
      (letfn [(re [x]
                (if (trace/empty-trace? x)
                  y
                  (if (metaprob-pair? x)
                    (pair (metaprob-first x)
                          (re (metaprob-rest x)))
                    (assert false "bad appender"))))]
        (re x)))
    ;; Neither is mutable; should be seqs.  concat always returns a seq.
    (concat x y)))

;; Random stuff

(defn set-difference
  "All the members of s1 that are *not* in s2. Translation of version
  found in builtin.py."
  [s1 s2]
  (seq-to-mutable-list
   (seq (set/difference (set (sequence-to-seq s1))
                        (set (sequence-to-seq s2))))))

(defn metaprob-sort [sq & more]
  (apply sort (sequence-to-seq sq) more))

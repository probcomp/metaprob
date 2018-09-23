
(ns metaprob.sequence
  (:require [metaprob.trace :refer :all])
  (:require [clojure.set :as set]))

;; ----------------------------------------------------------------------------
;; Metaprob sequences (lists and tuples) (not same as Clojure seq)
;;  (split off into its own file?)

;; 1. Metaprob pairs / lists

(defn empty-list [] '())

(defn pair-as-map? [state]
  (and (map? state)
       (= (count state) 2)
       (contains? state :value)
       (contains? state rest-marker)))

(defn metaprob-pair? [x]
  (and (trace? x)
       (let [state (trace-state x)]
         (if (seq? state)
           (not (empty? state))
           (pair-as-map? state)))))

(defn metaprob-first [mp-list]
  (assert (metaprob-pair? mp-list) ["first" mp-list])
  (trace-get mp-list))

(defn metaprob-rest [mp-list]
  (assert (metaprob-pair? mp-list) ["rest" mp-list])
  (trace-subtrace mp-list rest-marker))

(defn pair [thing mp-list]              ;NEEDS ERROR CHECKING
  (assert (ok-value? thing) ["wta" thing])
  (assert (or (empty-trace? mp-list)
              (metaprob-pair? mp-list))
          ["wanted empty or pair" mp-list])
  (if (seq? mp-list)
    (cons thing mp-list)                ;Keep it immutable
    (make-mutable-trace {:value thing rest-marker mp-list})))

;; 2. Metaprob tuples (implemented as Clojure vectors)

(defn tuple [& inputs]
  (vec (map (fn [val]
              (assert (ok-value? val))
              val)
            inputs)))

(defn tuple? [x]
  (and (trace? x)
       (let [state (trace-state x)]
         ;; [] always ends up getting represented as ()
         (or (empty? state) (vector? state)))))

;; sequence-to-seq - convert metaprob sequence (list or tuple) to clojure seq.

(defn sequence-to-seq [things]
  (let [state (trace-state things)]
    (cond (seq? state) state
          (vector? state) (seq state)
          (map? state) (if (pair-as-map? state)
                         (cons (get state :value)
                               (sequence-to-seq (get state rest-marker)))
                         (assert false ["not a sequence" state]))
          true (assert false ["sequence-to-seq wta" things state]))))

;; Length of list or tuple

(defn length [tr]
  (let [state (trace-state tr)]
    (cond (seq? state) (count state)
          (vector? state) (count state)
          (map? state) (if (pair-as-map? state)
                         (+ 1 (length (get state rest-marker)))
                         (assert false ["not a sequence" state]))
          true (assert false ["length wta" tr state]))))


;; ----------------------------------------------------------------------
;; Sequences

(declare metaprob-nth append)

;; Convert clojure seq to metaprob tuple
;; Private except for tests

(defn seq-to-mutable-tuple [things]
  (trace-from-subtrace-seq (map new-trace things)))

;; seq-to-mutable-list - convert clojure sequence to metaprob list.
;; used by: distributions.clj

(defn seq-to-mutable-list [things]
  (let [things (seq things)]
    (if (empty? things)
      (empty-trace)
      (pair (first things)
            (seq-to-mutable-list (rest things))))))

;; It is in principle possible to create traces that look like lists
;; but aren't (i.e. terminate in a nonempty, non-pair value).  
;; The `pair` function rules this out on creation, but a list tail
;; can be clobbered by side effect to be a non-list.
;; Let's ignore this possibility.

(defn metaprob-list? [x]
  (or (empty-trace? x)
      (metaprob-pair? x)))

;; list-to-tuple - convert metaprob list to metaprob tuple
;; used in: prelude.clj

(defn to-tuple [x]
  (if (tuple? x)
    x
    (if (metaprob-list? x)
      (vec (sequence-to-seq x))
      (assert false ["Expected a list or tuple" x]))))

;; list - builtin

(defn metaprob-list [& things]
  ;; (seq-to-mutable-list things)
  (if (nil? things)
    '()
    things))

;; to-list - builtin - convert metaprob sequence to metaprob list

(defn to-list [x]
  (cond (metaprob-list? x)
        x
        (tuple? x)
        (letfn [(scan [i]
                  (if (trace-has? x i)
                    ;; Cons always returns a clojure seq
                    ;;  and seqs are interpreted as metaprob lists
                    (cons (trace-get x i) (scan (+ i 1)))
                    '()))]
          (scan 0))
        ;; This is a kludge but it helps in dealing with [& foo]
        ;;  (where if there are no args, foo is nil instead of ())
        (nil? x)
        '()
        true
        (assert false ["Expected a tuple or list" x])))

;; drop - use prelude version?

;; last - overrides original prelude (performance + generalization)

(defn metaprob-last [mp-list]
  (let [more (metaprob-rest mp-list)]
    (if (metaprob-pair? more)
      (metaprob-last more)
      (metaprob-first mp-list))))

;; nth - overrides original prelude (performance + generalization)

(defn metaprob-nth [thing i]
  (if (mutable-trace? thing)
    (if (metaprob-pair? thing)
      (letfn [(re [l i]
                (if (metaprob-pair? l)
                  (if (= i 0)
                    (metaprob-first l)
                    (re (metaprob-rest l) (- i 1)))
                  (assert false [l i (length thing)])))]
        (re thing (int i)))
      (trace-get thing i))
    (nth thing i)))

;; prelude has: reverse, propose1, iterate, replicate, repeat

;; range - now returns an immutable trace

(defn metaprob-range [n]
  (range n))                            ;clojure range should work

;; append - overrides original prelude (for performance, generality)
;;; Currently only handles lists; could extend to tuples.
;; Mutability is contagious.

(defn append [x y]
  ;; Mutable or immutable?
  (if (or (mutable-trace? x) (mutable-trace? y))
    (if (empty-trace? y)
      x
      (letfn [(re [x]
                (if (empty-trace? x)
                  y
                  (if (metaprob-pair? x)
                    (pair (metaprob-first x)
                          (re (metaprob-rest x)))
                    (assert false "bad appender"))))]
        (re x)))
    ;; Neither is mutable; should be seqs.  concat always returns a seq.
    (concat x y)))

;; Random stuff

;; All the members of s1 that are *not* in s2
;; Translation of version found in builtin.py.

(defn set-difference [s1 s2]
  (seq-to-mutable-list
   (seq (set/difference (set (sequence-to-seq s1))
                        (set (sequence-to-seq s2))))))

(defn metaprob-sort [sq & more]
  (apply sort (sequence-to-seq sq) more))


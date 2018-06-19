;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin-impl
  (:require [metaprob.trace :refer :all])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(declare metaprob-nth metaprob-pprint append)

;; ----------------------------------------------------------------------
;; Sequences

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
  (if (metaprob-tuple? x)
    x
    (if (metaprob-list? x)
      (vec (metaprob-sequence-to-seq x))
      (assert false ["Expected a list or tuple" x]))))

;; list - builtin

(defn metaprob-list [& things]
  ;; (seq-to-mutable-list things)
  (if (= things nil)
    '()
    things))

;; to-list - builtin - convert metaprob sequence to metaprob list

(defn to-list [x]
  (cond (metaprob-list? x)
        x
        (metaprob-tuple? x)
        (letfn [(scan [i]
                  (if (trace-has? x i)
                    ;; Cons always returns a clojure seq
                    ;;  and seqs are interpreted as metaprob lists
                    (cons (trace-get x i) (scan (+ i 1)))
                    '()))]
          (scan 0))
        ;; This is a kludge but it helps in dealing with [& foo]
        ;;  (where if there are no args, foo is nil instead of ())
        (= x nil)
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
   (seq (set/difference (set (metaprob-sequence-to-seq s1))
                        (set (metaprob-sequence-to-seq s2))))))

(defn metaprob-sort [sq & more]
  (apply sort (metaprob-sequence-to-seq sq) more))

;; -----------------------------------------------------------------------------
;; Addresses

;; addr - create an address out of a key sequence

(declare procedure-name)

(defn addr [& keys]
  (if (= keys nil)
    '()
    (map (fn [key]
           (if (procedure? key)
             (procedure-name key)
             (do (assert (ok-key? key))
                 key)))
         keys)))

;; Translation of .sites method from trace.py.
;; Returns a seq of addresses, I believe.  (and addresses are themselves seqs.)

(defn addresses-of [tr]
  (letfn [(get-sites [tr]
            ;; returns a seq of traces
            (let [site-list
                  (mapcat (fn [key]
                            (map (fn [site]
                                   (cons key site))
                                 (get-sites (trace-subtrace tr key))))
                          (trace-keys tr))]
              (if (trace-has? tr)
                (cons '() site-list)
                site-list)))]
    (let [s (get-sites tr)]
      (doseq [site s]
        (assert (trace-has? tr site) ["missing value at" site]))
      s)))

;; -----------------------------------------------------------------------------
;; Control

;; In metaprob, these are strict functions.

(defn neq [x y] (not (= x y)))

;; Procedure-related

;; Invoke a "foreign" procedure.  Called from interpreter.

(defn generate-foreign [ifn inputs]
  (apply ifn (metaprob-sequence-to-seq inputs)))

(defn make-foreign-procedure [name ifn]
  ifn)

(defn foreign-procedure-name [ifn]
  (str ifn))

;; Like make-immutable, but recursive.
;; DEPRECATED, DO NOT USE.

(defn ^:private freeze [x]
  (if (trace? x)
    (let [x (trace-state x)]
      (cond (empty-trace? x)
            '()

            (metaprob-pair? x)
            (map freeze (metaprob-list-to-seq x)) ;cf. ok-value?

            (metaprob-tuple? x)
            (vec (map freeze (metaprob-tuple-to-seq x)))

            true
            (let [keys (trace-keys x)
                  result (into {} (for [key keys] [key (freeze (trace-subtrace x key))]))]
              (if (trace-has? x)
                (assoc result :value (freeze (trace-get x)))
                result))))
    x))

;; This is for computing a name for a procedure at its point of creation.
;; Careful, this loses if there's a cycle.  Don't include a lexical
;; environment in tr.

(defn trace-name
  ([proc-parse-tree]
   (str (hash (freeze proc-parse-tree))))
  ([proc-parse-tree name] 
   (if name
     (str name "-" (trace-name proc-parse-tree))
     (trace-name proc-parse-tree))))

;; Mainly for foreign procedures.

(defn procedure-name [pp]
  (if (trace? pp)
    (if (trace-has? pp "name")
      (trace-get pp "name")          ;Cached
      (str "?-" (trace-name pp)))    ;???
    (foreign-procedure-name pp)))    ;E.g. "clojure.core$str@1593f8c5"

;; !! REVIEW
;; This is a kludge, to use until there's a better solution.
;; Its purpose is to strip off all properties from the procedure, especially 
;; "generative_source", in order to prevent use of the interpreter.

(defn opaque [name ifn]
  (with-meta ifn nil))

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; What about sp = tracing_proposer_to_prob_prog in prelude (!!) - do
;; we need it, how to define, etc.?

;; original prelude has: proposer_of factor apply_with_address

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly 

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

(defn metaprob-assert [condition complaint & irritants]
  (binding [*out* *err*]
    (doseq [irritant irritants]
      (if (mutable-trace? irritant)
        (do (print "Irritant:")
            (metaprob-pprint irritant)))))
  (assert condition
          (if (empty? irritants)
            complaint
            (vec (cons complaint irritants)))))

;; error - overrides original prelude (???)

(defn error [& irritants]
  (assert false irritants))                     ;from prelude.vnts

;; ----------------------------------------------------------------------------
;; Metaprob top level environments are represented as clojure namespaces.

(defn make-top-level-env [ns]
  (let [ns (if (symbol? ns)
             (find-ns ns)
             ns)]
    (assert (top-level-environment? ns))
    ns))

;; TBD: extend this to allow namespace-prefixed variable references foo/bar

(defn top-level-lookup [the-ns name]
  (let [v (ns-resolve the-ns (symbol name))]
    (assert (var? v) ["unbound variable" name the-ns])
    (assert (not (get (meta v) :macro)) ["reference to macro" name the-ns])
    (deref v)))

(defn top-level-bind! [the-ns name value]
  ;; how to create a new binding in a namespace (a la def)???
  (let [sym (symbol name)
        r (ns-resolve the-ns sym)
        r (if r r (binding [*ns* the-ns]
                    (print (format "Assigning %s in %s" sym the-ns))
                    (eval `(def ~sym))
                    (ns-resolve the-ns sym)))]
    (ref-set r value)
    nil))

;; ----------------------------------------------------------------------------
;; Mathematical

(defn exp [x] (java.lang.Math/exp x))
(defn sqrt [x] (java.lang.Math/sqrt x))

(defn add
  ([x y]
   (if (number? x)
     (+ x y)
     (if (and (string? x) (string? y))
       (str x y)
       (let [x (if (string? x) (list x) x)
             y (if (string? y) (list y) y)]
         (if (and (trace? x) (trace? y))
           (append x y)
           (assert false ["not addable" x y]))))))
  ([] 0)
  ([x] x)
  ;; this is rather foolish
  ([x y z & w] (add (add x y) (apply add z w))))

(defn log [x] (java.lang.Math/log x))
(defn cos [x] (java.lang.Math/cos x))
(defn sin [x] (java.lang.Math/sin x))
(defn log1p [x] (java.lang.Math/log1p x))
(defn floor [x] (java.lang.Math/floor x))
(defn round [x] (java.lang.Math/round x))

(def pi*2 (* 2 (java.lang.Math/acos -1)))
(defn normal [mu variance]              ;not needed any more?
  (fn [x]
    (let [x-mu (- x mu)]
      (/ (exp (- 0 (/ (* x-mu x-mu) (* 2.0 variance))))
         (sqrt (* pi*2 variance))))))

(def ^:dynamic *rng* (java.util.Random. 42))

(defn sample-uniform
  ([] (.nextDouble *rng*))
  ([a b] (+ a (* (.nextDouble *rng*) (- b a)))))
  

;; -----------------------------------------------------------------------------
;; Graphical output (via gnuplot or whatever)

(defn binned-histogram [& {:keys [name samples overlay-densities
                                  sample-lower-bound sample-upper-bound
                                  number-of-intervals]}]
  (let [samples (metaprob-sequence-to-seq samples)
        sample-lower-bound (or sample-lower-bound -5)
        sample-upper-bound (or sample-upper-bound 5)
        number-of-intervals (or number-of-intervals 20)
        fname (clojure.string/replace name " " "_")
        path (str "results/" fname ".samples")
        commands-path (str path ".commands")]
    (print (format "Writing commands to %s for histogram generation\n" commands-path))
    ;;(print (format " overlay-densities = %s\n" (freeze overlay-densities)))
    (with-open [writor (io/writer commands-path)]
      (.write writor (format "reset\n"))
      (.write writor (format "min=%s.\n" sample-lower-bound))
      (.write writor (format "max=%s.\n" sample-upper-bound))
      (.write writor (format "n=%s\n" number-of-intervals))
      (.close writor))
    (print (format "Writing samples to %s\n" path))
    (with-open [writor (io/writer path)]
      (doseq [sample samples]
        (.write writor (str sample))
        (.write writor "\n"))
      (.close writor))))

;; -----------------------------------------------------------------------------
;; Prettyprint

(declare pprint-indented)

(defn  ^:private princ [x] (print x))

(defn pprint-atom [a]
  (if (mutable-trace? a)
    (let [x a
          keyseq (trace-keys x)]
      (if (trace-has? x)
        (if (empty? keyseq)
          (princ (format "{:value %s}" (trace-get x)))    ;should pprint-atom
          (princ (format "{:value %s, %s: ...}}" (trace-get x) (first keyseq))))
        (if (empty? keyseq)
          (princ "{}")
          (princ (format "{%s: ...}" (first keyseq))))))
    (pr a)))

;; x is a seq

(defn pprint-seq [x indent open close]
  (assert (seq? x))
  (princ open)
  (let [vertical? (some mutable-trace? x)
        indent (str indent " ")]
    (letfn [(lup [x first?]
              (if (not (empty? x))
                (do (if (not first?)
                      (if vertical?
                        (do (newline)
                            (princ indent))
                        (princ " ")))
                    (pprint-indented (first x) indent)
                    (lup (rest x) false))))]
      (lup x true)))
  (princ close))

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

(defn pprint-trace [tr indent]
  (letfn [(re [tr indent tag]
            (princ indent)
            (if (mutable-trace? tr) (princ "!"))
            (if (string? tag)
              (princ tag)
              (pprint-atom tag))
            (if (or (trace-has? tr)
                    (not (empty? (trace-keys tr))))
              (princ ": "))
            ;; If it has a value, clojure-print the value
            (if (trace-has? tr)
              (pprint-atom (trace-get tr)))
            (newline)
            (let [indent (str indent "  ")]
              (doseq [key (sort compare-keys (trace-keys tr))]
                (re (trace-subtrace tr key) indent key))))]
    (re tr indent "trace")))

(defn pprint-indented [x indent]
  (cond (empty-trace? x)
        (pprint-atom x)
        
        (seq? x)
        (pprint-seq x indent "(" ")")

        (vector? x)
        (pprint-seq (seq x) indent "[" "]")

        (map? x)
        (pprint-trace x indent)

        (metaprob-pair? x)
        (pprint-seq (metaprob-list-to-seq x) indent "!(" ")")

        (metaprob-tuple? x)
        (pprint-seq (metaprob-tuple-to-seq x) indent "![" "]")

        (mutable-trace? x)
        (pprint-trace x indent)

        true
        (pprint-atom x))
  (flush))

;!!
(defn metaprob-pprint [x]
  (pprint-indented x "")
  (newline)
  (flush))

;; Maybe this should print (i.e. princ) instead of pr (i.e. prin1)?

;!!
(defn metaprob-print [x]
  (princ x)
  (newline)
  (flush))

(declare same-states?)

;; Compare states of two traces.

(defn same-trace-states? [trace1 trace2]
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
                          keys1))))))

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


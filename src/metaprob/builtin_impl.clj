;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin-impl
  (:require [metaprob.trace :refer :all])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(declare metaprob-nth metaprob-pprint)

;; --------------------

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

;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

(declare append)

;; addr - like `list`, but pure

(defn addr [& things] things)    ;; a seq

;; Translation of .sites method from trace.py.
;; Returns a seq of addresses, I believe.  (and addresses are themselves seqs.)

(defn addresses-of [trace]
  (letfn [(sites [tr]
            ;; returns a seq of traces
            (let [site-list
                  (mapcat (fn [key]
                            (map (fn [site]
                                   (cons key site))
                                 (sites (lookup tr key))))
                          (trace-keys tr))]
              (if (trace-has? tr)
                (cons '() site-list)
                site-list)))]
    (let [s (sites trace)]
      (doseq [site s]
        (assert (trace-has? trace site) ["missing value at" site]))
      s)))

;; It is in principle possible to create traces that look like lists
;; but aren't (i.e. terminate in a nonempty, non-pair value).  
;; The `pair` function rules this out, but with effort it's possible
;; to create such a beast.  Let's ignore this possibility.

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

;!! used ?
(defn metaprob-list [& things]
  (seq-to-mutable-list things))

;; to-list - builtin - convert metaprob tuple to metaprob list

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

(defn ^:private mp-list-last [mp-list]
  (if (metaprob-pair? mp-list)
    (let [more (metaprob-rest mp-list)]
      (if (not (metaprob-pair? more))
        (metaprob-first mp-list)
        (mp-list-last more)))
    mp-list))

;!!
(defn metaprob-last [mp-list]
  (if (metaprob-pair? mp-list)
    (mp-list-last mp-list)
    (assert false (diagnose-nonpair mp-list))))

;; nth - overrides original prelude (performance + generalization)

(defn metaprob-nth [thing i]
  (if (mutable-trace? thing)
    (if (metaprob-pair? thing)
      (letfn [(re [l i]
                (if (metaprob-pair? l)
                  (if (= i 0)
                    (metaprob-first l)
                    (re (metaprob-rest l) (- i 1)))
                  (assert false [(diagnose-nonpair l) i (length thing)])))]
        (re thing (int i)))
      (trace-get thing i))
    (nth thing i)))

;; prelude has: reverse, propose1, iterate, replicate, repeat

;; range - overrides original prelude (performance + generalization)

(defn _range [n k]
  (if (>= k n) (empty-trace) (pair k (_range n (+ k 1)))))

(defn metaprob-range [n]
  (_range n 0))

;; append - overrides original prelude (performance)
;; This is only for metaprob lists, not for tuples.

(defn append [x y]
  (if (metaprob-pair? x)
    (pair (metaprob-first x) (append (metaprob-rest x) y))
    (do (assert (or (empty-trace? y)
                    (metaprob-pair? y))
                ["expected append 2nd arg to be a mp list" y])
        y)))

;; Random stuff

;; All the members of s1 that are *not* in s2
;; Translation of version found in builtin.py.

(defn set-difference [s1 s2]
  (seq-to-mutable-list
   (seq (set/difference (set (map purify
                                  (metaprob-sequence-to-seq s1)))
                        (set (map purify
                                  (metaprob-sequence-to-seq s2)))))))

;; -----------------------------------------------------------------------------
;; Control

;; In metaprob, these are strict functions.

;!!
(defn metaprob-and [a b]
  (and a b))

;!!
(defn metaprob-or [a b]
  (or a b))

(defn neq [x y] (not (= x y)))

(defn exactly [& body]
  (assert false "what is exactly, exactly?"))

;; Procedure-related

;; Invoke a "foreign" procedure.  Called from interpreter.

(defn generate-foreign [ifn inputs]
  (apply ifn (metaprob-sequence-to-seq inputs)))

(defn make-foreign-procedure [name ifn]
  (if true
    ifn
    (do (assert (or (string? name) (integer? name) (= name nil)) name)
        (assert (instance? clojure.lang.IFn ifn) ifn)
        (trace-as-procedure (trace-from-map {"name" (new-trace name)
                                             "foreign-generate-method" ifn}
                                            "prob prog")
                            ifn))))

;; !! REVIEW
;; This is a kludge, see syntax.clj, to use until there's a better solution.
;; Its purpose is to strip off all properties from the procedure, especially "source".

(defn export-procedure [ifn]
  (make-foreign-procedure (trace-get ifn "name") (with-meta ifn nil)))

(defn procedure-name [pp]
  (if (trace? pp)
    (if (trace-has? pp "name")
      (trace-get pp "name")
      (str pp))
    (str pp)))    ; E.g. "clojure.core$str@1593f8c5"

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

;; -----------------------------------------------------------------------------
;; The tag address business

;; capture-tag-address - overrides original prelude - but definition is the same.
;; Overrides definition in prelude.clj.

(defn capture-tag-address [i t o]
  (assert (and (or (= i nil) (trace? i))
               (or (= t nil) (trace? t))
               (or (= o nil) (trace? o))))
  (trace-from-map {"intervention" (new-trace i)
                   "target" (new-trace t)
                   "output" (new-trace o)}
                  "captured tag address"))

;; resolve-tag-address - original is in builtin.py

(defn resolve-tag-address [quasi_addr]
  (let [captured (metaprob-first quasi_addr)
        addr (addrify (metaprob-rest quasi_addr))]
    (assert (trace? captured))
    (assert (= (count (trace-keys captured)) 3))
    (let [i (trace-get captured "intervention")
          t (trace-get captured "target")
          o (trace-get captured "output")]
      (let [i2 (if i (lookup i addr) nil)
            t2 (if t (lookup t addr) nil)
            o2 (if o (lookup o addr) nil)]
        (assert (and (or (trace? i2) (= i2 nil))
                     (or (trace? t2) (= t2 nil))
                     (or (trace? o2) (= o2 nil))))
        (seq-to-mutable-tuple [i2 t2 o2])))))

;; ----------------------------------------------------------------------------
;; Metaprob top level environments are represented as clojure namespaces.

(defn make-top-level-env [ns]
  (let [ns (if (symbol? ns)
             (find-ns ns)
             ns)]
    (assert (top-level-environment? ns))
    ns))

(defn top-level-lookup [the-ns name]
  (let [v (ns-resolve the-ns (symbol name))]
    (assert (var? v) ["not bound" name the-ns])
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

;; Purity is contagious.

(defn add [x y]
  (if (number? x)
    (+ x y)
    (if (and (trace? x) (trace? y))
      (append x y)
      (if (and (string? x) (string? y))
        (concat x y)
        (let [to-seq (fn [x]
                       (if (string? x)
                         (list x) ;????
                         (metaprob-sequence-to-seq x)))]
          (concat (to-seq x) (to-seq y)))))))

(defn log [x] (java.lang.Math/log x))
(defn cos [x] (java.lang.Math/cos x))
(defn sin [x] (java.lang.Math/sin x))
(defn log1p [x] (java.lang.Math/log1p x))
(defn floor [x] (java.lang.Math/floor x))

(def pi*2 (* 2 (java.lang.Math/acos -1)))
(defn normal [mu variance]              ;not needed any more?
  (fn [x]
    (let [x-mu (- x mu)]
      (/ (exp (- 0 (/ (* x-mu x-mu) (* 2.0 variance))))
         (sqrt (* pi*2 variance))))))

(def ^:dynamic *rng* (java.util.Random. 42))

(defn sample-uniform
  ([] (.nextLong *rng*))
  ([a b] (+ a (* (.nextLong *rng*) (- b a)))))
  

;; -----------------------------------------------------------------------------
;; Graphical output (via gnuplot or whatever)

(defn binned-histogram [& {:keys [name samples overlay-densities]}]
  (let [samples (metaprob-sequence-to-seq samples)
        path (clojure.string/replace name " " "_")]
    (print (format "Writing samples to %s for histogram generation\n" path))
    (print (format " overlay-densities = %s\n" (purify overlay-densities)))
    (with-open [writor (io/writer (str "results/" path ".samples"))]
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
          (princ (format "{{%s}}" (trace-get x)))    ;should pprint-atom
          (princ (format "{{%s, %s: ...}}" (trace-get x) (first keyseq))))
        (if (empty? keyseq)
          (princ "{{}}")
          (princ (format "{{%s: ...}}" (first keyseq))))))
    (pr a)))

(defn pprint-seq [x indent open close]
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

(defn pprint-trace [tr indent]
  (letfn [(re [tr indent tag]
            (princ indent)
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
              (doseq [key (trace-keys tr)]
                (re (lookup tr key) indent key))))]
    (re tr indent "trace")))

(defn pprint-indented [x indent]
  (cond (empty-trace? x)
        (princ "{{}}")
        
        (metaprob-pair? x)
        (pprint-seq (metaprob-list-to-seq x) indent "(" ")")

        (metaprob-tuple? x)
        (pprint-seq (metaprob-tuple-to-seq x) indent "[" "]")

        (mutable-trace? x)
        (pprint-trace x indent)

        (seq? x)
        (pprint-seq x indent "(" ")")

        (vector? x)
        (pprint-seq x indent "[" "]")

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
  (flush))


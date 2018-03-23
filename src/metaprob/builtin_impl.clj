;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin-impl
  (:require [metaprob.trace :refer :all])
  (:require [kixi.stats.math :as math])
  (:require [kixi.stats.distribution :as dist])  ;for beta
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

(declare metaprob-nth metaprob-pprint)

;; --------------------

;; Convert clojure seq to metaprob tuple

(defn ^:private seq-to-metaprob-tuple [things]
  (trace-from-subtrace-seq (map new-trace things)))

;; seq-to-metaprob-list - convert clojure sequence to metaprob list.
;; used by: distributions.clj

(defn seq-to-metaprob-list [things]
  (let [things (seq things)]
    (if (empty? things)
      (empty-trace)
      (pair (first things)
            (seq-to-metaprob-list (rest things))))))

;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

(declare append)

;; addr - like list, but pure

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
      (seq-to-metaprob-list s))))

;; list-to-tuple - convert metaprob list to metaprob tuple
;; used in: prelude.clj

(defn list-to-tuple [mp-list]
  (letfn [(r [mp-list n]
            (if (empty-trace? mp-list)
              {}
              (assoc (r (metaprob-rest mp-list) (+ n 1))
                     n
                     (new-trace (metaprob-first mp-list)))))]
    (trace-from-map (r mp-list 0))))

;; list - builtin

;!!
(defn metaprob-list [& things]
  (seq-to-metaprob-list things))

;; tuple-to-list - builtin - metaprob tuple to metaprob list

(defn tuple-to-list [tup]
  (if (mutable-trace? tup)
    (letfn [(scan [i]
              (if (trace-has? tup i)
                (pair (trace-get tup i) (scan (+ i 1)))
                (empty-trace)))]
      (scan 0))
    ;; seqable?
    (apply list tup)))

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
  (seq-to-metaprob-list
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

;; Probprog-related

(defn make-foreign-probprog [name ifn]
  (assert (or (string? name) (integer? name) (= name nil)) name)
  (assert (instance? clojure.lang.IFn ifn) ifn)
  (fn-qua-trace ifn
                (trace-from-map
                 {"name" (new-trace name)
                  "foreign-generate-method" (new-trace ifn)}
                 "prob prog")))

;; !! REVIEW
;; This is a kludge, see syntax.clj, to use until there's a better solution.
;; Its purpose is to strip off all properties from the probprog, especially "source".

(defn export-probprog [ifn]
  (make-foreign-probprog (trace-get ifn "name") (with-meta ifn nil)))

;; Invoke a foreign probprog.  Called from query-foreign in interpreter.

(defn generate-foreign [ifn argseq]
  (apply ifn (metaprob-sequence-to-seq argseq)))

(defn probprog-name [pp]
  (if (trace-has? pp "name")
    (trace-get pp "name")
    nil))

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
        (seq-to-metaprob-tuple [i2 t2 o2])))))

;; ----------------------------------------------------------------------------
;; Metaprob top level environments are represented as clojure namespaces.

(defn make-top-level-env [ns]
  (if (symbol? ns)
    (find-ns ns)
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

;; Lexical environments, needed by program macro.
;; TBD: Move to prelude

(defn frame? [obj]
  (and (trace? obj)
       (trace-has? obj)
       (= (trace-get obj) "frame")))

(defn frame-parent [frame]
  (trace-get frame "parent"))

;; env-lookup - overrides original prelude

(defn env-lookup [env name]
  (if (frame? env)
    (if (trace-has? env name)
      (trace-get env name)
      (env-lookup (frame-parent env) name))
    ;; Top level environment
    (top-level-lookup env name)))

;; make-env - overrides original prelude

(defn make-env [parent]
  (trace-from-map {"parent" (new-trace parent)}
                  "frame"))

(defn env-bind! [env name val]
  (if false
    (top-level-bind! env name val)
    (trace-set env name val)))

;; match-bind - overrides original prelude.
;; Liberal in what it accepts: the input can be either a list or a
;; tuple, at any level.

(defn match-bind [pattern input env]
  (letfn [(re [pattern input]
            ;; pattern is a trace (variable or list, I think, maybe seq)
            (assert mutable-trace? pattern)
            (case (trace-get pattern)
              "variable" (env-bind! env (trace-get pattern "name") input)
              "tuple"
              ;; input is either a metaprob list or a metaprob tuple
              (let [subpatterns (subtraces-to-seq pattern)
                    parts (metaprob-sequence-to-seq input)]
                (assert
                 (= (count subpatterns) (count parts))
                 ["number of subpatterns differs from number of input parts"
                  (count subpatterns) (count parts)])
                ;; Ugh. https://stackoverflow.com/questions/9121576/clojure-how-to-execute-a-function-on-elements-of-two-seqs-concurently
                (doseq [[p i] (map list subpatterns parts)]
                  (re p i)))))]
    (dosync (re pattern input))
    "return value of match-bind"))


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
                         (let [p (purify x)]
                           (assert (seqable? p) ["invalid argument for add" x y])
                           p)))]
          (concat (to-seq x) (to-seq y)))))))

(defn log [x] (math/log x))
(defn cos [x] (math/cos x))
(defn sin [x] (math/sin x))
(defn log-gamma [x] (math/log-gamma x))
(defn log1p [x] (java.lang.Math/log1p x))

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
            (pprint-atom tag)
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


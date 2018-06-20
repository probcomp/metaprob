;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin-impl
  (:require [metaprob.trace :refer :all])
  (:require [metaprob.sequence :refer :all])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

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
  (apply ifn (sequence-to-seq inputs)))

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
            (map freeze (sequence-to-seq x)) ;cf. ok-value?

            (metaprob-tuple? x)
            (vec (map freeze (sequence-to-seq x)))

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
;; !! TBD:  If ifn is not, in fact, a function, then it is an
;; interpreted procedure, and it needs to be wrapped so that
;; the traces and score are ignored.

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
  (assert (string? name) ["wanted a string" name])
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
  (let [samples (sequence-to-seq samples)
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

;; Maybe this should print (i.e. princ) instead of pr (i.e. prin1)?

;!!
(defn metaprob-print [x]
  (print x)
  (newline)
  (flush))

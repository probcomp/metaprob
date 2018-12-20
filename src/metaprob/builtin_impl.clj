;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin-impl
  (:refer-clojure :exclude [get contains? get-in assoc dissoc empty? keys])
  (:require [metaprob.trace :refer :all])
  (:require [metaprob.compound :refer :all :exclude [assoc dissoc]])
  (:require [clojure.java.io :as io])
  (:require [clojure.set :as set]))

;; -----------------------------------------------------------------------------
;; Addresses

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
              (if (trace-has-value? tr)
                (cons '() site-list)
                site-list)))]
    (let [s (get-sites tr)]
      (doseq [site s]
        (assert (trace-has-value? tr site) ["missing value at" site]))
      s)))

;; -----------------------------------------------------------------------------
;; Control

;; Procedure-related

;; Invoke a "foreign" procedure.  Called from interpreter.

(defn generate-foreign [ifn inputs]
  (assert (fn? ifn) ["not a foreign-procedure" ifn inputs])
  (apply ifn (seq inputs)))

(defn make-foreign-procedure [name ifn]
  (assert (fn? ifn) ["not procedure" name ifn])
  ifn)

;; -----------------------------------------------------------------------------
;; Misc


(defn assoc
  ([m k v] (metaprob.compound/assoc m k v))
  ([m k v & kvs]
   (let [ret (metaprob.compound/assoc m k v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (IllegalArgumentException. "assoc expects an even number of arguments after the collection; found odd number")))
       ret))))

(defn dissoc
  ([m k] (metaprob.compound/dissoc m k))
  ([m k & ks]
   (let [ret (metaprob.compound/dissoc m k)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

;; This is for computing a name for a procedure at its point of creation.
;; Careful, this loses if there's a cycle.  Don't include a lexical
;; environment in tr.

(defn trace-name
  ([proc-parse-tree]
   (str (hash proc-parse-tree)))
  ([proc-parse-tree name] 
   (if name
     (str name "-" (trace-name proc-parse-tree))
     (trace-name proc-parse-tree))))

(defn foreign-procedure-name [ifn]
  (str ifn))

;; Mainly for foreign procedures.

(defn procedure-name [pp]
  (if (compound? pp)
    (or (get pp :name) (str "?-" (trace-name pp)))
    (foreign-procedure-name pp)))

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms, (3) it doesn't evaluate the
;; 2nd argument unless the condition fails.

(defn metaprob-assert [condition complaint & irritants]
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
  (assert (top-level-environment? the-ns) ["wanted a top-level env" the-ns name])
  (let [v (ns-resolve the-ns name)]
    (assert (var? v) ["no such variable" the-ns name])
    (assert (not (get (meta v) :macro)) ["reference to macro" the-ns name])
    (assert (bound? v) ["unbound variable" the-ns name])
    (deref v)))

(defn top-level-bind! [the-ns name value]
  ;; how to create a new binding in a namespace (a la def)???
  (let [r (ns-resolve the-ns name)
        r (if r r (binding [*ns* the-ns]
                    (print (format "Assigning %s in %s" name the-ns))
                    (eval `(def ~name))
                    (ns-resolve the-ns name)))]
    (ref-set r value)
    nil))

;; ----------------------------------------------------------------------------
;; Mathematical

(defn exp [x] (java.lang.Math/exp x))
(defn expt [x y] (java.lang.Math/pow x y))
(defn sqrt [x] (java.lang.Math/sqrt x))

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

(defn set-difference [s1 s2]
  (seq (set/difference (set s1) (set s2))))
  

;; -----------------------------------------------------------------------------
;; Graphical output (via gnuplot or whatever)

(defn binned-histogram [& {:keys [name samples overlay-densities
                                  sample-lower-bound sample-upper-bound
                                  number-of-intervals]}]
  (let [samples (seq samples)
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

;; -----------------------------------------------------------------------------
;; Interpreter-related

;; This could go in prelude.clj, with some effort.


(defn inf [name model implementation]
  (assoc
    ;; When called from Clojure:
    (fn [& inputs]
      (let [inputs (if (= inputs nil) (list) inputs)]
        ;(if (fn? model)
        ;  (apply model inputs)
          (nth (implementation inputs {:intervene {} :interpretation-id (gensym) :target {}}) 0)))
    ;; Annotations:
    :name (str "inf-" name),
    :model model,
    :implementation implementation))

(defn clojure-interpreter [proc inputs ctx]
  (assert (fn? proc))
  [(clojure.core/apply proc inputs) {} 0])

(def ^:dynamic *ambient-interpreter* clojure-interpreter)

(defn infer-apply [proc inputs intervention-trace
                   target-trace output-trace?]
  (if (and (fn? proc)
           (empty? intervention-trace)
           (empty? target-trace)
           (not output-trace?))
    ;; Bypass inference when there is no need to use it.
    [(generate-foreign proc inputs) {} 0]

    (let [[value output score]
          (*ambient-interpreter*
            proc inputs
            {:intervene intervention-trace, :target target-trace,
             :interpretation-id (gensym), :active? true})]
      (assert (number? score) ["bad score"
                               score proc
                               *ambient-interpreter*])

      ;(assert (if output-trace?
      ;          (trace? output)
      ;          true)
      ;        ["bad output" output proc])
      [value output score])))
